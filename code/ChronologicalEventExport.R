# Code to produce the chronological summary of programming changes made during PSGs
# Originally created by D Bourn and cGPT, edited by Meg McEachran
# 7/8/25 last updated 

###############################################################################
# 0.  Libraries                                                               #
###############################################################################
library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)

rm(list=ls())
###############################################################################
# 1.  File location                                                           #
###############################################################################
default_dir <- here("data/")

file_path <- dlg_open(
  default = default_dir,
  title = "Select Device Log CSV File",
  #filters = matrix(c("CSV files", "*.csv"), ncol = 3)
)$res

#read in raw data... NOTE this will likely throw a warning, use problems(log_raw) to check but it should be fine 
log_raw <- read_csv(file_path) %>% clean_names()
#   col_types = cols(.default = "c")   # <- keep everything as character on read-in
# ) |> 
 # janitor::clean_names()             # tidier col-names: timestamp, event, details ...

###############################################################################
# 2.  Parameters we keep from LogProgramming                                  #
###############################################################################
keep_params <- c(
  "PhasicTime", "TherapyRate", "RiseTime", "FallTime",
  "PhasicAmplitude", "PulseWidth", "Frequency",
  "UprightPause", "RollPause", "TherapyDelay","RollPauseMode"
)

###############################################################################
# 3.  Helper to canonicalise units                                            #
###############################################################################
abbr_unit <- function(u_raw) {
  u <- tolower(u_raw)
  case_when(
    str_detect(u, "^µ?s(ec|econd)?s?$")               ~ "s",
    str_detect(u, "^m(in(ute)?s?)?$")                 ~ "m",
    str_detect(u, "^(µ|u)s$|^usec$|^microsec.*$")     ~ "µs",
    TRUE                                              ~ u_raw
  )
}

###############################################################################
# 4.  Read CSV & classify rows                                                #
###############################################################################
log_df <- log_raw %>%
  mutate(
    date       = mdy_hms(date),
    event_type = case_when(
      str_detect(event, regex("LogProgramming",            TRUE)) ~ "LogProgramming",
      str_detect(event, regex("LogChangeAmplitudeByOrder", TRUE)) ~ "LogChangeAmp",
      str_detect(event, regex("LogTherapyStart",           TRUE)) ~ "LogTherapyStart",
      str_detect(event, regex("LogTherapyEnd",             TRUE)) ~ "LogTherapyEnd",
      str_detect(event, regex("LogPositionChange",         TRUE)) ~ "LogPositionChange",
      str_detect(event, regex("LogProgAlgoLL",          TRUE)) ~ "LogProgAlgoLL",
      TRUE ~ "OTHER")
  ) %>%
  filter(event_type != "OTHER") %>%
  arrange(date)

###############################################################################
# 5.  Keep only the programming rows we want                                  #
###############################################################################
# -- locate therapy periods ---------------------------------------------------
therapy_pairs <- log_df %>%
  filter(event_type %in% c("LogTherapyStart", "LogTherapyEnd")) %>%
  arrange(date) %>%
  mutate(grp = cumsum(event_type == "LogTherapyStart")) %>%
  group_by(grp) %>%
  summarise(start = first(date[event_type == "LogTherapyStart"]),
            end   = first(date[event_type == "LogTherapyEnd" & date > start]),
            .groups = "drop") %>%
  mutate(end = if_else(is.na(end), max(log_df$date) + days(1), end))

# -- mark programming rows to keep -------------------------------------------
keep_prog_idx <- logical(nrow(log_df))

prog_dates <- log_df$date[log_df$event_type == "LogProgramming"]

for (i in seq_len(nrow(therapy_pairs))) {
  st <- therapy_pairs$start[i]
  en <- therapy_pairs$end[i]
  
  # a) programming rows inside [start, end]
  inside <- which(log_df$event_type == "LogProgramming" &
                    log_df$date >= st & log_df$date <= en)
  keep_prog_idx[inside] <- TRUE
  
  # b) programming row immediately preceding start
  prev_idx <- max(which(log_df$event_type == "LogProgramming" &
                          log_df$date < st))
  if (length(prev_idx) && prev_idx > 0) keep_prog_idx[prev_idx] <- TRUE
}

# discard unwanted programming rows
log_df <- log_df[ !(log_df$event_type == "LogProgramming" & !keep_prog_idx), ]

###############################################################################
# 6A.  Parse LogProgramming rows                                              #
###############################################################################
prog_long <- log_df %>%
  filter(event_type == "LogProgramming") %>%
  separate_rows(data, sep = " - ") %>%
  mutate(
    raw_param = str_trim(str_extract(data, "^.*?(?=\\s*=)")),
    param     = str_remove_all(raw_param, "\\s+"),
    value_raw = str_trim(str_extract(data, "(?<=\\[).*(?=\\])"))
  ) %>%
  filter(param %in% keep_params) %>%
  mutate(
    value_num = str_extract(value_raw, "[-+]?[0-9]*\\.?[0-9]+"),
    unit_raw  = str_trim(str_remove(value_raw, "[-+]?[0-9]*\\.?[0-9]+")),
    unit      = if_else(param == "PulseWidth", "µs", abbr_unit(unit_raw)),
    value     = value_num
  )

###############################################################################
# 6B.  Parse LogChangeAmplitudeByOrder rows                                   #
###############################################################################
amp_tbl <- log_df %>%
  filter(event_type == "LogChangeAmp") %>%
  separate_rows(data, sep = " - ") %>%
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")),
    val = str_trim(str_extract(data, "(?<=\\[).*(?=\\])"))
  ) %>%
  pivot_wider(id_cols = c(date, event_type),
              names_from  = key,
              values_from = val) %>%
  mutate(
    step_mA   = as.numeric(str_remove(AmplitudeStep, "\\s*mA$")),
    amp_delta = if_else(tolower(WasIncreased) == "yes",  step_mA, -step_mA)
  ) %>%
  # Add all param_cols as NA columns
  mutate(!!!setNames(rep(list(NA_character_), length(param_cols)), param_cols)) %>%
  select(date, event_type, all_of(param_cols), amp_delta)


###############################################################################
# 6C.  Parse LogProgAlgoLL rows                                               #
###############################################################################
ll_tbl <- log_df %>%
  filter(event_type == "LogProgAlgoLL") %>%
  separate_rows(data, sep = " - ") %>%
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")),
    val = str_trim(str_extract(data, "(?<=\\[).*(?=\\])"))
  ) %>%
  filter(key == "Enable Stimulation Output") %>%
  mutate(
    LL_enabled = as.integer(val),
    LL_onoff = case_when(
      LL_enabled == 1 ~ "on",
      LL_enabled == 0 ~ "off",
      TRUE ~ NA_character_
    )
  ) %>%
  select(date, event_type, LL_onoff)


###############################################################################
# 7.  Pivot programming wide & rename headers                                 #
###############################################################################
prog_wide <- prog_long %>%
  select(date, event_type, param, value) %>%
  pivot_wider(id_cols = c(date, event_type),
              names_from  = param,
              values_from = value)

# ensure every parameter column exists
missing <- setdiff(keep_params, names(prog_wide))
if (length(missing)) prog_wide[missing] <- NA_character_

# build rename map, forcing PulseWidth (µs)
param_units <- prog_long %>% distinct(param, unit)
rename_map <- setNames(
  param_units$param,
  paste0(param_units$param, " (", param_units$unit, ")"))
rename_map["PulseWidth (µs)"] <- "PulseWidth"

prog_wide <- rename(prog_wide, !!!rename_map)

param_cols <- keep_params |>
  map_chr(~{
    nm <- names(rename_map)[rename_map == .x]
    if (length(nm)) nm[1] else .x
  })

# numeric amplitude tracker
prog_wide <- prog_wide %>%
  mutate(amp_val = as.numeric(`PhasicAmplitude (mA)`))

###############################################################################
# 8.  Placeholders for TherapyStart / End / PositionChange / LogProgAlgoLL    #
###############################################################################
placeholders <- log_df %>%
  filter(event_type %in% c("LogTherapyStart",
                           "LogTherapyEnd",
                           "LogPositionChange",
                           "LogProgAlgoLL")) %>%
  select(date, event_type) %>%
  bind_cols(as_tibble(setNames(rep(list(NA_character_), length(param_cols)),
                               param_cols))) %>%
  mutate(amp_val = NA_real_)

###############################################################################
# 9.  Combine & sort chronologically                                          #
###############################################################################
# Ensure ll_tbl has all columns
ll_tbl_full <- ll_tbl %>%
  mutate(!!!setNames(rep(list(NA_character_), length(param_cols)), param_cols)) %>%
  mutate(amp_val = NA_real_) %>%
  select(date, event_type, all_of(param_cols), amp_val, LL_onoff)


combined <- bind_rows(
  prog_wide %>% mutate(LL_onoff = NA_character_),
  amp_tbl,
  ll_tbl_full,
  placeholders %>% mutate(LL_onoff = NA_character_),
) %>%
  arrange(date)


###############################################################################
# 10.  Forward-fill parameters & compute amplitude                            #
###############################################################################
combined <- combined %>%
  fill(all_of(param_cols), .direction = "down")

for (i in seq_len(nrow(combined))) {
  if (i == 1 && is.na(combined$amp_val[i])) combined$amp_val[i] <- 0
  if (combined$event_type[i] == "LogChangeAmp")
    combined$amp_val[i] <- combined$amp_val[i - 1] + combined$amp_delta[i]
  if (is.na(combined$amp_val[i]))
    combined$amp_val[i] <- combined$amp_val[i - 1]
}

combined <- combined %>%
  mutate(`PhasicAmplitude (mA)` = sprintf("%.2f", amp_val)) %>%
  select(-amp_val, -amp_delta)

# blank all parameters on LogTherapyEnd rows
combined <- combined %>%
  mutate(across(all_of(param_cols),
                ~ ifelse(event_type == "LogTherapyEnd", NA_character_, .x)))

###############################################################################
# 11.  Table -> flextable                                                     #
###############################################################################
tbl <- combined %>%
  select(date, event_type, LL_onoff, any_of(param_cols))

ft <- flextable(tbl)

# bold any parameter change
bold_rows <- function(col){
  ch <- tbl[[col]] != dplyr::lag(tbl[[col]])
  ch[is.na(ch)] <- FALSE
  which(ch)
}
for (cl in param_cols) ft <- bold(ft, i = bold_rows(cl), j = cl)

###############################################################################
# 11-B.  Style boundary events (bold + color)                                 #
###############################################################################

# Bold + dark blue for Therapy Start
ft <- bold(ft, i = which(tbl$event_type == "LogTherapyStart"),
           j = "event_type", bold = TRUE)
ft <- bg(  ft, i = which(tbl$event_type == "LogTherapyStart"),
           bg = "#0D47A1")          # deep/dark blue

# Bold + light blue for Therapy End
ft <- bold(ft, i = which(tbl$event_type == "LogTherapyEnd"),
           j = "event_type", bold = TRUE)
ft <- bg(  ft, i = which(tbl$event_type == "LogTherapyEnd"),
           bg = "#BBDEFB")          # pale/light blue
ft <- align(ft, align = "center", part = "all")
ft <- width(ft, j = "date",       width = 1.5)
ft <- width(ft, j = "event_type", width = 1.5)
ft <- width(ft, j = param_cols,   width = 0.8)

###############################################################################
# 12.  Save to Word (landscape)                                               #
###############################################################################
landscape <- prop_section(page_size = page_size(orient = "landscape"))
output_dir <- here("reports/")
filename<-"programming_changes_log.docx"
output_filepath<-file.path(output_dir, filename)
save_as_docx(ft,
             path       = output_filepath, #"C:/Users/dbour/OneDrive/Documents/R-Projects/ChronologicalEventExport/Programming_Changes_Table.docx",
             pr_section = landscape)

# To view in RStudio instead of Word:
print(ft)
