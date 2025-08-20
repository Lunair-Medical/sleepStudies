###############################################################################
# 0.  Libraries                                                               #
###############################################################################
library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(svDialogs)
library(stringr)
library(dplyr)
library(openxlsx)
library(here)

###############################################################################
# 1.  Pick files & read tracking                                              #
###############################################################################
# CSV with device log
default_dir <- here("data/")
csv_path <- dlg_open(
  default  = default_dir,
  title    = "Select Device Log CSV File",
  filters  = matrix(c("CSV files","*.csv"), 2, byrow = TRUE)
)$res
stopifnot(nzchar(csv_path))

##############
#NEED TO ADD IN AUTOMATICALLY PULLING PTID
#############

# Tracking sheet with milestone dates as a tibble
tracking_path <- "C:/Users/MegMcEachram/Lunair Medical/R&D - Documents/FiH Data/ECLIPSE 1 - Paraguay/Analysis/px_tracking_sheet.xlsx"
#tracking_path <- "C:/Users/GraceBurkholder/Lunair Medical/R&D - Documents/FiH Data/ECLIPSE 1 - Paraguay/Analysis/px_tracking_sheet.xlsx"
track <- readxl::read_xlsx(tracking_path) %>%
  as_tibble() %>%      # ← ensure it’s a tbl_df/data.frame
  clean_names()
# Match serial number
log_sn_num <- str_extract(basename(csv_path), "\\d+") %>% as.integer()
row_info   <- track %>%
  mutate(device_sn_num = as.integer(device_sn)) %>%
  filter(device_sn_num == log_sn_num)
stopifnot(nrow(row_info)==1)

#Pull PatientID
patient_id <- row_info$patient_id[1]
message("DEBUG: patient_id = ", patient_id)

# Pull milestone dates, handling possible "x" prefix
base_milestones <- c("30d_activation_date","60d_date","90d_date","120d_date")
found           <- base_milestones[ base_milestones %in% names(row_info) ]
prefixed        <- paste0("x", base_milestones)
found_prefixed  <- prefixed[ prefixed %in% names(row_info) ]
milestone_cols  <- c(found, found_prefixed)

if(length(milestone_cols)==0) {
  stop("No milestone date columns (30d_activation_date, 60d_date, 90d_date) found in tracking sheet.")
}

milestones <- row_info %>%
  select(all_of(milestone_cols)) %>%
  pivot_longer(
    cols           = all_of(milestone_cols),
    names_to       = "milestone",
    values_to      = "value",
    values_drop_na = TRUE
  ) %>%
  mutate(
    milestone_date = case_when(
      inherits(value, "Date")    ~ value,
      inherits(value, "POSIXt")  ~ as.Date(value),
      # UNIX‐seconds → POSIXct → Date
      is.numeric(value) & value > 1e6 ~
        as.POSIXct(value, origin="1970-01-01", tz="UTC") %>% 
        as.Date(),
      # Excel‐serial days → Date
      is.numeric(value)               ~ as.Date(value, origin="1899-12-30"),
      # fall back: parse text via base R
      TRUE                            ~ as.Date(value)
    )
  ) %>%
  pull(milestone_date)


message("DEBUG: found milestone columns: ", paste(milestone_cols, collapse=", "))
message("DEBUG: milestones = ", paste(milestones, collapse=", "))

###############################################################################
# 2.  Read & parse the log                                                  #
###############################################################################
log_raw <- read_csv(csv_path,
                    col_types       = cols(.default="c"),
                    skip_empty_rows = TRUE,
                    comment         = "#") %>%
  clean_names() %>%
  filter(!is.na(date), str_detect(date, "\\d")) %>%
  mutate(
    row_id = row_number(),
    date   = str_remove(date, "\\.\\d+$") %>%
      parse_date_time(
        orders = c("mdy HMS p","mdy HMS"),
        tz     = "America/Chicago"
      )
  ) %>%
  filter(
    !is.na(date),
    date >= ymd("2025-01-01", tz="America/Chicago")
  )

#run this if it's patient 11!!: 
if (patient_id=="201-011"){
log_raw %>% 
  mutate (date= date + hours(12))->log_raw 
  
  #write it back out
  new_name<-paste0("corrected", basename(csv_path))
  corrected_filepath<-file.path(here("data/"),new_name)
  write.csv(log_raw,corrected_filepath)}

###############################################################################
# 3.  Define windows & types                                                 #
###############################################################################
milestone_windows <- tibble(
  milestone_date = milestones
) %>%
  mutate(
    start = as_datetime(milestone_date) + hours(12),
    end   = start + days(1)
  )

# sanity check – should print a 3×3 tibble (milestone_date, start, end):
print(milestone_windows)

non_prog_events <- c(
  "LogChangeAmplitudeByOrder",
  "LogTherapyStart","LogTherapyEnd",
  "LogPositionChange","LogProgAlgoLL",
  "LogTestStimStart"
)
prog_event <- "LogProgramming"

###############################################################################
# 4.  Filter, pick, parse & clean up                                         #
###############################################################################
final_selection <- purrr::pmap_dfr(
  list(
    milestone_date = milestone_windows$milestone_date,
    start          = milestone_windows$start,
    end            = milestone_windows$end
  ),
  function(milestone_date, start, end) {
    # subset to this 24-hour window
    win <- log_raw %>%
      filter(date >= start, date < end)
    
    # 1) non-programming events
    nonp <- win %>% 
      filter(event %in% non_prog_events)
    
    # 2a) LogProgramming just before each start
    starts <- win %>%
      filter(event %in% c("LogTherapyStart", "LogTherapyTestStimStart")) %>%
      pull(date)
    prior_prog <- purrr::map_dfr(starts, ~
                                   win %>%
                                   filter(event == prog_event, date < .x) %>%
                                   slice_tail(n = 1)
    )
    
    # 2b) LogProgramming between start→end
    therapy_periods <- win %>%
      filter(event %in% c("LogTherapyStart","LogTherapyEnd")) %>%
      arrange(date) %>%
      mutate(grp = cumsum(event == "LogTherapyStart")) %>%
      group_by(grp) %>%
      summarise(
        start2 = first(date[event=="LogTherapyStart"]),
        end2   = first(date[event=="LogTherapyEnd"]),
        .groups = "drop"
      ) %>%
      filter(!is.na(end2))
    during_prog <- purrr::map_dfr(seq_len(nrow(therapy_periods)), function(i) {
      p <- therapy_periods[i, ]
      win %>%
        filter(
          event == prog_event,
          date  >= p$start2,
          date  <= p$end2
        )
    })
    
    # 3) combine & dedupe on (event, data)
    base <- bind_rows(nonp, prior_prog, during_prog) %>%
      distinct(event, data, date, .keep_all = TRUE) %>%
      arrange(date)
    
    # 4) parse everything (but we'll only fill the programming & AlgoLL params)
    enriched <- base %>%
      mutate(
        milestone_date       = milestone_date,
        
        # — programming params —
        UprightPauseMode     = str_match(data, "UprightPauseMode\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        RollPauseMode        = str_match(data, "RollPauseMode\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        
        PhasicTimeRaw        = str_match(data, "PhasicTime\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        PhasicTime           = as.numeric(str_extract(PhasicTimeRaw, "\\d+")),
        
        TherapyRateRaw       = str_match(data, "TherapyRate\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        TherapyRate          = as.numeric(str_extract(TherapyRateRaw, "\\d+")),
        
        PhasicAmpRaw         = str_match(data, "PhasicAmplitude\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        PhasicAmplitude      = as.numeric(str_extract(PhasicAmpRaw, "\\d+\\.?\\d*")),
        
        PulseWidthRaw        = str_match(data, "PulseWidth\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        PulseWidth           = as.numeric(str_extract(PulseWidthRaw, "\\d+")),
        
        FrequencyRaw         = str_match(data, "Frequency\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        Frequency            = as.numeric(str_extract(FrequencyRaw, "\\d+")),
        
        UprightPauseRaw      = str_match(data, "UprightPause\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        UprightPause         = as.numeric(str_extract(UprightPauseRaw, "\\d+")),
        
        RollPauseRaw         = str_match(data, "RollPause\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        RollPause            = as.numeric(str_extract(RollPauseRaw, "\\d+")),
        
        AmplitudeStepRaw1    = str_match(data, "AmplitudeStep\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        AmplitudeStep        = as.numeric(str_extract(AmplitudeStepRaw1, "\\d+\\.?\\d*")),
        
        # — LogChangeAmplitudeByOrder params (no fill) —
        WasIncreasedRaw      = str_match(data, "WasIncreased\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        WasIncreased         = WasIncreasedRaw,
        
        AmpStepChangeRaw     = str_match(data, "AmplitudeStep\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        ChangeAmplitudeStep  = as.numeric(str_extract(AmpStepChangeRaw, "\\d+\\.?\\d*")),
        
        # — AlgoLL flags —
        EnableTTIPredictAlgo  = as.logical(as.integer(
          str_match(data,
                    "Enable TTI Predict Algorithm\\s*=\\s*\\[(\\d)\\]")[,2])),
        EnableXYZAlgo         = as.logical(as.integer(
          str_match(data,
                    "Enable XYZ Algorithm\\s*=\\s*\\[(\\d)\\]")[,2])),
        EnableStimOutput      = as.logical(as.integer(
          str_match(data,
                    "Enable Stimulation Output\\s*=\\s*\\[(\\d)\\]")[,2])),
        EnableTTIFreqLockAlgo = as.logical(as.integer(
          str_match(data,
                    "Enable TTI Freq Lock Algorithm\\s*=\\s*\\[(\\d)\\]")[,2]))
      ) %>%
      fill(
        # only these should carry forward
        UprightPauseMode, RollPauseMode, PhasicTime, TherapyRate,
        PhasicAmplitude, PulseWidth, Frequency, UprightPause, RollPause, AmplitudeStep,
        EnableTTIPredictAlgo, EnableXYZAlgo, EnableStimOutput, EnableTTIFreqLockAlgo,
        .direction = "down"
      ) %>%
      # drop Raw helpers
      select(-ends_with("Raw"))
    
    enriched
  }
)
#Adding Roll Pause Functionality
#Define your “lateral/active” postures
lat_active <- c("RightLateral","Supine","LeftLateral","Prone","Active")

final_selection <- final_selection %>%
  # 1) grab Posture, carry it forward
  mutate(
    Position = if_else(
      event == "LogPositionChange",
      str_match(data, "Posture\\s*=\\s*\\[([^\\]]+)\\]")[,2],
      NA_character_
    )
  ) %>%
  fill(Position, .direction = "down") %>%
  
  # 2) compute Pause as before
  mutate(
    Pause = case_when(
      event == "LogPositionChange" &
        Position %in% lat_active &
        RollPauseMode == "Enabled"   ~ RollPause,
      event == "LogPositionChange" &
        Position %in% lat_active &
        RollPauseMode != "Enabled"   ~ 0,
      event == "LogPositionChange" &
        !(Position %in% lat_active) &
        UprightPauseMode == "Enabled" ~ UprightPause,
      event == "LogPositionChange" &
        !(Position %in% lat_active) &
        UprightPauseMode != "Enabled" ~ 0,
      TRUE                           ~ NA_real_
    )
  )


#Final Selection Deubg
# # Inspect change events
# final_selection %>%
#   filter(event == "LogChangeAmplitudeByOrder") %>%
#   select(date, WasIncreased, ChangeAmplitudeStep) %>%
#   slice(1:5) %>%
#   print()
# 
# 
# # Quick verification
# final_selection %>%
#   filter(event == "LogChangeAmplitudeByOrder") %>%
#   select(date, data, WasIncreased, ChangeAmplitudeStep) %>%
#   slice(1:5) %>%
#   print()
# 
# # Inspect result
# final_selection %>% glimpse()


library(dplyr)
library(tidyr)
library(stringr)

# ─── Build out_df: carry amplitude & position forward ───────────────────────
out_df <- final_selection %>%
  arrange(date) %>%
  # 1) carry the last known programmed PhasicAmplitude into every row
  fill(PhasicAmplitude, .direction = "down") %>%
  # 2) extract Posture → Position on LogPositionChange and carry it forward
  mutate(
    Position = if_else(
      event == "LogPositionChange",
      str_match(data, "Posture\\s*=\\s*\\[([^\\]]+)\\]")[,2],
      NA_character_
    )
  ) %>%
  fill(Position, .direction = "down") %>%
  # 3) seed the running columns
  mutate(
    CurrentAmplitude = PhasicAmplitude,
    LastTherapyStart = as.POSIXct(NA, tz = "America/Chicago"),
    Duration         = NA_real_,
    Output           = NA_character_
  ) %>%
  # 4) ensure any NA flags become FALSE
  mutate(across(starts_with("Enable"), ~ replace_na(., FALSE)))

# ─── Initialize state from the first real amplitude ─────────────────────────
first_idx <- which(!is.na(out_df$CurrentAmplitude))[1]
curAmp    <- out_df$CurrentAmplitude[first_idx]
lastStart <- as.POSIXct(NA, tz = "America/Chicago")

# ─── Walk each row, updating state & Output ─────────────────────────────────
for (i in seq_len(nrow(out_df))) {
  row <- out_df[i, ]
  
  if (row$event == "LogProgramming") {
    # reset to the newly programmed amplitude
    curAmp <- row$PhasicAmplitude
    out_df$CurrentAmplitude[i] <- curAmp
    
  } else if (row$event == "LogChangeAmplitudeByOrder") {
    # step amplitude up or down
    curAmp <- curAmp + ifelse(
      row$WasIncreased == "Yes",
      row$ChangeAmplitudeStep,
      -row$ChangeAmplitudeStep
    )
    out_df$CurrentAmplitude[i] <- curAmp
    
  } else if (row$event == "LogTherapyStart") {
    # algorithm logic
    if      (row$EnableXYZAlgo       && row$EnableStimOutput) {
      out_df$Output[i] <- "XYZ Enabled"
    } else if (row$EnableXYZAlgo     && !row$EnableStimOutput) {
      out_df$Output[i] <- "XYZ Sensing Only"; curAmp <- 0
    } else if (row$EnableTTIPredictAlgo && row$EnableStimOutput) {
      out_df$Output[i] <- "Predictive Enabled"
    } else if (row$EnableTTIPredictAlgo && !row$EnableStimOutput) {
      out_df$Output[i] <- "Predictive Sensing Only"; curAmp <- 0
    } else if (row$EnableTTIFreqLockAlgo && row$EnableStimOutput) {
      out_df$Output[i] <- "RateLock Enabled"
    } else if (row$EnableTTIFreqLockAlgo && !row$EnableStimOutput) {
      out_df$Output[i] <- "RateLock Sensing Only"; curAmp <- 0
    } else {
      out_df$Output[i] <- "OpenLoop"
    }
    out_df$CurrentAmplitude[i]  <- curAmp
    lastStart                   <- row$date
    out_df$LastTherapyStart[i]  <- lastStart
    
  } else if (row$event == "LogTherapyEnd") {
    # compute duration in seconds
    dur_sec <- as.numeric(difftime(row$date, lastStart, units = "secs"))
    # format as hh:mm:ss
    hh        <- dur_sec %/% 3600
    mm        <- (dur_sec %% 3600) %/% 60
    ss        <- round(dur_sec %% 60)
    formatted <- sprintf("%02d:%02d:%02d", hh, mm, ss)
    
    out_df$Duration[i] <- dur_sec       # numeric duration
    out_df$Output[i]   <- formatted     # formatted hh:mm:ss
    
    # carry forward amplitude & start time
    out_df$CurrentAmplitude[i]  <- curAmp
    out_df$LastTherapyStart[i]  <- lastStart
    
  } else if (row$event == "LogPositionChange") {
    # carry forward state
    out_df$CurrentAmplitude[i] <- curAmp
    out_df$LastTherapyStart[i] <- lastStart
    
    # posture‐pause message if amplitude > 0
    if (curAmp > 0) {
      out_df$Output[i] <- paste0(
        row$Position,
        " Therapy paused for ",
        row$Pause,
        " min."
      )
    }
    
  } else {
    # all other events: carry forward
    out_df$CurrentAmplitude[i] <- curAmp
    out_df$LastTherapyStart[i] <- lastStart
  }
}

# ─── Quick inspection ────────────────────────────────────────────────────────
out_df %>%
  select(date, event, Position, Pause, CurrentAmplitude, Output, Duration) %>%
  slice(1:10) %>%
  print()

###############################################################################
# 5.  Write to Excel                                                          #
###############################################################################
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

for (i in seq_len(nrow(milestone_windows))) {
  # pull this milestone’s window
  wd   <- milestone_windows$milestone_date[i]
  ws   <- milestone_windows$start[i]
  we   <- milestone_windows$end[i]
  
  # 1) subset & drop the AlgoLL rows
  subset_df <- out_df %>%
    filter(
      date  >= ws,
      date  <  we,
      event != "LogProgAlgoLL"
    )
  if (!nrow(subset_df)) next
  
  # 2) build slim_df with Position, formatted Date, and blank‐out on TherapyEnd
  slim_df <- subset_df %>%
    # a) extract and carry posture → Position
    mutate(
      Position = if_else(
        event == "LogPositionChange",
        str_match(data, "Posture\\s*=\\s*\\[([^\\]]+)\\]")[,2],
        NA_character_
      )
    ) %>%
    fill(Position, .direction = "down") %>%
    # b) pick just the cols we want (including PhasicAmplitude now!)
    select(
      Date             = date,
      Event            = event,
      Output,
      CurrentAmplitude,
      PhasicTime,
      TherapyRate,
      Frequency,
      PulseWidth,
      Position
    ) %>%
    # c) blank out all params on LogTherapyEnd rows
    mutate(across(
      c(CurrentAmplitude, PhasicTime, TherapyRate,
        Frequency, PulseWidth, Position),
      ~ if_else(Event == "LogTherapyEnd", NA, .)
    )) %>%
    # d) force Date into uniform text
    mutate(
      Date = format(Date, "%Y/%m/%d %H:%M:%S")
    )
  
  # 3) create & fill workbook
  wb <- createWorkbook()
  addWorksheet(wb, "Report")
  writeData(
    wb, "Report", slim_df,
    headerStyle = createStyle(textDecoration = "bold")
  )
  
  # 4) highlight start/end rows
  style_start <- createStyle(fgFill = "#DCE6F1")
  style_end   <- createStyle(fgFill = "#BDD7EE")
  nC          <- ncol(slim_df)
  for (r in seq_len(nrow(slim_df))) {
    rowX <- r + 1
    ev   <- slim_df$Event[r]
    if (ev == "LogTherapyStart") {
      addStyle(wb, "Report", style_start, rows = rowX, cols = 1:nC, gridExpand = TRUE)
    }
    if (ev == "LogTherapyEnd") {
      addStyle(wb, "Report", style_end,   rows = rowX, cols = 1:nC, gridExpand = TRUE)
    }
  }
  
  # 5) bold changed programming parameters
  boldPar  <- createStyle(textDecoration = "bold")
  params   <- c("CurrentAmplitude","PhasicTime",
                "TherapyRate","Frequency","PulseWidth")
  pcols    <- match(params, names(slim_df))
  progRows <- which(slim_df$Event == "LogProgramming")
  for (k in seq_along(progRows)[-1]) {
    thisR <- progRows[k]
    prevR <- progRows[k-1]
    sheetRow <- thisR + 1
    for (col in pcols) {
      new <- slim_df[[col]][thisR]
      old <- slim_df[[col]][prevR]
      if (!is.na(new) && new != old) {
        addStyle(wb, "Report", boldPar, rows = sheetRow, cols = col, gridExpand = TRUE)
      }
    }
  }
  
  # 6) bold PhasicAmplitude on change‐order rows
  changeRows <- which(slim_df$Event == "LogChangeAmplitudeByOrder")
  paCol      <- match("CurrentAmplitude", names(slim_df))
  if (length(changeRows)) {
    addStyle(wb, "Report", boldPar,
             rows = changeRows + 1, cols = paCol, gridExpand = TRUE)
  }
  
  # 7) set print layout to letter landscape, fit to width
  pageSetup(wb, "Report",
            orientation = "landscape",
            paperSize   = 1,
            fitToWidth  = 1,
            fitToHeight = 0)
  
  # 8) save using patient_id + milestone date
  fname    <- sprintf("LogCrawler_%s_%s.xlsx", patient_id, format(wd, "%Y%m%d"))
  out_path <- file.path(here("data/"), fname)
  saveWorkbook(wb, out_path, overwrite = TRUE)
  message("✅ Written: ", out_path)
}

