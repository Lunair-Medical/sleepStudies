# Code to estimate AHI for different phases of the night
#Designed to read in the device log file and the Nox event logs
# 
# 7/8/25 last updated 

#set up and load libraries 
rm(list=ls())

library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)

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

#clean and parse data
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

#separate the data column into independent columns
id_cols <- setdiff(names(log_df), "data")

log_wide <- log_df %>%
  separate_rows(data, sep = " - ") %>% #breaks into rows
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")), #pull the column name
    value = str_trim(str_extract(data, "(?<=\\[).*(?=\\])")) #pull the column value
  ) %>%
  select(all_of(id_cols), key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>% #make wide
  clean_names() #clean the new column names

#select down some of the information we want to keep
log_wide %>%
  select(c("date","event","event_type","device_mode","magnet_mode", "roll_pause_mode",
                     "phasic_time" ,"therapy_rate","rise_time","fall_time","tonic_amplitude",
                     "phasic_amplitude","onset_ramp_time","ending_ramp_time","pulse_width",
                     "upright_pause","roll_pause" ,"max_phasic_amplitude","min_phasic_amplitude",
                     "amplitude_step","enable_tti_predict_algorithm" ,  "enable_xyz_algorithm",          
                     "enable_centered_inhalation",     "enable_stimulation_output",     
                     "enable_tti_freq_lock_algorithm", "battery_level",                 
                     "lead_impedance", "therapy_duration", "therapy_end_cause", "was_increased",
                     "order_from_mobile_app" ,"posture"))->log_wide

#write wider based on therapy start and end times 
st_idx<-which(log_wide$event_type=="LogTherapyStart")
end_idx<-which(log_wide$event_type=="LogTherapyEnd")
st_end_idx<-c(st_idx,end_idx)
therapy_pairs<-data.frame(start_times=log_wide$date[st_idx],
                          end_times=log_wide$date[end_idx])
therapy_pairs <- therapy_pairs %>% mutate(row_id=row_number()) #add a session id
 
#create session ids
log_wide <- log_wide %>%
  mutate(therapy_session_id = cumsum(event_type == "LogTherapyStart"),
         algo_session_id = cumsum(event_type == "LogProgAlgoLL")) 

#set up empty array to store which log programming rows are relevant: 
keep_prog_df <- data.frame(keep_idx=logical(nrow(log_wide)))

#figure out which log programming rows go with which therapy session ID
for (i in 1:nrow(therapy_pairs)) {
  
  #define therapy session start and end:
  st <- therapy_pairs$start[i]
  en <- therapy_pairs$end[i]
  
  # a) programming rows inside [start, end] should be kept:
  inside <- which(log_df$event_type == "LogProgramming" &
                    log_df$date >= st & log_df$date <= en)
  keep_prog_df$keep_idx[inside] <- TRUE
  
  # b) programming row immediately preceding start should be kept:
  prev_idx <- max(which(log_df$event_type == "LogProgramming" &
                          log_df$date < st))
  if (length(prev_idx) && prev_idx > 0) keep_prog_df$keep_idx[prev_idx] <- TRUE
  
  # c) indicate which therapy session it belongs to
  log_wide$therapy_session_id[inside] <- therapy_pairs$row_id[i]
  log_wide$therapy_session_id[prev_idx] <- therapy_pairs$row_id[i]
}

#check that it worked as expected: 
check<-cbind(keep_prog_df,log_wide)

#I think it did, so now i want to fill down based on session ID

#discard any rows that are not a) within a session or b) immediately preceding one
fl_log_wide <- log_wide[!(log_wide$event_type == "LogProgramming" & !keep_prog_df$keep_idx), ]

#so now the only log programming rows are ones that are explicitly set for a therapy session

#fill values forward
df <-fl_log_wide %>%
  
  #fill down whether stim is enabled or not first and the algorithms that are enabled
  group_by(algo_session_id) %>%
  fill(enable_tti_predict_algorithm, enable_xyz_algorithm,
       enable_centered_inhalation, enable_stimulation_output,
       enable_tti_freq_lock_algorithm, .direction = "down") %>%
  ungroup() %>%

  #fill down the therapy rate, rise time, fall time, tonic amplitude, phasic amplitude
  group_by(therapy_session_id) %>%
  fill(therapy_rate, rise_time, fall_time, tonic_amplitude,
       phasic_amplitude, onset_ramp_time, ending_ramp_time, pulse_width,
       upright_pause, roll_pause, max_phasic_amplitude, min_phasic_amplitude,
       amplitude_step,roll_pause_mode, .direction = "down") %>%
  ungroup()

#### Add Roll Pauses ####

## Have to account for the roll pause that occurs after each log position change that is the length of roll_pause and is only enabled some of the time
rp<-df %>%
  filter(event=="LogPositionChange", roll_pause_mode=="Enabled") %>%
  arrange(date)

rp_starts<-c()
rp_ends<-c()

current_pause_end <- as.POSIXct(NA)
pause_duration<-0

for (i in 1:length(nrow(df)){
  roll_time
}
#ok so now I want to identify if therapy is enabled and being delivered 
df %>% 
  mutate(therapy_enabled= case_when(enable_stimulation_output==1 &=TRUE)