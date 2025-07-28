#### CODE for parsing device log to get running traces and filled df for integration with the event grid to get parsed AHI calculation

#set up and load libraries
# set up and load libraries 
rm(list=ls())

library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)
library(data.table)      # for data.table operations
library(hms)
library(ggpubr)

#############################################
#### 2. Device Log Data #####################
#############################################
# read in the device log data
default_dir <- here("data/")

# file_path <- dlg_open(
#   default = default_dir,
#   title = "Select Device Log CSV File",
#   #filters = matrix(c("CSV files", "*.csv"), ncol = 3)
# )$res

file_path<-here("data/DeviceLog_[SN#000130]_23_06_2025_06_05_59.csv")

#read in raw data... NOTE this will likely throw a warning, use problems(log_raw) to check but it should be fine 
log_raw <- read_csv(file_path) %>% clean_names()

#clean and parse data
log_df <- log_raw %>%
  mutate(
    date_mdy=mdy_hms(date), #convert date to POSIXct 
    event_type = case_when(
      str_detect(event, regex("LogProgramming",            TRUE)) ~ "LogProgramming",
      str_detect(event, regex("LogChangeAmplitudeByOrder", TRUE)) ~ "LogChangeAmp",
      str_detect(event, regex("LogTherapyStart",           TRUE)) ~ "LogTherapyStart",
      str_detect(event, regex("LogTherapyEnd",             TRUE)) ~ "LogTherapyEnd",
      str_detect(event, regex("LogPositionChange",         TRUE)) ~ "LogPositionChange",
      str_detect(event, regex("LogProgAlgoLL",          TRUE)) ~ "LogProgAlgoLL",
      str_detect(event, regex("LogBLEConnection",        TRUE)) ~ "LogBLEConnection",
      str_detect(event, regex("LogBLEDisconnection",     TRUE)) ~ "LogBLEDisconnection",
      TRUE ~ "OTHER")
  ) %>%
  filter(event_type != "OTHER") %>%
  arrange(date)

# 1. Parse the log change amp by order rows: 
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
  select(date, event_type, OrderFromMobileApp, amp_delta) %>%
  mutate(date_mdy=mdy_hms(date))  %>%  
  filter(OrderFromMobileApp != "Yes") %>% #only keep rows NOT from the mobile app
  filter(date_mdy >= analysis_start_datetime & date_mdy <= analysis_end_datetime) 

# NOTE: It is possible that at this point there are 0 rows that are included in amp_tbl. This would be the case if no changes were made during the PSG using the logchange amp by order buttons


#2. Parse log data then, need to get the log 'data' column parsed out into different columns:

#separate the data column into independent columns
id_cols <- setdiff(names(log_df), "data")

log_wide <- log_df %>%
  separate_rows(data, sep = " - ") %>% #breaks into rows
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")), #pull the column name
    value = str_trim(str_extract(data, "(?<=\\[).*?(?=\\])")) #pull the column value; added a ? after the .* to be 'non-greedy' grabbing the shortest match not the longest bc for some reason the log sometimes has ]] at the end of things
  ) %>%
  select(all_of(id_cols), key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>% #make wide
  clean_names() #clean the new column names

#filter data based on the analysis start and end:
log_wide <- log_wide %>%
  filter(date_mdy >= analysis_start_datetime & date_mdy <= analysis_end_datetime)

#3. Find and parse the last LogProgAlgoLL row before the analysis start time:
algo_row_idx<-last(which(log_df$event_type=="LogProgAlgoLL" & 
                           log_df$date_mdy < analysis_start_datetime))

#split out the algo row into separate columns:
algo_row<- log_df[algo_row_idx,] %>% separate_rows(data, sep = " - ") %>% #breaks into rows
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")), #pull the column name
    value = str_trim(str_extract(data, "(?<=\\[).*?(?=\\])")) #pull the column value; added a ? after the .* to be 'non-greedy' grabbing the shortest match not the longest bc for some reason the log sometimes has ]] at the end of things
  ) %>%
  select(all_of(id_cols), key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>% #make wide
  clean_names() #clean the new column names

#DONT BIND NOW leave this for now and bind later with the log data after complete()

#select down some of the information we want to keep
log_wide %>%
  select(c("date_mdy","event","event_type","device_mode","magnet_mode", "roll_pause_mode",
           "phasic_time" ,"therapy_rate","rise_time","fall_time","tonic_amplitude",
           "phasic_amplitude","onset_ramp_time","ending_ramp_time","pulse_width",
           "upright_pause","roll_pause" ,"max_phasic_amplitude","min_phasic_amplitude",
           "amplitude_step","enable_tti_predict_algorithm" ,  "enable_xyz_algorithm",          
           "enable_centered_inhalation",     "enable_stimulation_output",     
           "enable_tti_freq_lock_algorithm", "battery_level",                 
           "lead_impedance", "therapy_duration", "therapy_end_cause", "was_increased",
           "order_from_mobile_app" ,"posture"))->log_wide

# 4. Identify which log programming rows go together with which therapy sessions:

#make a therapy sessions df 
st_idx<-which(log_wide$event_type=="LogTherapyStart")
end_idx<-which(log_wide$event_type=="LogTherapyEnd")
st_end_idx<-c(st_idx,end_idx)
therapy_pairs<-data.frame(start_times=log_wide$date_mdy[st_idx], #start times from LogTherapyStart rows
                          end_times=log_wide$date_mdy[end_idx]) #end times from LogTherapyEnd rows 
therapy_pairs <- therapy_pairs %>% mutate(row_id=row_number()) #add a session id

#create session ids
log_wide <- log_wide %>%
  arrange(date_mdy) #%>% commenting out therapy_session_id for now so i can assign it later inside the loop:
# mutate(therapy_session_id = cumsum(event_type == "LogTherapyStart"))

#set up empty array to store which log programming rows are relevant: 
keep_prog_df <- data.frame(keep_idx=logical(nrow(log_wide)))
#initialize therapy session ID column:
#log_wide$therapy_session_id <- NA 

#figure out which log programming rows go with which therapy session ID
for (i in 1:nrow(therapy_pairs)) {
  
  #define therapy session start and end:
  st <- therapy_pairs$start[i]
  en <- therapy_pairs$end[i]
  
  # a) programming rows inside [start, end] should be kept:
  inside <- which(log_wide$event_type == "LogProgramming" &
                    log_wide$date_mdy >= st & log_wide$date_mdy <= en)
  keep_prog_df$keep_idx[inside] <- TRUE
  
  # b) programming row immediately preceding start should be kept:
  prev_idx <- max(which(log_wide$event_type == "LogProgramming" &
                          log_wide$date_mdy < st))
  if (length(prev_idx) && prev_idx > 0) keep_prog_df$keep_idx[prev_idx] <- TRUE
  
  #dropping this for now, will ID therapy sessions later once the PA trace is in, because I don't need to fill down based on a therapy session, I'm doing it later using cumsum + log change amp by order rows 
  # # c) indicate which therapy session it belongs to
  # log_wide$therapy_session_id[inside] <- therapy_pairs$row_id[i]
  # log_wide$therapy_session_id[prev_idx] <- therapy_pairs$row_id[i]
}

#check that it worked as expected: 
check<-cbind(keep_prog_df,log_wide)


#discard any rows that are not a) within a session or b) immediately preceding one
df <- log_wide[!(log_wide$event_type == "LogProgramming" & !keep_prog_df$keep_idx), ]

#so now the only log programming rows included are ones that are explicitly set for a therapy session

# 5. Combine the df and the amplitude change data: 
combined<-df %>% 
  bind_rows(amp_tbl) %>% #bind the amp change table to the log data
  arrange(date_mdy) #rearrange by date

# 6. Add a running phasic amplitude trace (agnostic to roll pauses at first):

# first need to extract the phasic amplitude values from 'log programming change'
combined %>%
  rename(phasic_amp_char="phasic_amplitude") %>%
  replace_na(list(amp_delta=0)) -> new_df #replace NAs in amp_delta with 0, so that if there's no change by order rows it doesn't break the code below

#grab just the numeric part of the phasic amplitude column:
new_df %>% 
  mutate(phasic_amplitude=str_extract(phasic_amp_char, "\\d+\\.+\\d*")) %>% 
  mutate(phasic_amplitude=as.numeric(phasic_amplitude)) ->new_df 

# This gives me the programmed rows, need to account for log change by order rows (which may be 0, depending on the study, but i still need to account for them):


#drop any rows where event==NA so the double-logged events aren't included
new_df %>% drop_na(event) -> new_df

# Initialize running amplitude vector
running_amp <- numeric(nrow(new_df))

# Initialize current amplitude (assume 0 or set to initial known value)
current_amp <- 0

# Loop through rows to update running amplitude

for(i in 1:nrow(new_df)) {
  if(new_df$event[i] == "LogProgramming") {
    # Directly set the amplitude
    current_amp <- new_df$phasic_amplitude[i]
  } else if(new_df$event[i] == "LogChangeAmpByOrder") {
    # Increment or decrement by step size * direction
    current_amp <- current_amp + new_df$amp_delta
  }
  
  running_amp[i] <- current_amp
}

# Add the running amplitude as a new column to df
new_df$running_phasic_amplitude <- running_amp

#take a look to see that it's working as expected: 

check <- new_df %>% select(date_mdy, event, event_type, phasic_amplitude, running_phasic_amplitude)
#check that the phasic amplitude is working:
check %>%
  drop_na(running_phasic_amplitude) %>%
  ggplot(aes(x=date_mdy, y=running_phasic_amplitude)) +
  geom_point() +
  labs(title="Phasic Amplitude Over Time", x="Time", y="Phasic Amplitude (mA)") +
  theme_minimal()

#seems reasonable, now I think I expand out to the seconds level to make "continuous" data:

# 7. Expand data and fill down based on therapy and algo session:

#expand to get seconds level data
if (nrow(new_df %>% filter(!is.na(date_mdy))) > 0) {
  expanded <- new_df %>%
    filter(!is.na(date_mdy)) %>%
    complete(date_mdy = seq(min(date_mdy), max(date_mdy), by = "1 sec"))
} else {
  stop("No valid timestamps in new_df; cannot expand time series.")
}

if (nrow(algo_row) > 0) {
  expanded <- expanded %>%
    bind_rows(algo_row) %>%
    arrange(date_mdy)
}


#need to fill down therapy times a different way to make sure the last previous LogProgramming row is included in the therapy session
expanded %>%
  arrange(date_mdy) %>%
  
  #fill all the columns that get set inside a LogProgramming row (NOT including the phasic amplitude values that come from this since they get updated by logchangeorder)
  fill(phasic_time, therapy_rate, rise_time, fall_time, tonic_amplitude,
       onset_ramp_time, ending_ramp_time, pulse_width, roll_pause, roll_pause_mode,
       max_phasic_amplitude, min_phasic_amplitude, .direction = "down") %>%
  
  #fill in the NAs that were introduced to the running phasic amplitude when we did complete()
  fill(running_phasic_amplitude, .direction = "down") %>%
  
  #fill in most things that get set inside a LogProgAlgoLL row (not all because there's a ton)
  fill(device_mode, magnet_mode, roll_pause_mode, enable_tti_predict_algorithm,
       enable_xyz_algorithm, enable_centered_inhalation, enable_stimulation_output,
       enable_tti_freq_lock_algorithm, tti_averaging_filter, tti_peak_detect,
       min_valid_tti_pk_pk, max_valid_tti_pk_pk, max_valid_tti_pt_pt,
       .direction = "down") %>%
  
  #fill in the posture
  fill(posture, .direction = "down") -> filled_df

#take a look at the filled_df which should show continuous phasic amplitude data
filled_df %>%
  filter(date_mdy>=analysis_start_datetime) %>% #this will drop the algo row and other things before the PSG
  ggplot(aes(x=date_mdy, y=running_phasic_amplitude)) +
  geom_point() +
  labs(title="Phasic Amplitude Over Time", x="Time", y="Phasic Amplitude (mA)") +
  theme_minimal()

## Have to account for the roll pause that occurs after each log position change that is the length of roll_pause and is only enabled some of the time

####
# 8. Implement roll pauses and other logic columns
####

# Roll Pause Handling
roll_pauses <- filled_df %>%
  filter(event_type == "LogPositionChange", roll_pause_mode == "Enabled") %>%
  select(date_mdy, roll_pause) %>%
  mutate(roll_pause_numeric = as.numeric(str_extract(roll_pause, "\\d+")),  # Extract numeric part
         pause_end = date_mdy + dminutes(roll_pause_numeric))  # Convert to minutes

stop("Make sure you have checked for ghost pauses.")

#### STOP HERE AND MAKE SURE YOU GET GHOST ROLL PAUSES IN 
#add in the ghost roll pauses: 
roll_pauses %>% bind_rows(ghost_rolls)->roll_pauses

# Initialize roll_pause_active column as FALSE
filled_df <- filled_df %>%
  mutate(roll_pause_active = FALSE)

# Mark rows within each roll pause window
for (i in seq_len(nrow(roll_pauses))) {
  filled_df$roll_pause_active <- ifelse(
    filled_df$date_mdy >= roll_pauses$date_mdy[i] & filled_df$date_mdy <= roll_pauses$pause_end[i],
    TRUE,
    filled_df$roll_pause_active
  )
}

#Initialize therapy column:
filled_df <- filled_df %>%
  mutate(during_therapy_session = FALSE)

#Mark rows within a therapy session
for (i in seq_len(nrow(therapy_pairs))){
  filled_df$during_therapy_session <- ifelse(
    filled_df$date_mdy >= therapy_pairs$start_times[i] & 
      filled_df$date_mdy <= therapy_pairs$end_times[i],
    TRUE,
    filled_df$during_therapy_session
  )
}

#first make an algo column:
filled_df <- filled_df %>%
  mutate(
    any_algorithm_enabled = ifelse(
      enable_tti_predict_algorithm == 1 | enable_xyz_algorithm == 1 | 
        enable_centered_inhalation == 1 | enable_tti_freq_lock_algorithm == 1, TRUE, FALSE))

# Compute stim_active based on stimulation being enabled and roll pause not active
#NOTE!! IF all algorithms are turned off, enable_stim_output does NOT have to be on/1...no algorithms-->stim is enabled even if device says 0!!

filled_df <- filled_df %>%
  
  
  # two ways for stim to be active (MUST be in a therapy session) 
  mutate(stim_active = case_when(
    
    #1 - if an algorithm is enabled, stimulation is enabled, and roll pause is not active:
    during_therapy_session & any_algorithm_enabled & enable_stimulation_output == 1 & !roll_pause_active ~ running_phasic_amplitude ,
    
    #2 - if no algorithms are enabled, DOESN'T MATTER what enable_stim is doing, and roll pause is not active:
    during_therapy_session & !any_algorithm_enabled  & !roll_pause_active ~ running_phasic_amplitude,
    
    #otherwise, should be set to 0
    TRUE ~ 0
  )
  )

filled_df %>% 
  filter(date_mdy >= analysis_start_datetime) %>% #this will drop the algo row and other things before the PSG
  ggplot(aes(x=date_mdy, y=stim_active)) +
  geom_line()+theme_minimal()

#pivot longer and plot:
filled_df %>%
  filter(date_mdy >= analysis_start_datetime) %>% #this will drop the algo row and other things before the PSG
  select(date_mdy, stim_active, running_phasic_amplitude) %>%
  pivot_longer(cols = c(stim_active, running_phasic_amplitude), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x=date_mdy, y=value, color=variable)) +
  geom_line(show.legend = F) +
  theme_minimal()+
  facet_wrap(~ variable, scales = "free_y",ncol=1) +
  labs(x="Time", y="Amplitude (mA)") +
  theme(axis.text = element_text(size = 24),
        axis.title = element_text(size=32),
        legend.position = "none")

####
# 9. Identify therapy enabled periods based on user input:
####

#add a therapy enabled column based on when the efficacy portion of the night started. Manual for now, but may add something later:
te_start_char<- dlg_input(message="Please provide a therapy enabled start time (HH:MM:SS):")$res
te_end_char<- dlg_input(message="Please provide a therapy enabled end time (YYYY-MM-DD HH:MM:SS):")$res

te_start_time <-as.ITime(te_start_char) #convert to ITime
te_end_time <-as.ITime(te_end_char) #convert to ITime

therapy_enabled_start_date<-dplyr::if_else(te_start_time<=as.ITime("11:59:00"),end_date, start_date) #if before noon, assume next day
therapy_enabled_start_datetime <- as.POSIXct(therapy_enabled_start_date) + as.numeric(te_start_time) #combine date and time

#if no end time is provided just assume end of analysis is end of therapy:
if(length(te_end_time) == 0) {
  therapy_enabled_end_date <- analysis_end_datetime # if no end time provided, assume same as end of analysis
} else {
  therapy_enabled_end_date<-dplyr::if_else(te_end_time<=as.ITime("11:59:00"),end_date, start_date) #if before noon, assume next day
  therapy_enabled_end_datetime <- as.POSIXct(therapy_enabled_end_date) + as.numeric(te_end_time) #combine date and time
}

# 10. Add therapy enabled column to filled_df:
filled_df <- filled_df %>%
  mutate(therapy_enabled = ifelse(
    date_mdy >= therapy_enabled_start_datetime & date_mdy <= therapy_enabled_end_datetime,
    TRUE, FALSE
  )) %>%
  
  #add a categorical column for stimulating:
  mutate(stim_cat = if_else(stim_active > 0, TRUE, FALSE),
         
         #and an categorical column for cat_stim during therapy enabled time:
         stim_during_te = if_else(stim_cat==TRUE & therapy_enabled==TRUE, TRUE, FALSE)) 


####
# 11. Prepare device log data (analogous to waveform data) 
####

## filter out rows in filled_df that came from wake time: 
filled_dt<-as.data.table(filled_df)
for (i in 1:nrow(wake_epochs)){
  start_time <- wake_epochs$start_datetime[i]
  end_time <- wake_epochs$end_datetime[i]
  
  # Find the rows in all_data_dt that fall within the start and end times
  filled_dt[date_mdy >= start_time & date_mdy <= end_time, sleep_stage := "wake"]
}

filled_dt[is.na(sleep_stage), sleep_stage := "sleep"] #set all other rows to asleep

#add a duration column so I can sum and get time by strata:
# Calculate time difference to next timestamp (in seconds)
filled_dt[, duration := as.numeric(shift(date_mdy, type = "lead") - date_mdy, units = "secs")]

# The last row will have NA for duration; you may want to set it to 0 or remove it before summing
filled_dt[is.na(duration), duration := 0]

#examine 
filled_dt %>% 
  group_by(sleep_stage) %>% 
  summarize(total_time_hr = sum(duration)/3600) #total time in each sleep stage

# filter out the wake data:
sleeping <- filled_dt %>%
  filter(sleep_stage == "sleep") #keep only sleep data
