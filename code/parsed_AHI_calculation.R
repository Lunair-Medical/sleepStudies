## Code to estimate AHI for different phases of the night
## Designed to read in the device log file and the Nox event logs
# 
# 7/15/25 last updated 

# set up and load libraries 
rm(list=ls())
default_dir <- here("data/") # set default directory for file picker

library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)
library(data.table)      # for data.table operations

##### 1. Event Grid Data #####################

## read in event data in the form of a (scored) event grid out of Nox (make sure it's one with a 'sleep' column)
# eg_fp<-dlgOpen(
#   title = "Select the scored event grid file",
#   default=default_dir,
#   #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
#   multi = FALSE,
#   filters = dlgFilters[c("CSV files", "All files")])$res

eg_fp<-here("data/201-013_60-day_EventGrid_26-Jun-2025.csv")
#read in the event grid with the scored events 
event_grid<-read.csv(eg_fp) %>% clean_names()

#get the patient identifier off the event grid:
eg_filename<-basename(eg_fp)

#check the structure of the event grid:
str(event_grid)

#clean the event grid data (this will throw warnings about coercing factors to numeric, but that's okay):
event_grid %>%
  slice(-1) %>% # remove first row
  #  mutate(phasic_amplitude=as.numeric(phasic_amplitude_max)) %>% #make numeric
  #  mutate(stimulation=as.numeric(stimulation_max)) %>% #make numeric
  mutate(start_time=as_hms(start_time)) %>% #this will add a date; disregard the date here
  mutate(end_time=as_hms(end_time)) %>%
  drop_na()-> clean_events

#grab analysis start and end time off the nox event grid and prompt if it doesn't exist in the event grid::
start_time <- clean_events %>%
  filter(event == "Analysis Start") %>%
  pull(start_time) %>%
  first()

# Camden 7/17: Updated the if-statement logic for start and end time to fix error where the script wasn't prompting the user for an end time, causing missing-value problems down the line.
if (length(start_time) != 1) {
  stop("Analysis Start time not found in the event grid. Please provide a start time")
  st<-dlg_input(message="Please provide an analysis start time.")$res
  start_time <- as_hms(st)
}

end_time <- clean_events %>%
  filter(event == "Analysis End") %>%
  pull(end_time) %>%
  first()
if (length(end_time) != 1) {
  message("Analysis End time not found in the event grid. Please provide an end time")
  en<-dlg_input(message="Please provide an analysis end time.")$res
  end_time <- as_hms(en)
}


#check if there's already a date column:
if (!"date" %in% names(clean_events)) {
  
  #if not, create a date column based on user input:
  st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
  start_date<-as.Date(st_dt) #convert to Date
  end_date<-start_date + days(1) #assume the same date for now
  
  clean_events <- clean_events %>%
    mutate(
      date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date))  #events before noon automatically assigned to next day 
}

#if date already exists as a column, just ensure it's in the right format and concatenate:
clean_events <- clean_events %>% mutate(
  start_datetime = as.POSIXct(date) + as.numeric(start_time),
  end_datetime = as.POSIXct(date) + as.numeric(end_time)
)

#put them together into a datetime: 
analysis_start_datetime<- as.POSIXct(start_date) + as.numeric(start_time)
analysis_end_datetime <- as.POSIXct(end_date) + as.numeric(end_time)


#filter by event:
sleep_epochs<-clean_events %>% 
  filter(event %in% c("N1","N2", "N3", "REM","Wake")) %>%
  mutate(duration=as.numeric(duration))  #duration in seconds

wake_epochs<-sleep_epochs %>% filter (event=="Wake")

mvmts<-clean_events %>% 
  filter(event %in% c("Movement","Spontaneous Arousal","Respiratory Arousal")) %>%
  mutate(start_time=as_hms(start_time)) 

all_events<-clean_events %>%
  filter(event %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                      "H. Obstructive", "H.Mixed","Desat")) %>%
  filter(sleep!="Wake") #double check on this with david...not sure how there can be events if it's 'wake'


#now should be able to evaluate the TST:
total_sleep_time<-sleep_epochs %>%
  filter(event!="Wake") %>%
  summarize(TST=sum(duration)) %>%
  unlist() %>%
  unname() #total sleep time in seconds

tst_hours<-round(total_sleep_time/3600,1) #convert to hours

#calculate whole night AHI:
AHI_whole_night= all_events %>%
  filter(event !="Desat") %>%
  summarize(AHI=round(n()/total_sleep_time*3600,1)) #AHI in events per hour

#this is within 0.1 of the AHI calculated in the event grid, so it seems to be working okay.

#calculate whole night ODI:
ODI_whole_night= all_events %>%
  filter(event =="Desat") %>%
  summarize(ODI=round(n()/total_sleep_time*3600,1)) #ODI in events per hour


### At this point, the event grid data is loaded and ready for analysis.

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

file_path<-here("data/DeviceLog_[SN#000135]_26_06_2025_06_06_42.csv")

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

## Camden 7/17:

# 8. Implement roll pauses and other logic columns 
# Roll Pause Handling
roll_pauses <- filled_df %>%
  filter(event_type == "LogPositionChange", roll_pause_mode == "Enabled") %>%
  select(date_mdy, roll_pause) %>%
  mutate(roll_pause_numeric = as.numeric(str_extract(roll_pause, "\\d+")),  # Extract numeric part
         pause_end = date_mdy + dminutes(roll_pause_numeric))  # Convert to minutes

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
  geom_line()

# 9. Identify therapy enabled periods based on user input:

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
  ))

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


# Summarize by different conditions durations by stratum
te_strata <- sleeping[, .(total_time_secs = sum(duration), total_time_hr=sum(duration)/3600), by = therapy_enabled]

####
# 12. Match events to the device/waveform data
####

events_dt <- as.data.table(filter(all_events,event!="Desat")) #filter out desats for AHI calculation, already has 'wake' removed

#add empty columns to store summary values for the stimulation during that event and what stratum you're in when the event starts: 
sum_cols<-c("mean_stim","max_stim","min_stim","therapy_enabled","stim_cat","stim_val")
events_dt[, (sum_cols) := NA_real_]

#loop over the events dt and calculate summary values for each:
for (i in 1:nrow(events_dt)){
  
  start_time <- events_dt$start_datetime[i]
  end_time <- events_dt$end_datetime[i]
  
  # Find the rows in all_data_dt that fall within the start and end times
  relevant_rows <- sleeping[date_mdy >= start_time & date_mdy <= end_time]
  
  #for the events that are outside the analysis window, keep those as NA:
  if (nrow(relevant_rows) == 0) {
    events_dt[i, sum_cols] <- NA
    next #skip to the next iteration if no relevant rows found
    
  } else {
    
    #calculate summary stats for each event:
    mean(relevant_rows$stim_active, na.rm = TRUE) -> events_dt$mean_stim[i]
    min(relevant_rows$stim_active, na.rm = TRUE) -> events_dt$min_stim[i]
    max(relevant_rows$stim_active, na.rm = TRUE) -> events_dt$max_stim[i]

    #what was the status of stimulation when the event was starting?
    
    first(relevant_rows$therapy_enabled) -> events_dt$therapy_enabled[i]#status of therapy enabled flag
    events_dt$stim_cat[i] <- if_else(first(relevant_rows$stim_active)> 0, T,F) #stim flag status
    events_dt$stim_val[i] <- (first(relevant_rows$stim_active)) #stim amp value 
  }
}

#several rows at the end of events_dt are NA because therapy sessions stopped before end of the study (and therefore aren't in 'sleeping' bc it's filtered to the device rows i kept):
#so replace those NAs: 
events_dt %>%
  mutate(across(c("mean_stim","min_stim","max_stim","stim_val","stim_cat","therapy_enabled"), ~ replace_na(., 0))) -> events_dt #replace NAs with 0 for stim cols

#summarize by stim strata:
events_dt %>%
  group_by(stim_cat) %>%
  summarize(n_events = n()) -> events_by_stim_strata
events_by_stim_strata

#summarize by therapy enabled strata:
events_dt %>%
  group_by(therapy_enabled) %>%
  summarize(n_events = n()) -> events_by_therapy_strata
events_by_therapy_strata

#calculate time spent in each: 
te_strata_time <- sleeping %>%
  group_by(therapy_enabled) %>%
  summarize(total_time_secs = sum(duration), total_time_hr = sum(duration)/3600) #total time in each therapy enabled stratum
#summarize by sleep stage:
events_dt %>% 
  group_by(sleep) %>% 
  summarize(n_events=n())

#calculate an AHI by strata
AHI_by_strata <- merge(
  x= events_by_strata,
  y=strata_time,by.x="strat_at_start",by.y = "stratum")

AHI_by_strata %>%
  mutate(AHI=round(n_events/total_time_hr,1)) -> AHI_by_strata


#depth and length of apneas etc.
