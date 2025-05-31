#Summarize data by run for board meeting 

#Takes two sources of data as input: event grid from Noxturnal and manually selected lab chart runs, to provide calculated AHI and ODI for stim on vs stim off.

#setup
rm(list=ls())

## Load necessary libraries
library(tidyverse)
library(here)
library(readxl)
library(ggmosaic)
library(lubridate)
library(ggpubr)
library(svDialogs)
library(hms)
library(fuzzyjoin)

## set paths
#output_path<-here("figures")
default_dir<-"C:/Users/MegMcEachram/Downloads/"

ref_date<-as.Date("2025-05-20") #this will eventually be pulled from the patient tracking sheet but for now is just hard coded 
## graphic settings
lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2", "#397b96")

extrafont::loadfonts()

theme_lunair <- function(textsize=24){
  theme_minimal() %+replace% 
    theme(text = element_text(#family = "Arial", 
      size = textsize), 
      axis.ticks.length=unit(-0.05, "in"), 
      axis.text.y.right = element_blank(), 
      axis.text.x.top = element_blank(), 
      axis.title.y.right = element_blank(),
      axis.title.x.top = element_blank(),
      #panel.border = element_rect(fill = NA),
      plot.title = element_text(size = textsize,# face = "bold", 
                                hjust = 0),
      legend.position = "none",
      panel.background = element_rect(fill = "white",colour = "white"),
      plot.background = element_rect(fill = "white",colour = "white"))
  #strip.background = element_rect(fill="white"))
}

## value settings
stim_threshold<-200 #threshold value for the 'on'/off designation for stim

## read in data in the form of a (scored) event grid out of Nox
fp<-dlgOpen(
  title = "Select the scored event grid file",
  default=default_dir,
  #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
  multi = FALSE,
  filters = dlgFilters[c("CSV files", "All files")])$res

#set the default directory to the directory of the file selected:
patient_dir= str_extract(fp, ".*[\\\\/]")

# #dialog box to select where the summary figures will be saved: 
# output_path<-dlgDir(
#   title = "Select the output directory for figures",
#   default=patient_dir)$res

### DATA ANALYSIS 

#read in event grid data
events_data<-read.csv(fp)

#clean and rename data
events_data %>%
  slice(-1) %>% # remove first row
  rename(
    start_time = "Start.Time",
    end_time = "End.Time",
    phasic_amplitude = "Phasic.Amplitude..max.", 
    stim="Stimulation..max.") %>%
  mutate(amp_factor=as.factor(as.character(phasic_amplitude))) %>%
  mutate(Event=as.factor(as.character(Event))) %>%
  #mutate(time=as.POSIXct(str_extract(start_time,pattern="^[^\\.]+"),format="%H:%M:%S")) %>% #this will add a date; disregard the date here
  mutate(Duration=as.numeric(Duration)) %>%
  mutate(Start.Epoch=as.numeric(Start.Epoch)) %>% 
  mutate(End.Epoch=as.numeric(End.Epoch))-> events_data

## To get stimulation information we need to read in data that has been manually selected and exported into labchart: 
labchart_raw<-read.csv("C:/Users/MegMcEachram/Downloads/201-013 30d psg lab chart export.csv")

#clean up the labchart read in: 
labchart_raw%>% 
  slice(-c(1,2)) %>%
  rename(original_time="Time") %>%
  rename(run_number="Run") %>% 
  select(-c("Abdomen"    ,      "Audio.Volume.dB" ,"ECG","Flow"  ,"Heart.Rate",
            "Left.Leg" ,        "PosAngle"   ,      "Pulse"  ,"Saturation"  ,"Y.Axis" ,
            "Acceleration..Z.", "Impedance",
            "Thermistor","Thorax","X.Axis","Z.Axis"))->labchart #remove random extra columns

#chop off empty rows: 
# Find the last non-blank row
is_blank <- apply(labchart, 1, function(row) all(str_trim(row) == "" | is.na(row)))
last_non_blank <- max(which(!is_blank))

# Keep only up to the last non-blank row
labchart <- labchart[1:last_non_blank, ]

#mutate columns:
labchart %>% mutate(stim_numeric=as.numeric(Stimulation))%>%
 mutate(parsed_time=as.POSIXct(original_time,format= "%H:%M:%OS")) %>% 
  mutate(time_only=as_hms(parsed_time))->labchart

labchart <- labchart %>%
  mutate(assigned_date = if_else( time_only > as_hms("18:00:00") & time_only < as_hms("23:59:59"), ref_date, ref_date + 1)) %>% 
  mutate(assigned_date = as_date(assigned_date)) %>% 
  mutate(datetime = as.POSIXct(paste(assigned_date, format(parsed_time, "%H:%M:%S"))))


## Have to fix the weird artifact where stim appears to be slowly ramping through time, so write code to go through and identify where there are long runs of nonzero numbers:
labchart$stim_numeric->all_stim_values
nonzero_logical<-all_stim_values != 0
r<-rle(nonzero_logical) #run length encoding to find runs of "true" (nonzero) and the length of those runs
long_nonzero_runs<-which(r$values==TRUE & r$lengths>6) #find the runs of nonzeros that are longer than 6, since actual stim bursts always come in 3 or 6 length runs.

# Compute the end positions of each run
ends <- cumsum(r$lengths)
starts <- ends - r$lengths + 1

# Get positions of long nonzero runs
long_indices <- unlist(mapply(seq, starts[long_nonzero_runs], ends[long_nonzero_runs]))


#now correct the stim values: 
labchart %>% 
  mutate(corrected_stim = case_when(row_number() %in% long_indices ~ 0, #set the artifact long runs of nonzero stim to zero
                                    TRUE ~ stim_numeric))->labchart  #otherwise keep the original stim values

#plt stim over time to double check 
labchart %>% ggplot() + aes(x=datetime,y=corrected_stim) + geom_line() 

#NOW I can add stim_binary:
labchart %>% 
  mutate(stim_binary = case_when(corrected_stim !=0 ~ 1, 
                                 TRUE ~ 0)) -> labchart 


#group by run number and summarize the start and end times of each run:
labchart_runs<-labchart %>% 
  group_by(run_number) %>% 
  summarise(
    start_time = first(datetime), 
    end_time = last(datetime), 
    stim_val=sum(stim_binary),
    T_all= as.numeric(difftime(end_time, start_time, units = "sec")),
    stim_percentage=stim_val/T_all)#total time in the run

#actually need to break the runs down further than just run number so first need a smoothed stim value: 

#figure out what the 'actual' stim is (rounding to nearest 50 milli-amp increment)
round_to_50 <- function(x) {
  round(x / 50) * 50
}


#apply to stim_numeric:
labchart %>% 
  mutate(smoothed_stim_numeric=round_to_50(corrected_stim)) ->labchart
# 
# #######################################################

### ON A PER RUN BASIS:

#split out the run
run1<-labchart %>% filter(run_number==1)
ggplot(run1) + aes(x=datetime,y=smoothed_stim_numeric) + geom_line() + theme_lunair()
nonzero_logical<-run1$smoothed_stim_numeric > 1 #this should grab all the 0's but some of them don't show up as 'true' zeros for whatever reason

r <-rle(nonzero_logical) #run length encoding to find runs of stim values and the length of those runs
true_stim_runs<-which(r$values==TRUE & r$lengths>=3) #find runs of stim values that are longer than 3, since actual stim bursts always come in 3.

# Compute the starting index of each run
series_starts <- cumsum(c(1, head(r$lengths, -1)))
series_ends <- cumsum(r$lengths)

#identify the indices that are part of 'true' stim runs (aka just a stim run)
stim_indices<- unlist(mapply(seq, series_starts[true_stim_runs], series_ends[true_stim_runs]))

#make a dataframe of the stim bursts (ie true nonzero values):
bursts_df<- data.frame( 
  index=stim_indices,
  burst_id=cumsum(c(1,diff(stim_indices) != 1))
)

#merge burst id with original data:  
run1 %>% mutate(row_id=row_number()) -> run1 #add a row id to the run data
lc<-left_join(run1,bursts_df, by=c("row_id" = "index"))

#add a column that's the max of each stim burst
lc <- lc %>%
  group_by(burst_id) %>%
  mutate(
    burst_max = ifelse(!is.na(burst_id), max(smoothed_stim_numeric, na.rm = TRUE), smoothed_stim_numeric)
  ) %>%
  ungroup()


#make a column that identifies each stim series (incl. series where the value = 0)
lc %>% mutate(stim_series=cumsum(burst_max != lag(burst_max, default = first (burst_max)+1)))->lc2


lc3 <- lc2 %>%
  mutate(
    # Only keep the value if it's nonzero and different from the previous nonzero
    new_val = if_else(burst_max != 0 & (lag(burst_max, default = 0) != burst_max), burst_max, NA_real_)
  ) %>%
  # Fill the last seen non-NA value forward
  fill(new_val, .direction = "down") %>%
  # Replace any remaining NA (before first nonzero) with 0
  mutate(running_stim_tracker = replace_na(new_val, 0)) %>%
  select(-new_val)


## that will have papered over my long runs of zeroes so need to put those back in: 
nonzero_logical<-lc3$smoothed_stim_numeric != 0
r<-rle(nonzero_logical) #run length encoding to find runs of "true" (nonzero) and the length of those runs
long_zero_runs<-which(r$values==FALSE & r$lengths>2)
# Compute the end positions of each run
ends <- cumsum(r$lengths)
starts <- ends - r$lengths + 1

# Get positions of long zero runs in the original data so that we can correct the running tracker:
long_zero_indices <- unlist(mapply(seq, starts[long_zero_runs], ends[long_zero_runs]))

#now correct the running tracker:
lc3 %>% 
  mutate(running_stim_tracker = case_when(row_number() %in% long_zero_indices ~ 0, #set the artifact long runs of zero stim to zero
                                          TRUE ~ running_stim_tracker)) -> lc3 #otherwise keep the original running tracker values

# #identify the value of each stim series and find the total duration of each stim series: 
lc3 %>% group_by(stim_series) %>% 
  mutate(stim_series_start=first(datetime),
         stim_series_end=last(datetime),
         stim_series_duration=stim_series_end-stim_series_start)->lc4

#want only unique values so we don't double count a series (bc its duration occurs on multiple rows)
df_unique <- lc4 %>%
  group_by(stim_series, running_stim_tracker) %>%
  summarize(stim_series_duration = first(stim_series_duration), .groups = "drop")

### ^ I think this is wrong bc i dont actually want stim series duration i want the duration across the whole tracker run 
# Now, sum the durations for each stim level, using the unique stim series:
stim_summary <- df_unique %>%
  group_by(running_stim_tracker) %>%
  summarize(total_duration = sum(stim_series_duration, na.rm = TRUE))
# mvd %>% group_by(stim_series) %>% summarize(n=n(),
#                                             value=first(middle_values),
#                                             stim_series_start=first(mv_times),
#                                             stim_series_end=last(mv_times),
#                                             stim_series_duration=stim_series_end-stim_series_start)->mvd_summary


# Okay NOW I need to see how many events occur (start time) within the interval of a stim series
### Back to event grid data to link event start times with the intervals in the labchart data:

#filter out wake time on the event grid data and fix the time columns
#events_data %>% filter(Sleep!="Wake") -> sleeping_only
events_data -> sleeping_only
sleeping_only %>%
  #mutate(stim_numeric=as.numeric(Stimulation))  %>% #convert stim to numeric
  mutate(parsed_start=as.POSIXct(start_time,format= "%H:%M:%OS")) %>%
  mutate(start_time_only=as_hms(parsed_start))->sleeping_only


#doing that added an arbitrary date so we have to fix it back to the actual procedure dates:
events_while_asleep <- sleeping_only %>%
  filter(Event  %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                       "H. Obstructive", "H.Mixed"))

events_while_asleep <- events_while_asleep %>%
  mutate(assigned_date = if_else(start_time_only > as_hms("18:00:00") &start_time_only < as_hms("23:59:59"), ref_date, ref_date + 1)) %>%
  mutate(assigned_date = as_date(assigned_date)) %>%
  mutate(datetime = as.POSIXct(paste(assigned_date, format(parsed_start, "%H:%M:%S"))))



# #FIRST match the event start times with the run intervals identified in labchart:
# matched <- fuzzy_full_join(
#   events_while_asleep, labchart_runs,
#   by = c("datetime" = "start_time", "datetime" = "end_time"),
#   
#   match_fun = list(`>=`, `<=`)
# )
# 
# matched %>% filter(run_number==1)->matched_run_only

events_while_asleep %>% 
  filter(datetime <= labchart_runs$end_time[1] & datetime >= labchart_runs$start_time[1]) -> events_in_run

## NOW need to break it down and match on data from run number 1 only:
matched_stim_by_run<- fuzzy_inner_join( # want to keep events that fall within a stim series bc recall, the stim series can now include runs where stim is 0 so that's ok
  events_in_run, lc4,
  by = c("datetime" = "stim_series_start", "datetime" = "stim_series_end"),
  match_fun = list(`>=`, `<=`)
)

#get just the unique events: 
matched_stim_by_run %>%
  group_by(running_stim_tracker) %>%
  summarise(n_events = n_distinct(datetime.x))->run_summary


#now merge with the stim summary:
all_summary <- merge(run_summary, stim_summary, by = "running_stim_tracker",all.y = T) %>%
  replace_na(list(n_events = 0)) #have to have this in case there was a stim level that didn't appear in events 

all_summary %>% 
  mutate(duration_hours=as.numeric(total_duration,"hours")) %>%
  mutate(AHI=n_events/duration_hours) %>%
  mutate(sub_therapeutic=ifelse(running_stim_tracker<500,1,0)) %>%
  mutate(Stim500=ifelse(running_stim_tracker>=500,1,0)) %>%
  mutate(Stim550=ifelse(running_stim_tracker>=550,1,0)) %>%
  mutate(Stim600=ifelse(running_stim_tracker>=600,1,0)) %>%
  mutate(Stim650=ifelse(running_stim_tracker>=650,1,0)) -> all_summary


#binning by each value:
binning_vals<-c("sub_therapeutic","Stim500","Stim550","Stim600","Stim650")
binned_run_summaries<-vector(mode="list",length=length(binning_vals))

for (col in binning_vals){
  run_summary %>%
    group_by(.data[[col]]) %>%
    summarise(n_events = sum(n_events),total_duration = sum(total_duration, na.rm = TRUE),
              duration_hours=as.numeric(total_duration,"hours"),
              calc_AHI=n_events/duration_hours) ->result
  #result -> binned_run_summaries[[col-3]]
  print(paste("Grouped by","", col))
  print(result) }


#there isn't actually a stim duration for the "NA" in the run summary, so we need to calculate the unstimmed AHI within the run:
unstimmed_time<-labchart_runs$T_all[1]-sum(run_summary$total_duration[1], na.rm = TRUE)
unstimmed_AHI<-run_summary$n_events[is.na(run_summary$value)]/as.numeric(unstimmed_time,"hours")
unstimmed_AHI

#run-based on/off
events_while_asleep %>%
  filter(Event  %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                       "H. Obstructive", "H.Mixed")) %>%
  filter(start_time_only<= labchart_runs$end_time[1] & start_time_only >= labchart_runs$start_time[1])%>%
  summarise(n_events = n()) -> run_based_on_off




# ##### RUN 2 #############################
#need to go through the process from the top including grabbing the run from the data but i think i'll just functionalize it...


parse_stimulation_runs<-function(labchart_data,run_num){
  #split out the run
  run<-labchart_data %>% filter(run_number==run_num)
  nonzero_logical<-run$smoothed_stim_numeric != 0
  
  r <-rle(nonzero_logical) #run length encoding to find runs of stim values and the length of those runs
  true_stim_runs<-which(r$values==FALSE & r$lengths>=3) #find runs of stim values that are longer than 3, since actual stim bursts always come in 3.
  # Compute the starting index of each run
  run_starts <- cumsum(c(1, head(r$lengths, -1)))
  # Only keep the starts of runs of length 3
  target_starts <- run_starts[true_stim_runs]
  middle_indices<-target_starts + 1
  middle_values<- run$smoothed_stim_numeric[middle_indices] #get the middle value of each run and the time that it occurred 
  mv_times<-run$datetime[middle_indices] #get the time of each middle value
  
  # Ok so now I have series of middle values (i.e. stim peaks) and each peak represents a stim pulse. I need to identify runs of stims at particular amplitudes, 
  #then go back and divide the run data up categorically. 
  mvd<-data.frame(middle_values,mv_times)
  mvd %>% mutate(stim_series=cumsum(middle_values != lag(middle_values, default = first (middle_values)+1)))->mvd
  mvd %>% group_by(stim_series) %>% summarize(n=n(),
                                              value=first(middle_values),
                                              stim_series_start=first(mv_times),
                                              stim_series_end=last(mv_times),
                                              stim_series_duration=stim_series_end-stim_series_start)->mvd_summary 

  # Okay NOW I can split the run data into these accordingly. I think. 
  
  ### Back to event grid data to link event start times with the intervals in the labchart data: 
  
  #filter out wake time on the event grid data and fix the time columns
  events_data %>% filter(Sleep!="Wake") -> sleeping_only
  
  sleeping_only%>%
    #mutate(stim_numeric=as.numeric(Stimulation))  %>% #convert stim to numeric
    mutate(parsed_start=as.POSIXct(start_time,format= "%H:%M:%OS")) %>% 
    mutate(start_time_only=as_hms(parsed_start))->sleeping_only
  
  #doing that added an arbitrary date so we have to fix it back to the actual procedure dates: 
  
  events_while_asleep <- sleeping_only %>%
    mutate(assigned_date = if_else(start_time_only > as_hms("18:00:00") &start_time_only < as_hms("23:59:59"), ref_date, ref_date + 1)) %>% 
    mutate(assigned_date = as_date(assigned_date)) %>% 
    mutate(datetime = as.POSIXct(paste(assigned_date, format(parsed_start, "%H:%M:%S")))) %>%
    filter(Event  %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                         "H. Obstructive", "H.Mixed"))
  
  
  
  #FIRST match the event start times with the run intervals identified in labchart: 
  matched <- fuzzy_left_join(
    events_while_asleep, labchart_runs,
    by = c("datetime" = "start_time", "datetime" = "end_time"),
    match_fun = list(`>=`, `<=`)
  )
  
  matched %>% filter(run_number==run_num)->matched_run_only
  
  ## NOW need to break it down and match on data from run number 1 only: 
  matched_stim_by_run<- fuzzy_left_join(
    matched_run_only, mvd_summary,
    by = c("datetime" = "stim_series_start", "datetime" = "stim_series_end"),
    match_fun = list(`>=`, `<=`)
  ) 
  
  #matched_stim_by_run now contains run-specific data:
  matched_stim_by_run %>% 
    group_by(value) %>% 
    summarise(n_events = n(),
              total_duration = sum(stim_series_duration, na.rm = TRUE))-> run_summary
  
  
  run_summary %>% 
    mutate(duration_hours=as.numeric(total_duration,"hours")) %>% 
    mutate(AHI=n_events/duration_hours)-> run_summary
  
  #bin by < values 
  run_summary %>% 
    # mutate(sub_therapeutic=ifelse(value<500,1,0)) %>%
    mutate(Stim500=ifelse(value>=500,1,0)) %>%
    mutate(Stim550=ifelse(value>=550,1,0)) %>%
    mutate(Stim600=ifelse(value>=600,1,0)) %>%
    mutate(Stim650=ifelse(value>=650,1,0)) -> run_summary
  
  
  run_summary %>% 
    mutate(duration_hours=as.numeric(total_duration,"hours")) %>% 
    mutate(AHI=n_events/duration_hours)-> run_summary
  
  #return(run_summary)
  
  ## Sensitivity analysis:
  
  #binning by each value:
  binning_vals<-c("Stim500","Stim550","Stim600","Stim650")
  binned_run_summaries<-vector(mode="list",length=length(binning_vals))
  
  printed<- for (col in binning_vals){
    run_summary %>% 
      group_by(.data[[col]]) %>% 
      summarise(n_events = sum(n_events),total_duration = sum(total_duration, na.rm = TRUE),
                duration_hours=as.numeric(total_duration,"hours"),
                calc_AHI=n_events/duration_hours) ->result
    #result -> binned_run_summaries[[col-3]]
    print(paste("Grouped by","", col))
    print(result) }
  #return(printed)
  
  #there isn't actually a stim duration for the "NA" in the run summary, so we need to calculate the unstimmed AHI within the run: 
  unstimmed_time<-labchart_runs$T_all[run_num]-sum(run_summary$total_duration, na.rm = TRUE)
  unstimmed_AHI<-run_summary$n_events[is.na(run_summary$value)]/as.numeric(unstimmed_time,"hours")
  return(list(unstimmed_AHI=unstimmed_AHI,printed=printed, run_summary=run_summary) )
  
  #return the unstimmed AHI, the printed binned run summaries, and the run summary itself
}

output<-parse_stimulation_runs(labchart_data=labchart,run_num=1)
output$unstimmed_AHI
