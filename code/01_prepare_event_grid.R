# Code to prepare event grid data 

#setup
rm(list=ls())
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(hms)
library(svDialogs)

##### 1. Event Grid Data #####################

## Read in event data in the form of a (scored) event grid out of Nox (make sure it's one with a 'sleep' column)
# eg_fp<-dlgOpen(
#   title = "Select the scored event grid file",
#   default=default_dir,
#   #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
#   multi = FALSE,
#   filters = dlgFilters[c("CSV files", "All files")])$res

#read in the event grid with the scored events 

eg_fp<-here("data/201-013_60-day_EventGrid_26-Jun-2025.csv")
event_grid<-read.csv(eg_fp) %>% clean_names()

#get the patient identifier off the event grid:
eg_filename<-basename(eg_fp)

#check the structure of the event grid:
str(event_grid)

#clean the event grid data (this will throw warnings about coercing factors to numeric, but that's okay):
event_grid %>%
  slice(-1) %>% 
  mutate(start_time=as_hms(start_time),
         end_time=as_hms(end_time),
         duration=as.numeric(duration))->clean_events

## Getting dates and times right 

#grab analysis start and end time off the nox event grid and prompt if it doesn't exist in the event grid::
analysis_start_time <- clean_events %>%
  filter(event == "Analysis Start") %>%
  pull(start_time) %>%
  first()

if (length(analysis_start_time) != 1 | !exists("analysis_start_time")) {
  stop("Analysis Start time not found in the event grid. Please provide a start time")
  st<-dlg_input(message="Please provide an analysis start time.")$res
  start_time <- as_hms(st)
}

analysis_end_time <- clean_events %>%
  filter(event == "Analysis End") %>%
  pull(end_time) %>%
  first()

if (length(analysis_end_time) != 1 | !exists("analysis_end_time")) {
  message("Analysis End time not found in the event grid. Please provide an end time")
  en<-dlg_input(message="Please provide an analysis end time.")$res
  analysis_end_time <- as_hms(en)
}


## CHECK the structure of the time and add dates IFF they dont exist already:
checkdate<-mdy_hms(clean_events$start_time[1]) #attempt to convert to mdy_hms format, will be NA if there's not a date in it.

# need to accommodate
if (is.na(checkdate)){ #check if the start time is in mdy_hms format
  
  #### if it's na, not in mdy_hms format, so we need to add a date column:
  st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
  analysis_start_date<-as.Date(st_dt) #convert to Date
  analysis_end_date<-analysis_start_date + days(1) #assume the same date for now
  
  clean_events <- clean_events %>%
    mutate(
      start_date = if_else(as.numeric(start_time) < 12*60*60, analysis_end_date, analysis_start_date),
      end_date = if_else(as.numeric(start_time) < 12*60*60, analysis_end_date, analysis_start_date)) %>%
    drop_na() #events before noon automatically assigned to next day 
  
  #put them together into a datetime: 
  analysis_start_datetime<- as.POSIXct(analysis_start_date) + as.numeric(analysis_start_time)
  analysis_end_datetime <- as.POSIXct(analysis_end_date) + as.numeric(analysis_end_time) } else {
  
  #######
  
  ### if it is NOT NA, that means it converted okay, but now i want to rename it to datetime and split into two
  clean_events <- clean_events %>%
    rename(start_datetime="start_time",
           end_datetime="end_datetime")%>%
    mutate(
      start_time = as_hms(start_datetime), 
      end_time = as_hms(end_datetime),
      start_date = as.Date(start_datetime), 
      end_date = as.Date (end_datetime) 
    ) %>%
    drop_na() #drop any rows with NA values in them
  
  #name out the analysis start and end datetimes:
  analysis_start_datetime<- first(clean_events$start_datetime)
  analysis_end_datetime <- last(clean_events$end_datetime)
  
}



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
AHI_whole_night

#this is within 0.1 of the AHI calculated in the event grid, so it seems to be working okay.

#calculate whole night ODI:
ODI_whole_night= all_events %>%
  filter(event =="Desat") %>%
  summarize(ODI=round(n()/total_sleep_time*3600,1)) #ODI in events per hour


### At this point, the event grid data is loaded and ready for analysis.
