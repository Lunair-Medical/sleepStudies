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
  slice(-1)->clean_events

## Getting dates and times right 

#grab analysis start and end time off the nox event grid and prompt if it doesn't exist in the event grid::
start_time <- clean_events %>%
  filter(event == "Analysis Start") %>%
  pull(start_time) %>%
  first()

if (length(start_time) != 1) {
  stop("Analysis Start time not found in the event grid. Please provide a start time")
  st<-dlg_input(message="Please provide an analysis start time.")$res
  start_time <- as_hms(st)
}

end_time <- clean_events %>%
  filter(event == "Analysis End") %>%
  pull(end_time) %>%
  first()

if (length(end_time) != 1 | is.na(end_time)) {
  message("Analysis End time not found in the event grid. Please provide an end time")
  en<-dlg_input(message="Please provide an analysis end time.")$res
  end_time <- as_hms(en)
}


## CHECK the structure of the time and add dates IFF they dont exist already:
checkdate<-mdy_hms(clean_events$start_time[1]) #attempt to convert to mdy_hms format, will be NA if there's not a date in it.

# need to accommodate
if (is.na(checkdate)){ #check if the start time is in mdy_hms format
  
  #### if it's na, not in mdy_hms format, so we need to add a date column:
  st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
  start_date<-as.Date(st_dt) #convert to Date
  end_date<-start_date + days(1) #assume the same date for now
  
  clean_events <- clean_events %>%
    mutate(
      start_date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date),
      end_date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date)) %>%
    drop_na() #events before noon automatically assigned to next day 
  
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
  
  #put them together into a datetime: 
  analysis_start_datetime<- as.POSIXct(start_date) + as.numeric(start_time)
  analysis_end_datetime <- as.POSIXct(end_date) + as.numeric(end_time)
  
}

# 
# 
# #check if there's already a date column:
# if (!"date" %in% names(clean_events)) {
#   
#   #if not, create a date column based on user input:
#   st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
#   start_date<-as.Date(st_dt) #convert to Date
#   end_date<-start_date + days(1) #assume the same date for now
#   
#   clean_events <- clean_events %>%
#     mutate(
#       date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date))  #events before noon automatically assigned to next day 
# }
# 
# #if date already exists as a column, just ensure it's in the right format and concatenate:
# clean_events <- clean_events %>% mutate(
#   start_datetime = as.POSIXct(date) + as.numeric(start_time),
#   end_datetime = as.POSIXct(date) + as.numeric(end_time)
# )



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

#Run device log data script to get the prepared device log

## 

if (is.na(checkdate)){ #check if the start time is in mdy_hms format
  
  #if it's na, not in mdy_hms format, so we need to add a date column:
  st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
  start_date<-as.Date(st_dt) #convert to Date
  end_date<-start_date + days(1) #assume the same date for now
  
  clean_events <- event_grid %>%
    mutate(
      date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date)) %>%
    drop_na() #events before noon automatically assigned to next day 
  
  #if it is NOT NA, that means it converted okay, but now i want to split it out into two:
  clean_events <- event_grid %>%
    mutate(
      start_time = as_hms(start_time), #convert to hms
      end_time = as_hms(end_time),
      date = as.Date(start_time) #convert to hms
    ) %>%
    drop_na() #drop any rows with NA values in them
}


