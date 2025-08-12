#This is a script to read in and parse the roll pauses that are not indicated by a LogPositionChange in the device log, but which nonetheless occur.
#These rolls have to be manually identified by looking at the Nox + the marker file.
#Ghost rolls are rolls that are 1) not identified in the device log, and 2) are the same duration as the roll pause, 3) are apparent in the Nox ECG. 
#We may have ghost rolls that are not apparent in the stim trace because of bluetooth dropouts. 

#This script should be run in conjunction with parsed_AHI_calculation.R

#setup
library(tidyverse)
library(here)
library(janitor)
library(data.table)

#manually enter start and end date: 
end_date<-as.Date("2025-06-26") 
start_date<-as.Date("2025-06-25") 

#read in manually identified ghost rolls
#So far these are just those I identified during the therapy enabled period for px 13 on 6/25/25-6/26/25
ghost_rolls<-read.csv(here("data/201-013-60-day_ghost_rolls.csv")) %>% 
  mutate(roll_pause_numeric=duration/60, #convert to minutes
         start_time=as.ITime(start_time), 
         end_time=as.ITime(end_time),
         st_dt=if_else(start_time<=as.ITime("11:59:59"),end_date,start_date),
         end_dt=if_else(end_time<=as.ITime("11:59:59"),end_date,start_date),
         date_mdy=as.POSIXct(st_dt) + as.numeric(start_time), #this is the start time, named to match up with roll_pauses df 
         pause_end=as.POSIXct(end_dt)+as.numeric(end_time)) # this is the end time, named to match up with roll_pauses df

#select down only to columns in roll_pause
ghost_rolls<-ghost_rolls %>% select( any_of(colnames(roll_pauses)))
