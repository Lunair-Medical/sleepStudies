#this script will check for 3% desats, check for overlapping events, and remove events that are associated with either of those mistakes
#originally created by Grace Burkholder on 18 July 2025

library(janitor)
library(tidyverse)
library(dplyr)
library(readr)

# read in the csv desat event grid
rawdata<-read.csv("C:/Users/GraceBurkholder/OneDrive - Lunair Medical/Documents/Repositories/Data/201-010_EventGrid_60-dayPSG_05212025.csv")

# clean the column names
clean_names(rawdata)->rawdata

rawdata<-rawdata[-1,]
rawdata <- rawdata[, !(names(rawdata) %in% "x")]

# Add a row number for unique identification and add a diff SpO2 column
rawdata <- rawdata %>% mutate(row_id = row_number())
rawdata$sp_o2_max<-as.numeric(rawdata$sp_o2_max)
rawdata$sp_o2_min<-as.numeric(rawdata$sp_o2_min)
rawdata$diffSpO2<-NA
rawdata$keep<-NA


##look for 3% desats
#filter the dataframe to only look at desats and hypopneas
allevents <- rawdata %>%
  filter(event %in% (c("Hypopnea", "H. Obstructive", "H.Mixed", "Desat")))

#for desat rows, find the desat value and store it in a new column
allevents$diffSpO2[allevents$event == "Desat"] <- allevents$sp_o2_max[rawdata$event == "Desat"] - allevents$sp_o2_min[rawdata$event == "Desat"]


##filter the dataframe to only look at apneas and hypopneas
respevents <- rawdata %>%
  filter(event %in% (c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea",
                       "H. Obstructive", "H.Mixed")))

##check that the apneas/hypopneas don't have any overlaps
overlaps <- expand.grid(row_id1 = respevents$row_id, row_id2 = respevents$row_id) %>%
  filter(row_id1 < row_id2) %>%
  left_join(respevents, by = c("row_id1" = "row_id")) %>%
  left_join(respevents, by = c("row_id2" = "row_id"), suffix = c("_1", "_2")) %>%
  filter(
    (start_time_2 <= end_time_1) & (start_time_2 >= start_time_1))   %>% 
  select(
    row_id1, event_1, start_time_1, end_time_1,
    row_id2, event_2, start_time_2, end_time_2, 
  )
overlaps

#in the hyp/desat data frame, only keep the rows if their row_id doesn't appear in row_id2 of overlaps
allevents <- allevents[!(allevents$row_id %in% overlaps$row_id2), ]

##----------------------Stuff works above this line----------------------------------------------##

##get rid of any apneas or hypopneas that come immediately before a 3% desat
#find the indices of all the hypopneas - will be checked for a following 3% desat
events_index = which(allevents$event == "Hypopnea" , "H. Obstructive" , "H. Mixed")
for (i in 1:length(events_index)) {
  index = events_index[i]
  event = allevents[index,]
  desatafter = first(which(allevents$event=="Desat") & allevents$start_time > allevents$end_time)
}
