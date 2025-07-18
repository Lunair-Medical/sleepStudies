#the purpose of this script is to check PSG event grids for 3% desat scored events
#originally created by Grace Burkholder, 17 July 2025

library(janitor)
library(tidyverse)
library(dplyr)
library(readr)

# read in the csv desat event grid
rawdata<-read.csv("C:/Users/GraceBurkholder/OneDrive - Lunair Medical/Documents/Repositories/Data/201-010_EventGrid_60-dayPSG_05212025.csv")

# clean the column names
clean_names(rawdata)->rawdata

rawdata<-rawdata[-1,]

# Add a row number for unique identification
rawdata <- rawdata %>% mutate(row_id = row_number())

#filter for just desat events and making numeric
desats <- rawdata %>%
  filter(event %in% (c("Desat")))
desats$sp_o2_min<-as.numeric(desats$sp_o2_min)
desats$sp_o2_max<-as.numeric(desats$sp_o2_max)

#finding difference between min and max SpO2 values and flagging if they are 3% desats
desats$diffSpO2<-(desats$sp_o2_max)-(desats$sp_o2_min)
desats$scoring4<-desats$diffSpO2>3 #true if the desat is >= 4; false if desat is < 4 (ie, 3% scoring may have been used)


#filter to just respiratory events
respevents <- rawdata %>%
  filter(event %in% (c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea",
                       "H. Obstructive", "H.Mixed")))

respevents

# Find overlapping events
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

rawdata$diffSpO2 <-NA_real_

#for every row in desats:
for(j in seq_len(nrow(desats))) {
  
  #put the desat value into rawdata
  rawdata$diffSpO2[rawdata$row_id == desats$row_id[j]] <- desats$diffSpO2[j]
}

# #default a keep column to true so that all the other events will be kept
rawdata$overlapsmaybe<-NA_character_
 
# For each row in overlaps:
 for(i in seq_len(nrow(overlaps))) {
   # Get the IDs for this overlap pair
   id1 <- overlaps$row_id1[i]
   id2 <- overlaps$row_id2[i]
   
   # Mark "true" for the row matching row_id1
   rawdata$overlapsmaybe[rawdata$row_id == id1] <- "keep"
   
   # Mark "false" for the row matching row_id2
  rawdata$overlapsmaybe[rawdata$row_id == id2] <- "discard"
   
# }
# 
# rawdata$keep<-NA
# 
# rawdata %>%
#   mutate(keep= case_when(rawdata$diffSpO2<4 ~ FALSE,
#                          is.na(rawdata$diffSpO2) ~ TRUE
#                         (rawdata$overlapsmaybe=="discard") ~ FALSE,
#                         is.na(rawdata$overlapsmaybe) ~ TRUE
#                          TRUE ~ TRUE
#                         )
#         )

  #  desats$diffSpO2>3) & !(rawdata$row_id %in% overlaps) 
