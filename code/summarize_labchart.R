#read in the lab chart data pad export: 

rm(list=ls())
library(readxl)
library(lubridate)
library(tidyverse)
library(here)
library(hms)
library(data.table)

therapy_stim_level<-500 #what value is considered therapeutic, will vary by patient

#set the patient directory
patient_dir<-dlgOpen("Select the patient directory",
                     caption = "Select the patient directory",
                     default = "C:/Users/MegMcEachram/Lunair Medical/R&D - Documents/FiH Data/ECLIPSE 1 - Paraguay") 
set_here(patient_dir)

output_dir<-here(_insert path to summary folder) #sets the output filepath 
lcfp<- file path for the labchart data pad export

labchart_raw<-read_excel("C:/Users/MegMcEachram/Downloads/exported lab chart data pad from 60 day psg from 201 005.xlsx")
samples_in_block<-33861
block_start<-lubridate::ymd_hms(" 2025-05-18 20:54:19")
block_end<-block_start + seconds(samples_in_block)
second_timestep<-seq(block_start, block_end-seconds(1), by = "1 sec") #make a time series 


#clean up the labchart read in: 
labchart_raw%>% 
  slice(-c(1,2)) %>% #remove first rows that contain the sample info and units
  select(-c(...22,...23,...24,...25,...26)) -> labchart #remove random extra columns


labchart[1:samples_in_block,]->labchart #chop off empty rows at the end
labchart %>% 
  mutate(datetime = second_timestep) %>% #add the time series to the data
  mutate(stim_numeric=as.numeric(Stimulation))  %>% #convert stim to numeric
  mutate(time_only=hms::as_hms(format(datetime, "%H:%M:%S"))) %>% 
  mutate(date_only=as_date(format(datetime)))->labchart

## Have to fix the weird artifact where stim appears to be slowly ramping through time, so write code to go through and identify where there are long runs of nonzero numbers:
labchart$stim_numeric->all_stim_values
nonzero_logical<-all_stim_values != 0
r<-rle(nonzero_logical) #run length encoding to find runs of "true" (nonzero) and the length of those runs
long_nonzero_runs<-which(r$values==TRUE & r$lengths>3) #find the runs of nonzeros that are longer than 3, since actual stim bursts always come in 3.

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



# Define custom 30-second floored epoch function

floor_to_epoch <- function(time_hms, epoch_length = 30) {
  # Convert to total seconds since midnight
  secs <- hour(time_hms) * 3600 + minute(time_hms) * 60 + second(time_hms)
  
  # Floor to nearest lower epoch
  floored_secs <- floor(secs / epoch_length) * epoch_length
  
  # Return as hms
  as_hms(floored_secs)
}

epoch_start_times <- floor_to_epoch(labchart$time_only, 30)
labchart$epoch_start <- epoch_start_times
length(unique(labchart$epoch_start))

#how many epochs with stim on/off? First need to identify if the epoch had stim on at all during it:
lcdt<-as.data.table(labchart) #convert to data.table
lcdt[, stim_during_epoch := any(stim_binary > 0), by = epoch_start_times] #if stim is on at any point during epoch this is 'true'

lcdt %>% as.data.frame() -> labchart #convert back to data frame
labchart %>% group_by(stim_during_epoch) %>% 
  summarise(n_epochs = n_distinct(epoch_start)) -> stim_summary

## Align with the data table sleep epochs:

#what's the first time in the data from the event grid sleep epochs?
start_time=sleep_epochs$start_time[1]
start_date=min(labchart$date_only) #get the start date from the event grid data

start_index=first(which(labchart$epoch_start==start_time)) #find the index of the first row in the labchart data that matches the start time of the event grid data and assign that to be epoch 1.

labchart %>% 
  filter(epoch_start>=start_time | date_only > start_date) %>% 
  mutate(epoch_number = match(epoch_start, unique(epoch_start))) -> labchart#add an epoch number column to the labchart data

#simplify events of interest data 
event_cols<-c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
              "H. Obstructive", "H.Mixed","Desat") 

epoch_name_cols<-c("Start.Epoch", "End.Epoch")

#pivot events of interest wider:
events_of_interest %>% 
  pivot_wider(names_from = Event, values_from = Duration) %>%
  group_by(Start.Epoch) %>%
  select(any_of(c(event_cols, epoch_name_cols)))->eoi_wide
         
eoi_wide_by_epoch<-eoi_wide%>%
  group_by(across(all_of(epoch_name_cols))) %>%
  summarise(across(-any_of(epoch_name_cols), ~ sum(.x, na.rm = TRUE), .names = "sum_{col}")) 

#NOTE this is going to include some events that crossed epoch boundaries so the DURATIONS might not make a ton of sense

## Combine it all--stimulation information, sleep stage, and events of interest:
#merge sleep scoring with lab chart data series
scored_sleep<-merge(x=labchart,
                    y=dplyr::select(sleep_epochs,-c(stim,stim_numeric)),
                    by.x="epoch_start",
                    by.y="start_time") #merge the lab chart data with the sleep epochs data

#group by epoch to get a single observation per epoch to merge with events of interest:
ss_grouped<-scored_sleep %>% group_by(epoch_number) %>%
  summarise(start_time=first(time_only),
            end_time=last(time_only),
            stim_during_epoch=first(stim_during_epoch),
            stim_binary=max(stim_binary), #grabs max value from during that epoch
            stim_numeric=max(corrected_stim),
            sleep_stage=first(Event), #grabs first sleep stage but they should be the same for all 30 rows
            n_epochs=n()) %>% #count the number of epochs in each group
  ungroup() %>% 
  mutate(epoch_number=row_number()) #reassign epoch numbers


#merge scored sleep with events of interest
all_data <- left_join(ss_grouped, eoi_wide_by_epoch,
                  by=c("epoch_number"="Start.Epoch"))

#figure out what the 'actual' stim is (rounding to nearest 50 milli-amp increment)
round_to_50 <- function(x) {
  round(x / 50) * 50
}

round_to_50(394)

#apply to stim_numeric:
all_data %>% 
  mutate(smoothed_stim_numeric=round_to_50(stim_numeric)) %>% #round to nearest 50
  mutate(stim_therapeutic = case_when(smoothed_stim_numeric > therapy_stim_level ~ 1, #if stim is above the therapeutic level, set stim binary to 1
                                 TRUE ~ 0)) -> all_data #otherwise set to zero

#need new AHI columns:
new_AHI_columns<-c("sum_A. Mixed" ,"sum_A. Obstructive" ,
                   "sum_A. Central", "sum_Hypopnea"  ,"sum_H. Obstructive")   

#add a column that counts the number of AHI events in each epoch
all_data %>%
  mutate(AHI_sum = rowSums(across(any_of(new_AHI_columns), ~ !is.na(.x) & .x != 0))) -> all_data
  


#summarize and calculate number of events in each: 
all_data %>% filter (sleep_stage != "Wake") %>% group_by(stim_therapeutic) %>% 
  summarise(n_epochs = n(),
            n_AHI_events=sum(AHI_sum),
            n_ODI_events = sum(!is.na(sum_Desat)))-> summary_table

summary_table %>% 
  mutate(TST=paste(round(n_epochs*30/3600,1),"hours")) %>%
  mutate(AHI=round(n_AHI_events/(n_epochs*30/3600),1)) %>%
  mutate(ODI=round(n_ODI_events/(n_epochs*30/3600),1))
            
            
