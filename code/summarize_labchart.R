#read in the lab chart data pad export: 

rm(list=ls())
library(readxl)
library(lubridate)
library(tidyverse)
library(here)
library(hms)

therapy_stim_level<-325 #what value is considered therapeutic, will vary by patient

#set the patient directory
patient_dir<-dlgOpen("Select the patient directory"),
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
  mutate(time = second_timestep) %>%
  mutate(stim_numeric=as.numeric(Stimulation)) -> labchart #add the time series to the data frame

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
labchart %>% ggplot() + aes(x=time,y=corrected_stim) + geom_line()

#NOW I can add stim_binary:
labchart %>% 
  mutate(stim_binary = case_when(corrected_stim !=0 ~ 1, 
                              TRUE ~ 0)) -> labchart 


#add an epoch column 
labchart %>% 
  mutate(time_only=hms::as_hms(format(time, "%H:%M:%S")))->labchart

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

#figure out what the 'actual' stim is (rounding to nearest 50 milli-amp increment)
# Find runs of nonzero values of exactly length 3
run_indices <- which(r$values == TRUE & r$lengths == 3)

# Convert to positions in x
ends <- cumsum(r$lengths)
starts <- ends - r$lengths + 1

# Middle index is the second value in each 3-length run
middle_indices <- starts[run_indices] + 1

# Get the actual values
middle_values <- x[middle_indices]
print(middle_values)
# [1] 2.2 5.5