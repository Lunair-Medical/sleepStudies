# Title: Summarize PSG study data
# Description: Code for parsing, summarizing, and generating figures and summary graphics for whole-night PSG data
#Takes in: labchart data (waveforms) and event grid (discrete events) 
#Author: Meg McEachran
# Date Last Updated: 7/9/2025

#### 1. Initial Setup
rm(list=ls())

## Load necessary libraries
library(tidyverse)
library(here)
library(readxl)
library(ggmosaic)
library(lubridate)
library(ggpubr)
library(svDialogs)
library(data.table)
library(janitor)
library(hms)
library(officer)

## Source
source(here("code/helper_fxns.R")) #load helper functions

## set paths
#output_path<-here("figures")
default_dir<-here("data/")

#have to add datetime
input<-dlg_input(
  message = "Enter the date of the PSG in the format YYYY-MM-DD")$res
input_char<-as.character(input)
start_date<-as.Date(input_char) #first date of the PSG
end_date <- start_date + days(1) #end date is the next day

# # Manually add the therapy start and end times: #th
# therapy_start<-as.POSIXct() #as posixct
# therapy_end<- #as posixct
# 
# #dialog box to select where the summary figures will be saved: 
# output_path<-dlgDir(
#   title = "Select the output directory for figures",
#   default=patient_dir)$res

## graphic settings
lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2","#7CCDF5", "#397b96","#833080")

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
# 
# ## value settings
# stim_threshold<-200 #threshold value for the 'on'/off designation for stim

#### 2. Read in data 
## read in event data in the form of a (scored) event grid out of Nox (make sure it's one with a 'sleep' column)
eg_fp<-dlgOpen(
  title = "Select the scored event grid file",
  default=default_dir,
  #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
  multi = FALSE,
  filters = dlgFilters[c("CSV files", "All files")])$res
## Event-based data

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


clean_events <- clean_events %>%
  mutate(
    date = if_else(as.numeric(start_time) < 12*60*60, end_date, start_date), #events before noon automatically assigned to next day 
    start_datetime = as.POSIXct(date) + as.numeric(start_time),
    end_datetime = as.POSIXct(date) + as.numeric(end_time)
  )

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


# #set the default directory to the directory of the file selected:
# patient_dir= str_extract(eg_fp, ".*[\\\\/]")

## read in the waveform data in the form of data that was exported from Labchart as raw data
waveform_fp<-dlgOpen(
  title = "Select the labchart .txt file",
  default=default_dir,
  #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
  multi = FALSE,
  filters = dlgFilters[c("CSV files", "All files")])$res

# #make a con for opening the lab chart text file since it's so big, read it in as binary:
# con<-file(filepath,"rb")
# all_data<-readBin(con=con,what = "raw",n=1e06) #read in the first 1 million bytes
# head_all_data<-all_data[1:1000]
# rawToChar(head_all_data)
# whole_thing<-read_tsv(wfp)

## Read data and downsample:

#use the helper function to parse .txt into a data.table: 
all_data_dt<-parse_labchart_txt(waveform_fp,downsample_rate = 5)
#head(all_data_dt)

## clean data:


#numeric columns: 
num_cols<-setdiff(colnames(all_data_dt),c("date","sec_since_midnight")) #all columns except date time
cols_to_zero<-c("stimulation","phasic_amplitude")
cols_to_round<-c("pulse","saturation","heart_rate")

#make changes to the columns:
all_data_dt[,(num_cols) := lapply(.SD,as.numeric), .SDcols = num_cols] #make numeric 
all_data_dt[,(cols_to_round) := lapply(.SD,round), .SDcols = cols_to_round] #round
all_data_dt[, (cols_to_zero) := lapply(.SD, function(x) fifelse(x<0,0,x)), .SDcols = cols_to_zero] #zero out columns

all_data_dt %>% drop_na() -> all_data_dt #drop any rows with NA values

#put together so you also have datetime: 
all_data_dt[, date_char := as.character(date)]
all_data_dt[, datetime := as.POSIXct(
  paste(date_char, sec_since_midnight),
  format = "%m/%d/%Y %I:%M:%OS %p",
  tz = "UTC"
)]

# convert date from a character string:
all_data_dt[, date := as.IDate(date, format = "%m/%d/%Y")]    

# convert time from a character string:
all_data_dt[, time := as_hms(sec_since_midnight)]    

# #check the structure of dt: 
# str(all_data_dt)

##for now, just grab half of the time so it's not such a big file:
nhalf<-floor(nrow(all_data_dt)/2)
half_data<-all_data_dt[1:nhalf, ] 


#### STRATIFIED AHI CALCULATION:

## Create the strata in the waveform data:
#right now, what are the times on the data?
min(all_data_dt$datetime)
max(all_data_dt$datetime)

#get a look at the overall shape of the night: 
df<- as.data.frame(all_data_dt) 
df %>% ggplot(aes(x=datetime,y=phasic_amplitude)) +
  geom_line() +
  labs(x="Time", y="Phasic Amplitude") +
  theme_lunair()

#add a tharapy enabled column based on the datetime stamps of when therapy enabled session started 
#This is a manual determination based on when the 'efficacy portion' of the night started 
all_data_dt[ther_enabl := fcase(
  datetime >= therapy_start & datetime <= therapy_end, TRUE,
  default = FALSE
)] #make a therapy enabled column

#Add a categorical variable:
all_data_dt[,stratum := fcase (
  phasic_amplitude == 0, "off",
  phasic_amplitude > 0 & stimulation > 0, "stimulating",
  phasic_amplitude > 0 & stimulation == 0 & ther_enabl==TRUE, "therapy_enabled"
)]

#can i plot it?
all_data_dt %>% 
  ggplot(aes(x=stratum, y=saturation, color=stratum)) +
geom_boxplot()

#seems to be working okay.

# Calculate amount of time in each strata in all_data_dt:

# Make sure your data is sorted by timestamp
setorder(all_data_dt, datetime)

# Calculate time difference to next timestamp (in seconds)
all_data_dt[, duration := as.numeric(shift(datetime, type = "lead") - datetime, units = "secs")]

# The last row will have NA for duration; you may want to set it to 0 or remove it before summing
all_data_dt[is.na(duration), duration := 0]

# filter out rows in all_data_dt that came from wake time: 
for (i in 1:nrow(wake_epochs)){
  start_time <- wake_epochs$start_datetime[i]
  end_time <- wake_epochs$end_datetime[i]
  
  # Find the rows in all_data_dt that fall within the start and end times
  all_data_dt[datetime >= start_time & datetime <= end_time, sleep_stage := "wake"]
}

#set the other rows to asleep:
all_data_dt[is.na(sleep_stage), sleep_stage := "sleep"] #set all other rows to asleep

#examine 
all_data_dt %>% 
  group_by(sleep_stage) %>% 
  summarize(total_time_hr = sum(duration)/3600) #total time in each sleep stage

# filter out the wake data:
sleep_data_dt <- all_data_dt %>%
  filter(sleep_stage == "sleep") #keep only sleep data

# Sum durations by stratum
strata_time <- sleep_data_dt[, .(total_time_secs = sum(duration), total_time_hr=sum(duration)/3600), by = stratum]


## Calculate an AHI stratified by what's happening with the stimulation:

## Have to match up events with the waveform data
events_dt <- as.data.table(filter(all_events,event!="Desat")) #filter out desats for AHI calculation, already has 'wake' removed

#add empty columns to store summary values:
sum_cols<-c("max_pa","min_pa","mean_stim","max_stim","min_stim","mean_sat","max_sat","min_sat","strat_at_start")
events_dt[, (sum_cols) := NA_real_]

#loop over the events dt and calculate summary values for each:
for (i in 1:nrow(events_dt)){
  
  start_time <- events_dt$start_datetime[i]
  end_time <- events_dt$end_datetime[i]
  
  # Find the rows in all_data_dt that fall within the start and end times
  relevant_rows <- sleep_data_dt[datetime >= start_time & datetime <= end_time]
  
  #for the events that are outside the analysis window, keep those as NA:
  if (nrow(relevant_rows) == 0) {
    events_dt[i, sum_cols] <- NA
    next #skip to the next iteration if no relevant rows found
    
  } else {
    
  #calculate summary stats for each event:
  mean(relevant_rows$phasic_amplitude, na.rm = TRUE) -> events_dt$mean_pa[i]
  min(relevant_rows$phasic_amplitude, na.rm = TRUE) -> events_dt$min_pa[i]
  max(relevant_rows$phasic_amplitude, na.rm = TRUE) -> events_dt$max_pa[i]
  
  mean(relevant_rows$stimulation, na.rm = TRUE) -> events_dt$mean_stim[i]
  min(relevant_rows$stimulation, na.rm = TRUE) -> events_dt$min_stim[i]
  max(relevant_rows$stimulation, na.rm = TRUE) -> events_dt$max_stim[i]
  
  mean(relevant_rows$saturation, na.rm = TRUE) -> events_dt$mean_sat[i]
  min(relevant_rows$saturation, na.rm = TRUE) -> events_dt$min_sat[i]
  max(relevant_rows$saturation, na.rm = TRUE) -> events_dt$max_sat[i]
  
  #what was the status of stimulation when the event was starting?
  first(relevant_rows$stratum) -> events_dt$strat_at_start[i]}
}


#summarize by strata:
events_dt %>%
  group_by(strat_at_start) %>%
  summarize(
    n_events = n()) -> events_by_strata

events_by_strata

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


#make and save a pretty summary table: 
ordered_table<-AHI_by_strata %>%
  mutate(strat_at_start = recode(strat_at_start,
         "off" = "Off",
         "stimulating" = "Stimulating",
         "therapy_enabled" = "Therapy Enabled")) %>%
  rename(Status="strat_at_start") %>%
  rename(`# events`="n_events") %>%
  mutate(total_time_hr=round(total_time_hr,1)) %>%
  rename(`Total time (hr)`="total_time_hr") %>%
  select(!("total_time_secs")) 

#add a row for the whole night AHI:
ordered_table[nrow(ordered_table)+1,] <- NA
ordered_table$Status[nrow(ordered_table)] <- "Whole Night"
ordered_table$`# events`[nrow(ordered_table)] <- sum(events_by_strata$n_events)
ordered_table$`Total time (hr)`[nrow(ordered_table)] <- tst_hours
ordered_table$AHI[nrow(ordered_table)] <- AHI_whole_night$AHI

ordered_table

#make a pretty table with ggtexttable
ordered_table%>%
  ggtexttable(rows= NULL,
            #  align="lll",
              theme = ttheme("blank",
                             tbody.style = tbody_style(color=lunair_palette[3],
                                                       fill = "white"),
                                                       #hjust = as.vector(matrix(c(rep(3,12)), ncol = 4, nrow = nrow(df), byrow = TRUE))),
                             colnames.style = colnames_style(color=lunair_palette[2],
                                                             fill = "white",
                                                             size = 14,
                                                             face = "bold"))) ->pretty.table



#make a plot
summary_graph<-ordered_table %>% 
  ggplot(aes(Status,AHI)) +
  geom_bar(stat="identity",color=lunair_palette[3],fill = lunair_palette[3],width = 0.5) +
    geom_text(aes(label=round(AHI,1)), vjust=-0.5,size=8) +
    labs(x="Stimulation", y="AHI (Events/hr)") +
    ylim(0,max(ordered_table$AHI)*1.1)+
    theme_lunair()+
    theme(legend.position = "none",
          axis.text.x=element_text(hjust=0.5)) +
    scale_x_discrete(labels=c("Off","Stimulating",
                              "Therapy\nEnabled", #make the label on two rows
                              "Whole\nNight") #make the label on two rows
                     )-> summary_graph
summary_graph
  
#write to a doc:
doc<-read_docx()
target_name<-gsub(eg_filename,pattern="EventGrid",replacement="summary")
target_name<-gsub(target_name,pattern="csv",replacement="docx")
path<-here("reports")
target_path<-file.path(path,target_name)
doc <- doc %>%
  body_add_gg(pretty.table) %>% 
  body_add_gg(summary_graph)

#save the doc:
print(doc,target = target_path)
