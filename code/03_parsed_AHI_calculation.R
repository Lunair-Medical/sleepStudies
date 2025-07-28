## 03. Code to estimate AHI for different phases of the night
## Designed to read in the processed device logs and processed Nox event grids 
# 7/15/25 last updated 

# set up and load libraries 
rm(list=ls())

library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)
library(data.table)      # for data.table operations
library(hms)
library(ggpubr)

lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2","#7CCDF5", "#397b96","#833080")

#Read in the prepared event grid .rds and the prepared device log .rds:


####
# 12. Match events to the device/waveform data
####

events_dt <- as.data.table(filter(all_events,event!="Desat")) #filter out desats for AHI calculation, already has 'wake' removed

#add empty columns to store summary values for the stimulation during that event and what stratum you're in when the event starts: 
sum_cols<-c("mean_stim","max_stim","min_stim","therapy_enabled","stim_during_te","stim_val")
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
    events_dt$stim_val[i] <- (first(relevant_rows$stim_active)) #stim amp value 
    events_dt$stim_during_te[i] <- if_else(
      sum(relevant_rows$stim_during_te)>nrow(relevant_rows)/2, T,F #if more than half of the rows in the event are stimulating, then it's a 'stimulating' event
    )
  }
}

#several rows at the end of events_dt are NA because therapy sessions stopped before end of the study (and therefore aren't in 'sleeping' bc it's filtered to the device rows i kept):
#so replace those NAs: 
events_dt %>%
  mutate(across(c("mean_stim","min_stim","max_stim","stim_val","stim_during_te","therapy_enabled"), ~ replace_na(., 0))) -> events_dt #replace NAs with 0 for stim cols


####
# 13. Calculate stratified AHI:
####

## Summarize by different conditions durations by stratum

#summarize by stim strata:
events_dt %>%
  group_by(stim_during_te) %>%
  summarize(n_events = n()) -> events_by_stim_strata
events_by_stim_strata

#summarize by therapy enabled strata:
events_dt %>%
  group_by(therapy_enabled) %>%
  summarize(n_events = n()) -> events_by_therapy_strata
events_by_therapy_strata

#calculate time spent in each stim category
stim_during_te_time <- sleeping %>%
  filter(date_mdy>=analysis_start_datetime) %>%
  group_by(stim_during_te) %>%
  summarize(total_time_secs = sum(duration), total_time_hr = sum(duration)/3600) -> stim_cat_time
stim_during_te_time

#calculate time spent in each therapy category:
te_strata_time <- sleeping %>%
  filter(date_mdy>=analysis_start_datetime)%>% #filter out the log algo row that's many days ahead of the log prog rows 
  group_by(therapy_enabled) %>%
  summarize(total_time_secs = sum(duration), total_time_hr = sum(duration)/3600) #total time in each therapy enabled stratum
te_strata_time

#how are we doing during therapy enabled stimulation?
AHI_by_te <- merge(
  x= te_strata_time,
  y=events_by_therapy_strata,by="therapy_enabled")

AHI_by_te<- AHI_by_te %>%
  mutate(AHI=round(n_events/total_time_hr,1)) #calculate AHI in events per hour
AHI_by_te
AHI_te<-AHI_by_te %>% filter(therapy_enabled) %>% pull(AHI)

#how are we doing during stimulation during therapy time? 

#calculate an AHI by stim_cat strata
AHI_by_stim <- merge(
  x= stim_during_te_time,
  y=events_by_stim_strata,by="stim_during_te")

AHI_by_stim %>%
  mutate(AHI=round(n_events/total_time_hr,1)) -> AHI_by_stim
AHI_by_stim
AHI_stim<-AHI_by_stim %>% filter(stim_during_te) %>% pull(AHI)

#All the AHI values:
AHI_whole_night
AHI_te
AHI_stim

#put them together into a summary table:
AHI_summary <- data.frame(
  Status = c("Whole Night", "Therapy-Enabled", "Stimulating"),
  AHI = c(AHI_whole_night$AHI, AHI_te, AHI_stim)
)
AHI_summary %>% ggtexttable(rows= NULL,
            #  align="lll",
            theme = ttheme("blank",
                           tbody.style = tbody_style(color=lunair_palette[3],
                                                     fill = "white",
                                                     hjust = as.vector(matrix(c(rep(0,8)), ncol = 2, nrow = 4, byrow = TRUE)),
                                                     x=0.1),
                           colnames.style = colnames_style(color=lunair_palette[2],
                                                           fill = "white",
                                                           size = 14,
                                                           face = "bold"))) ->pretty.table
pretty.table <- set_table_properties(pretty.table, layout = "fixed", width = 1)  # width = 1 means 100%

pretty.table

grid::grid.newpage()
grid::grid.draw(pretty.table)

#summarize by sleep stage:
events_dt %>% 
  group_by(sleep) %>% 
  summarize(n_events=n())
#depth and length of apneas etc.
