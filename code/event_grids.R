## DISCRETE EVENT BASED DATA
# Code for looping over event grids and making plots for board meeting
# EVENT GRIDS NEED TO HAVE POSITION EVENTS AND SLEEP SET UP FOR START OF INTERVAL, and to be exported as CSVs:

#setup
rm(list=ls())
library(lubridate)
library(hms)
library(janitor)
library(tidyverse)
source(here("code/helper_fxns.R"))

#read in multiple event grids:
event_grid_mostrecent<-read.csv(here("data/Event Grid.csv")) %>% clean_names() %>%  slice(-1)
event_grid_baseline1 <-read.csv(here("data/Event Grid_baseline"))
event_grid_baseline2<-read.csv()

all_event_grids<-list(event_grid_mostrecent,event_grid_baseline1,event_grid_baseline2)

#clean the event grid data (this will throw warnings about coercing factors to numeric, but that's okay):
prep_event_grids<-function(event_grid){
event_grid %>%
  #slice(-1) %>% 
  mutate(start_time=mdy_hms(start_time),
         end_time=mdy_hms(end_time),
         duration=as.numeric(duration)) %>% 
  filter(sleep!="Wake")->raw_events

## Add position information

#mark position changes in separate column
raw_events %>% 
  mutate(position= case_when(event %in% c("Upright","Supine","Left","Right","Prone","Unknown") ~ event,
                             TRUE ~ NA)) %>%
  mutate(type=case_when(event %in% c("Apnea","A. Mixed","A. Obstructive","A. Central") ~ "apnea",
                        TRUE ~ "hypopnea")) -> raw_events
#fill down 
raw_events %>%
  fill(position,.direction="down")->raw_events 

#supine/nonsupine and rem/nonrem
raw_events %>% mutate(simple_position=case_when(position=="Supine" ~ "supine",TRUE ~ "nonsupine")) %>%
  mutate(simple_sleep=case_when(sleep=="REM" ~ "REM", TRUE ~ "nonREM"))-> raw_events

#filter by event:
sleep_epochs<-raw_events %>% 
  filter(event %in% c("N1","N2", "N3", "REM","Wake")) %>%
  mutate(duration=as.numeric(duration))  #duration in seconds

wake_epochs<-sleep_epochs %>% filter (event=="Wake")

mvmts<-raw_events %>% 
  filter(event %in% c("Movement","Spontaneous Arousal","Respiratory Arousal")) %>%
  mutate(start_time=as_hms(start_time)) 

all_events<-raw_events %>%
  filter(event %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                      "H. Obstructive", "H.Mixed")) %>%
  filter(sleep!="Wake") #double check on this with david...not sure how there can be events if it's 'wake'


all_events %>% 
  group_by(event)%>%
  summarize(n=n())
#now should be able to evaluate the TST:
total_sleep_time<-sleep_epochs %>%
  filter(event!="Wake") %>%
  summarize(TST=sum(duration)) %>%
  unlist() %>%
  unname() #total sleep time in seconds

tst_hours<-round(total_sleep_time/3600,1) #convert to hours


#parse by position 
events_by_position<-all_events %>% group_by(simple_position) %>% summarize(n=n())
events_by_position

#calculate sleep time in each position 
sleep_epochs %>%
  filter(event!="Wake") %>% group_by(simple_position) %>%
  summarize(TST=sum(duration)) %>%
  mutate(TST_hours=round(TST/3600,2))->sleep_by_position

positional<-merge(sleep_by_position,events_by_position) %>% 
  mutate(AHI=n/TST_hours)
positional


#parse by sleep stage 
events_by_stage<-all_events %>% group_by(simple_sleep) %>% summarize(n=n())
events_by_stage

#calculate sleep time in each stage 
sleep_epochs %>%
  filter(event!="Wake") %>% group_by(simple_sleep) %>%
  summarize(TST=sum(duration)) %>%
  mutate(TST_hours=round(TST/3600,2))->sleep_by_stage

sleepstage<-merge(sleep_by_stage,events_by_stage) %>% 
  mutate(AHI=n/TST_hours)
sleepstage

all_events->> all_events
events_by_position->>events_by_position
events_by_stage->>events_by_stage
sleepstage->>sleepstage
positional->>positional

}

#run the function on the most recent and save as named objects
prep_event_grids(event_grid_mostrecent)
all_events -> all_events_mostrecent
events_by_position -> events_by_position_mostrecent
events_by_stage -> events_by_stage_mostrecent
sleepstage -> sleepstage_mostrecent
positional -> positional_mostrecent

#now we know that's working here are some plots 

#overall AHI by position and sleep 
positional %>% 
  ggplot()+aes(simple_position,AHI,fill=simple_position) +geom_bar(stat="identity",)+theme_lunair()+theme(legend.position = "right")

sleepstage %>% 
  ggplot()+aes(simple_sleep,AHI,fill=simple_sleep) +geom_bar(stat="identity",)+theme_lunair()+theme(legend.position = "right")

#A vs H:
all_events %>% 
  ggplot()+aes(simple_position,fill=type) +geom_bar()+theme_lunair()+theme(legend.position = "right")

all_events %>% 
  ggplot()+aes(simple_sleep,fill=type) +geom_bar()+theme_lunair()+theme(legend.position = "right")

#make them proportions now:
all_events %>%
  ggplot()+aes(simple_position,fill=type) +geom_bar(position="fill")+theme_lunair()+theme(legend.position = "right")
all_events %>%
  ggplot()+aes(simple_sleep,fill=type) +geom_bar(position="fill")+theme_lunair()+theme(legend.position = "right")

