# Title: Summarize PSG study data
# Description: Code for parsing, summarizing, and generating figures and summary graphics for whole-night PSG data
# Author: Meg McEachran
# Date Last Updated: 6/9/2025

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
## Source
source(here("code/helper_fxns.R")) #load helper functions

## set paths
#output_path<-here("figures")
default_dir<-"C:/Users/MegMcEachram/Downloads"


# 
# #dialog box to select where the summary figures will be saved: 
# output_path<-dlgDir(
#   title = "Select the output directory for figures",
#   default=patient_dir)$res

## graphic settings
lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2", "#397b96")

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

## value settings
stim_threshold<-200 #threshold value for the 'on'/off designation for stim

#### 2. Read in data 
## read in event data in the form of a (scored) event grid out of Nox
fp<-dlgOpen(
  title = "Select the scored event grid file",
  default=default_dir,
  #default = here("data/original/0513_board_meeting/Scored-201-010-30Day_Event Grid-Stim.csv"),
  multi = FALSE,
  filters = dlgFilters[c("CSV files", "All files")])$res


#set the default directory to the directory of the file selected:
patient_dir= str_extract(fp, ".*[\\\\/]")

## read in the waveform data in the form of data that was exported from Labchart as raw data
filepath<-dlgOpen(
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
all_data_dt<-parse_labchart_txt(filepath,downsample_rate = 5)
head(all_data_dt)

## clean data:

# parse date: 
all_data_dt[, date := as.IDate(date_char, format = "%m/%d/%Y")]    

#numeric columns: 
num_cols<-setdiff(colnames(all_data_dt),c("date_char","date")) #all columns except date and time
cols_to_zero<-c("stimulation","phasic_amplitude")
cols_to_round<-c("pulse","saturation","heart_rate")

#make changes to the columns:
all_data_dt[,(num_cols) := lapply(.SD,as.numeric), .SDcols = num_cols] #make numeric 
all_data_dt[,(cols_to_round) := lapply(.SD,round), .SDcols = cols_to_round] #round
all_data_dt[, (cols_to_zero) := lapply(.SD, function(x) fifelse(abs(x)<1,0,x)), .SDcols = cols_to_zero] #zero out columns

#check the structure of dt: 
str(all_data_dt)

#convert time 


### DATA ANALYSIS 

#read in data
data<-read.csv(fp)

#clean and rename data
data %>%
  slice(-1) %>% # remove first row
  rename(
    start_time = "Start.Time",
    end_time = "End.Time",
    phasic_amplitude = "Phasic.Amplitude..max.", 
    stim="Stimulation..max.") %>%
  mutate(amp_factor=as.factor(as.character(phasic_amplitude))) %>%
  mutate(Event=as.factor(as.character(Event))) %>%
  mutate(time=as.POSIXct(str_extract(start_time,pattern="^[^\\.]+"),format="%H:%M:%S")) %>% #this will add a date; disregard the date here
  mutate(Duration=as.numeric(Duration)) %>%
  mutate(Start.Epoch=as.numeric(Start.Epoch)) %>% 
  mutate(End.Epoch=as.numeric(End.Epoch))-> data

#convert stim strength to numeric values: 
data$stim_numeric<-as.numeric(data$stim)
hist(data$stim_numeric)
max(data$stim_numeric)

#look at the stim trace thru time:
data  %>%  ggplot() + aes(x=time, y=stim_numeric) + geom_line()

#fix stim trace to on/off based on threshold value:
data %>% 
  mutate(cat_stim=case_when(stim_numeric > stim_threshold ~ "On",
                            stim_numeric <= stim_threshold ~ "Off",
                            TRUE ~ "No Change")) -> data

data %>% group_by(cat_stim) %>% summarize(n=n())

#add a column for hypopnic/apnic events of any kind:
data %>% 
  
  mutate(AHI_events=case_when(Event %in% c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea", #general hyp and ap
                                           "H. Obstructive", "H.Mixed") ~ 1,
                              TRUE ~ 0)) %>%
  mutate(ODI_events=case_when(Event == "Desat" ~ 1,
                              TRUE ~ 0)) %>%
  mutate(arousals=case_when(Event %in% c("Spontaneous Arousal", "Movement") ~ 1,
                            TRUE ~ 0))-> data


# filter on sleep epochs
sleep_epochs<-data %>% 
  filter(Event %in% c("N1","N2", "N3", "REM","Wake")) %>%
  mutate(start_time=as_hms(start_time)) %>%
  mutate(end_time=as_hms(end_time))

sleep_epochs%>%
  group_by(Event) %>% 
  summarize(time=paste(round(sum(Duration)/3600,1), "hours")) 

## AHI and ODI calculation

# First calculate the total number of events for stim on/stim off
data %>% 
  filter(AHI_events == 1|ODI_events ==1)->events_of_interest

events_of_interest%>% 
  group_by(cat_stim) %>% 
  summarize(total_AHI_events=sum(AHI_events)) -> summary_table

#Then calculate total sleep time with stim on/stim off
TST_on<-sleep_epochs %>% 
  filter(cat_stim=="On") %>% 
  summarize(TST=round(sum(Duration)),
            TST_hr=round(sum(Duration)/3600,1))

TST_off<-sleep_epochs %>%
  filter(cat_stim=="Off") %>%
  summarize(TST=round(sum(Duration)),
            TST_hr=round(sum(Duration)/3600,1))


summary_table %>%
  mutate(TST=case_when(cat_stim=="On" ~ TST_on$TST_hr,
                       cat_stim=="Off" ~ TST_off$TST_hr)) %>% 
  mutate(AHI_score=round(total_AHI_events/TST,1))->summary_table
summary_table 

#graph delta AHI
summary_table %>% ggplot(aes(x=sort(cat_stim), y=AHI_score,fill=cat_stim)) +
  geom_bar(stat="identity" ) +
  geom_text(aes(label=round(AHI_score,1)), vjust=-0.5,size=8) +
  labs(x="Stimulation", y="Apnea-Hypopnea Index (Events/hr)") +
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3])) +
  theme_lunair()+
  theme(legend.position = "none") -> summary_graph
summary_graph


ggsave(summary_graph, filename=paste0(output_path,"/AHI_summary_graph.png"), width=8, height=9)



### ODI ###

#Because of circulatory delay, we need to offset the ODI calculations by 1 epoch (30s) to approximate this:

#Identify whether stim was on or off on the epoch immediately prior to the desat
sleep_epochs$prior_to_desat<-NA #empty column
for (i in 2 : length( sleep_epochs$prior_to_desat)){
  sleep_epochs$prior_to_desat[i]<-sleep_epochs$cat_stim[i-1]
}

#calculate how many desats with stim on/off 
desat_summary <- data %>% group_by(prior_to_desat) %>%
  summarize(n_desats=sum(ODI_events), mean_dur_desats=mean(Duration)) 

desat_summary %>% 
  mutate(ODI=case_when(prior_to_desat=="On" ~ round(n_desats/TST_on$TST_hr,1),
                       prior_to_desat=="Off" ~ round(n_desats/TST_off$TST_hr,1))) %>% 
  filter(!is.na(prior_to_desat))-> desat_summary

desat_summary %>% 
  ggplot(aes(x=sort(prior_to_desat), y=ODI,fill=prior_to_desat)) +
  geom_bar(stat="identity",)+
  geom_text(aes(label=round(ODI,1)), vjust=-0.5,size=8) +
  labs(x="Stimulation", y="Oxygen Desaturation Index (Events/hr)") +
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3])) +
  theme_lunair() ->desat_graph
desat_graph
ggsave(desat_graph, filename=paste0(output_path,"/ODI_summary_graph.png"), width=8, height=9)

##### Movement and arousal #####

#calculate how many movement events with stim on/off
mvmt_summary <- data %>% group_by(cat_stim) %>%
  summarize(all_arousals=sum(arousals),
            mvmts=sum(Event=="Movement"),
            resp.arousal=sum(Event=="Respiratory Arousal"),
            spont.arousal=sum(Event=="Spontaneous Arousal"))
mvmt_summary

mvmt_summary %>% 
  mutate(arousal_index=case_when(cat_stim=="On" ~ round(all_arousals/TST_on$TST_hr,1),
                                 cat_stim=="Off" ~ round(all_arousals/TST_off$TST_hr,1))) ->mvmt_summary


#graph delta arousals
mvmt_summary %>% ggplot(aes(x=sort(cat_stim), y=arousal_index, fill = cat_stim)) +
  geom_bar(stat="identity") +
  #geom_text(aes(label=round(arousal_index,2)), vjust=-0.5) +
  labs(x="Stimulation", y="Arousal Events/hr") +
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3])) +
  geom_text(aes(label=round(arousal_index,1)), vjust=-0.5,size=8) +
  theme_lunair() -> arousal_graph
arousal_graph
ggsave(arousal_graph, filename=paste0(output_path,"/arousal_graph.png"), width=8, height=9)


##### Combine all together into summaries:

#make pretty summary summary table synthesizing all of the above: 
merged_table<-merge(summary_table,mvmt_summary,by="cat_stim")
merged_table<-merge(merged_table,desat_summary,by.x="cat_stim",by.y="prior_to_desat") %>% 
  select(!c("mvmts","resp.arousal","spont.arousal","n_desats","mean_dur_desats","total_AHI_events","all_arousals")) 


merged_table %>% 
  rename(Stimulation="cat_stim") %>%
  rename(AHI="AHI_score") %>%
  #rename(`Total Events`="total_AHI_events") %>% 
  rename(`TST (hr)`="TST") %>%
  rename(`Arousals/hr`="arousal_index") ->merged_table

#reorder the columns so they make sense:
ordered_table<-merged_table[,c("Stimulation","TST (hr)","AHI","ODI","Arousals/hr")]
#dplyr::select(!c("Total Events","TST"))

ordered_table%>%
  ggtexttable(theme = ttheme("classic",
                             tbody.style = tbody_style(color=lunair_palette[3],
                                                       fill = "white"),
                             colnames.style = colnames_style(color=lunair_palette[2],
                                                             fill = "white",
                                                             size = 14,
                                                             face = "bold"),
  ),
  rows = NULL)->pretty_summary
pretty_summary
ggsave(pretty_summary, filename=paste0(output_path,"/summary_table.png"), width=8, height=4)

ordered_table %>% select(!c("ODI","Arousals/hr")) %>%
  ggtexttable(theme = ttheme("classic",
                             tbody.style = tbody_style(color=lunair_palette[3],
                                                       fill = "white"),
                             colnames.style = colnames_style(color=lunair_palette[2],
                                                             fill = "white",
                                                             size = 14,
                                                             face = "bold"),
  ),
  rows = NULL)->pretty_AHI_only
ggsave(pretty_AHI_only, filename=paste0(output_path,"/AHI_only_summary.png"), width=8, height=4)
# 
# #mosaic plot
# events_of_interest %>% 
#   filter(Event %in% c("A. Mixed", "A. Obstructive", "A. Central", "Desat"))->plt_events
# 
# plt_events%>%
#   ggplot() + 
#   geom_mosaic(aes(product(Event,cat_stim))) +
#  # scale_fill_manual(values=lunair_palette) +
#   theme_lunair() +
#   theme(legend.position = "none")
# 
# # 
# # #stick graphs together
# # combined_plots<-ggarrange(summary_graph, desat_graph, arousal_graph,
# #           ncol=3, nrow=1,
# #           common.legend = TRUE, legend="none",
# #           labels = c("AHI", "ODI", "Arousals/hr"),
# #           font.label = list(size = 14, face = "bold",hkjust = 0.5))
# # 
# # 
# # 
# # combined_plots
# 
# 
# 
# 
# 
# 
# #####################
# #first need to grab out the events for AHI calculation:
# data_ahi<-data %>% 
#   filter(Event %in% c("A. Mixed", "A. Obstructive", "A. Central")) %>%
# #wake by amp          
# data %>% 
#   group_by(phasic_amplitude) %>% 
#   filter(Event=="Wake") %>% 
#   summarize(time_awake=sum(Duration))
# 
# #sleep by amp
# data %>% group_by(amp_factor) %>%
#   summarize(time_N2=sum(Duration[Event=="N2"]),
#             time_N3=sum(Duration[Event=="N3"]),
#             time_REM=sum(Duration[Event=="REM"]),
#             time_Wake=sum(Duration[Event=="Wake"])) 
# 
# #more rough cut categorical
# unique(data$phasic_amplitude)
# pos_amps<-c("0.821581799" , "3.895920429" , "0.600081848" , "0.200315506")
# neg_amps<-c("-4.718511011", "-0.937159523", "-4.703091041", "-0.168610894" ,"-2.897081023", "-4.691417979","-2.474400894" ,"-1.705708153" ,"-1.133151673", "-4.703235153")
# 
# data %>% 
#   mutate(cat_amp=case_when(phasic_amplitude %in% neg_amps ~ "Off",
#                            phasic_amplitude %in% pos_amps ~ "On",
#                            TRUE ~ "No Change")) %>%
#   mutate(any_apnea=case_when(Event %in% c("A. Mixed", "A. Obstructive", "A. Central") ~ "Apnea"))->data
# #Mosaic plot
# data %>%
#   filter(Event %in% c("A. Central","A. Mixed","A. Obstructive","Desat")) %>%
#   ggplot()+geom_mosaic(aes(product(Event,cat_amp),fill=Event))+
#   scale_fill_manual(values=lunair_palette)
# 
# data %>%
#   filter(Event %in% c("A. Central","A. Mixed","A. Obstructive","Desat")) %>%
#   group_by(Event,cat_amp) %>%
#   summarize(time=sum(Duration),.groups = "drop") %>%
#   ggplot() + geom_mosaic(aes(product(Event,time),fill=Event))
#                