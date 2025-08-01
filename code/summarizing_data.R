# code for working with the summary data file: 
rm(list=ls())
library(tidyverse)
library(janitor)
library(readxl)
library(svDialogs)
source(here("code/helper_fxns.R"))




#read in data
raw_data<-read_excel(odfp("ECLIPSE 1 - Paraguay/Analysis/all_summary_data.xlsx"))


#Filter and plot patient data
all_ids<-unique(raw_data$patient_id)
subj_ids <- dlg_list(message="Please select a patient",choices = all_ids,multiple = T)$res

#grab and clean
raw_data %>% filter(patient_id %in% subj_ids) -> filtered_data
filtered_data %>% 
  filter(encounter!="unsched") %>%
  mutate(encounter=as.factor(encounter))%>%
  mutate(encounter=fct_relevel(encounter,c("N1","N2","30D","60D","90D"))) %>%
  mutate(encounter=case_when(encounter=="N1" | encounter == "N2" ~ "Baseline",
                             TRUE ~ encounter)) %>%
  mutate(value=as.numeric(value))->filtered_data


#group and average the two baseline nights:
filtered_data %>%
  group_by(patient_id,encounter,strata)%>%
  summarize(n=n(),mean=mean(value,na.rm=T),sd=sd(value,na.rm=T))
##progress plots

for (i in 1:length(subj_ids)){
  
#individual patient plots:  
AHI_over_time<-
 filtered_data %>% 
  filter(parameter=="AHI" & strata == "WN") %>%
  ggplot() +
  aes(x=encounter,y=round(value))+geom_point()+geom_line(aes(x=encounter,y=value))+ 
  xlab("Milestone PSGs")+
  ylab("Whole night AHI")+
  ggtitle(paste0("Subject ", subj_ids[i]))
  theme_lunair()
AHI_over_time}
