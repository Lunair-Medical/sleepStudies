# code for working with the summary data file: 
rm(list=ls())
library(tidyverse)
library(janitor)
library(readxl)
library(svDialogs)

#helper functions: 
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

odfp <- function(rel_path) {
  user<-Sys.getenv("USERNAME")
  onedrive_path<-paste0("C:/Users/",user,"/Lunair Medical/R&D - Documents/FiH Data")
  #if (path == "") stop("OneDrive path not found in environment variables.")
  file.path(onedrive_path, rel_path)
}

#check it's working:
odfp("Apple")

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
