# code for working with the summary data file: 
rm(list=ls())
library(tidyverse)
library(janitor)
library(readxl)
library(svDialogs)
library(here)
library(hms)
library(lubridate)
library(tidytext)
source(here("code/helper_fxns.R"))


compare_flag<-0 #0 if WN vs WN, 1 if WN on baseline vs TE on later psgs

# SUMMARY DATA 
#read in data
raw_data<-read_excel(odfp("ECLIPSE 1 - Paraguay/Analysis/all_summary_data.xlsx"))


#Filter and plot patient data
all_ids<-unique(raw_data$patient_id)
subj_ids <- dlg_list(message="Please select a patient",choices = all_ids,multiple = T)$res

#grab and clean
factor_levels<-c("N1","N2","30D","60D","90D","120D")
raw_data %>% filter(patient_id %in% subj_ids) -> filtered_data

filtered_data %>% 
  #filter(strata=="WN")%>%
  filter(encounter %in% factor_levels )->filtered_data #filter out unsched and 2 part studies
  
#factor the encounters depending on how many visits they've had
filtered_data %>% 
  mutate(encounter=as.factor(encounter))%>%
  mutate(encounter=fct_relevel(encounter,factor_levels[1:length(unique(filtered_data$encounter))])) %>%
  mutate(encounter_mean=case_when(encounter=="N1" | encounter == "N2" ~ "Baseline",
                             TRUE ~ encounter)) %>%
  mutate(value=as.numeric(value))->filtered_data


#simplify the AI vs HI events for plotting:
filtered_data %>%
  mutate(parameter_simple=case_when(grepl("AI",parameter)~"Apneas",
                                    grepl("HI",parameter)~"Hypopneas",
                                    TRUE ~NA)) ->filtered_data

#add position and sleep stage col
filtered_data %>% 
  mutate(position=case_when(grepl("nonsupine", parameter)~"Nonsupine", 
                            grepl("supine",parameter) ~ "Supine",
                            TRUE ~ "All"))%>%
  mutate(REMnonREM=case_when(grepl("_nonREM",parameter) ~"nonREM",
                             grepl("_REM",parameter)  ~ "REM",
                             TRUE~ "All"))->filtered_data


### PLOTTING LOOPS:
#loop over and create summary tables:
tables_list<-list(length(subj_ids))

## create trends and stacked bar plots
for (i in 1:length(subj_ids)){
  patient_data<- filtered_data %>%
    filter(patient_id==subj_ids[i])
  
  #what's the last psg they got?
  present_levels<-levels(patient_data$encounter)[which(levels(patient_data$encounter) %in% unique(patient_data$encounter))]
  last_psg<-tail(present_levels,1)
  
  #patient 005's 120 was a HST so we don't have TE; grab 90D instead:
  if (compare_flag==1 & subj_ids[i]=="201-005" & last_psg=="120D"){last_psg<-tail(present_levels,2)[1]}
  
summary_csv<- patient_data%>%
  pivot_wider(id_cols = c(parameter,strata), names_from = encounter, values_from = value) %>% 
  mutate(baseline_mean=round(rowMeans(cbind(N1,N2),na.rm=T))) %>% 
  mutate(Baseline=paste0(baseline_mean," (",N1,"-",N2,")")) %>% 
  dplyr::select(c(parameter,strata, Baseline, any_of(c("N1","N2","30D","60D","90D","120D")),baseline_mean)) 
summary_csv->tables_list[[i]]
#write.csv(summary_csv,file = paste0("data/",subj_ids[i],"_trend.csv"))



#pivot long for plotting
AHI_plotting_data <-summary_csv %>%
  select (!c(Baseline,N1,N2))%>%
  filter(parameter %in% c( 
                          "AI","AI_supine","AI_nonsupine",
                          "HI","HI_supine","HI_nonsupine",
                          "T90","T90_supine","T90_nonsupine",
                          "T88", "T88_supine", "T88_nonsupine",
                          "mean_desat","mean_desat_supine","mean_desat_nonsupine",
                          "mean_SpO2","mean_SpO2_supine","mean_SpO2_nonsupine"))%>%
  pivot_longer(cols= -c(parameter,,strata),names_to="encounter",values_to="value")



rem_plotting_data<- #parse by REM/nonREM
  summary_csv %>%
  select (!Baseline)%>%
  filter(parameter %in% c( 
    "AI","AI_REM","AI_nonREM",
    "HI","HI_REM","HI_nonREM"))%>%
  pivot_longer(cols= -c(parameter,strata),names_to="encounter",values_to="value")




# order the facets:
facet_order <- c("All","Supine","Nonsupine") 

#order the encounters within each facet
AHI_plotting_data %>%
  mutate(ordering_var=case_when(encounter=="baseline_mean" ~ 1,
                                TRUE ~ 2)) %>% 
  filter(!(encounter %in% c("N1","N2")))->AHI_plotting_data

#filter out values if i want to compare last psg TE on to wn baseline:
if(compare_flag==1){
AHI_plotting_data %>%
  mutate(keep=case_when(encounter=="baseline_mean" ~ T,
                        strata=="TE" & !(encounter %in% c("baseline_mean","N1","N2")) ~ T,
                        TRUE ~ F)) ->AHI_plotting_data
} else {
AHI_plotting_data %>% filter(strata == "WN")->AHI_plotting_data
}

#add a position value 
AHI_plotting_data %>% 
  mutate(position=case_when(grepl("_supine",parameter)~"Supine",
                            grepl("_nonsupine",parameter)~"Nonsupine",
                            TRUE ~ "All"))->AHI_plotting_data

#add a parameter_simple value
AHI_plotting_data %>% 
  mutate(parameter_simple=case_when(grepl("AI",parameter)~"Apneas",
                                    grepl("HI",parameter)~"Hypopneas",
                                    TRUE ~ NA))->AHI_plotting_data

###PLOT WHOLE NIGHT VALUES OVER TIME
AHI_plotting_data %>%
  
  filter(encounter %in% c("baseline_mean", last_psg)) %>%
  filter(parameter %in% c( 
    "AI","AI_supine","AI_nonsupine",
    "HI","HI_supine","HI_nonsupine"))%>%
  mutate(position = factor(position, levels = facet_order)) %>%
  #mutate(encounter = factor(encounter, levels = bar_order)) %>%
  mutate(encounter=recode(encounter,baseline_mean="BL"))%>%
  ggplot(aes(x = reorder_within(encounter,ordering_var,position), y = value, fill = encounter)) +
  geom_col() +
  facet_wrap(~position, scales = "free_x") +
  scale_x_reordered()+
  xlab("")+
  ylab("AHI")+
  #labs(fill="Composition")+
  theme_lunair()+
  theme(legend.position="none")+
  #geom_hline(aes(yintercept=20))+
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3]))->bar_wholenight
bar_wholenight
ggsave(plot=bar_wholenight,filename=paste0("figures/",subj_ids[i],"_barplot.png"),width = 6,height=4,units = "in")


#stacked barplots to show composition of A/H
AHI_plotting_data %>%
 
  filter(encounter %in% c("baseline_mean", last_psg)) %>%
  filter(parameter %in% c( 
    "AI","AI_supine","AI_nonsupine",
    "HI","HI_supine","HI_nonsupine")) %>%
  mutate(position = factor(position, levels = facet_order)) %>%
  #mutate(encounter = factor(encounter, levels = bar_order)) %>%
  mutate(encounter=recode(encounter,baseline_mean="BL"))%>%
  ggplot(aes(x = reorder_within(encounter,ordering_var,position), y = value, fill = parameter_simple)) +
  geom_col() +
  facet_wrap(~position, scales = "free_x") +
  scale_x_reordered()+
  xlab("")+
  ylab("AHI")+
  labs(fill="Composition")+
  theme_lunair()+
  theme(legend.position="right")+
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3]))->stacked_bar_wholenight
stacked_bar_wholenight

if(compare_flag==0){
ggsave(plot=stacked_bar_wholenight,filename=paste0("figures/",subj_ids[i],"_stackedbar.png"),width=6,height=4,units = "in")
} else {
ggsave(plot=stacked_bar_wholenight,filename=paste0("figures/",subj_ids[i],"_stackedbar_stim.png"),height=4,units = "in")
}

AHI_plotting_data %>%
  #filter(strata == "WN") %>%
  filter(encounter %in% c("baseline_mean", last_psg)) %>%
  filter(parameter %in% c( 
    "AI","AI_supine","AI_nonsupine",
    "HI","HI_supine","HI_nonsupine"))%>%
  mutate(position = factor(position, levels = facet_order)) %>%
  #mutate(encounter = factor(encounter, levels = bar_order)) %>%
  mutate(encounter=recode(encounter,baseline_mean="BL"))%>%
  ggplot(aes(x = reorder_within(encounter,ordering_var,position), y = value, fill = parameter_simple)) +
  geom_bar(stat="identity",position = "fill") +
  facet_wrap(~position, scales = "free_x") +
  scale_x_reordered()+
  xlab("")+
  ylab("AHI")+
  labs(fill="Composition")+
  theme_lunair()+
  theme(legend.position="right")+
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3]))->stacked_bar_wn_prop


#save the plots
if(compare_flag==0){
  ggsave(plot=stacked_bar_wn_prop,filename=paste0("figures/",subj_ids[i],"_stackedbar_prop.png"),width=6,height=4)
} else {
  ggsave(plot=stacked_bar_wn_prop,filename=paste0("figures/",subj_ids[i],"_stackedbar_prop_stim.png"),width=6,height = 4)
}

}

#### end of the loop 


### Loop to create patient progress line/dot plots: 
for (i in 1:length(subj_ids)){
  patient_data<- filtered_data %>%
    filter(patient_id==subj_ids[i])
  
  #what's the last psg they got?
  present_levels<-levels(patient_data$encounter)[which(levels(patient_data$encounter) %in% unique(patient_data$encounter))]
  last_psg<-tail(present_levels,1)
  
  #patient 005's 120 was a HST so we don't have TE; grab 90D instead:
  if (subj_ids[1]=="201-005"){patient_data<-filter(patient_data,encounter!="120D")}
  
  #group to get the patient baseline values 
  mean_factor_levels<-c("Baseline","30D","60D","90D","120D")
  mean_data<-patient_data %>%
    group_by(encounter_mean,strata, parameter)%>%
    summarize(n=n(),mean=mean(value,na.rm=T),max=max(value),min=min(value)) %>% 
    mutate(encounter_mean=as.factor(encounter_mean))->mean_data
  mean_data<-mean_data%>% 
    mutate(encounter_mean=fct_relevel(encounter_mean,mean_factor_levels[1:length(unique(mean_data$encounter_mean))])) 

  
  #dot plot whole night AHI over time
  AHI_over_time<-mean_data %>% 
    filter(strata=="WN" & parameter=="AHI") %>%
    replace_na(list(sd=0))%>% #replace the NAs with 0 to get the error bars
    ggplot(aes(x=encounter_mean,y=mean)) + 
    geom_errorbar(aes(ymin=min,ymax=max),linewidth=0.5, color=lunair_palette[3],width=0.2)+
    geom_point(color=lunair_palette[7],size=3)+
    geom_line(aes(x=encounter_mean,y=mean,group=1),color=lunair_palette[7],linewidth=1)+
    theme_lunair()+
    xlab("PSG Day")+
    ylab("AHI (Events/hr)")+
    ylim(0,max(mean_data[which(mean_data$parameter=="AHI" & mean_data$strata=="WN"),"mean"])*1.5)+
    ggtitle(paste0("Subject ", subj_ids[i]," Whole-Night AHI Trend"))
  
ggsave(plot=AHI_over_time,filename = paste0("figures/",subj_ids[i],"_AHI_progress_plot.png"),width=6,height=4,units = "in")

  #dot plot whole night T90 over time 
T90_over_time<-mean_data %>% 
  filter(strata=="WN" & parameter=="T90") %>% 
  replace_na(list(sd=0))%>% #replace the NAs with 0 to get the error bars
  ggplot(aes(x=encounter_mean,y=mean)) + 
  geom_errorbar(aes(ymin=min,ymax=max),linewidth=0.5, color=lunair_palette[3],width=0.2)+
  geom_point(color=lunair_palette[7],size=3)+
  geom_line(aes(x=encounter_mean,y=mean,group=1),color=lunair_palette[7],linewidth=1)+
  theme_lunair()+
  xlab("PSG Day")+
  ylab("Percent Time <90% SpO2")+
  ylim(0,max(mean_data[which(mean_data$parameter=="T90" & mean_data$strata=="WN"),"mean"])*1.1)+
  ggtitle(paste0("Subject ", subj_ids[i]," Whole-Night T90 Trend"))+
  theme(plot.title = element_text(hjust=0.5))
T90_over_time
ggsave(plot=T90_over_time,filename = paste0("figures/",subj_ids[i],"_T90_progress_plot.png"),width=6.5,height=4,units = "in")

#dot plot whole night HB over time 
HB_over_time<-mean_data %>% 
  filter(strata=="WN" & parameter=="HB") %>% 
  replace_na(list(sd=0))%>% #replace the NAs with 0 to get the error bars
  ggplot(aes(x=encounter_mean,y=mean)) + 
  geom_errorbar(aes(ymin=min,ymax=max),linewidth=0.5, color=lunair_palette[3],width=0.2)+
  geom_point(color=lunair_palette[7],size=3)+
  geom_line(aes(x=encounter_mean,y=mean,group=1),color=lunair_palette[7],linewidth=1)+
  theme_lunair()+
  xlab("PSG Day")+
  ylab("Hypoxic Burden (%min/hr)")+
  ylim(0,max(mean_data[which(mean_data$parameter=="HB"),"mean"])*1.1)+
  ggtitle(paste0("Subject ", subj_ids[i]," Whole-Night Hypoxic Burden Trend"))+
  theme(plot.title = element_text(hjust=0.5))
HB_over_time
ggsave(plot=HB_over_time,filename = paste0("figures/",subj_ids[i],"_HB_progress_plot.png"),width=6.5,height=4,units = "in")


#dot plot whole night HB over time 
desat_over_time<-mean_data %>% 
  filter(strata=="WN" & parameter=="mean_desat") %>% 
  replace_na(list(sd=0))%>% #replace the NAs with 0 to get the error bars
  ggplot(aes(x=encounter_mean,y=mean)) + 
  geom_errorbar(aes(ymin=min,ymax=max),linewidth=0.5, color=lunair_palette[3],width=0.2)+
  geom_point(color=lunair_palette[7],size=3)+
  geom_line(aes(x=encounter_mean,y=mean,group=1),color=lunair_palette[7],linewidth=1)+
  theme_lunair()+
  xlab("PSG Day")+
  ylab("Average % Desat")+
  ylim(4,max(mean_data[which(mean_data$parameter=="mean_desat"),"mean"])*1.5)+
  ggtitle(paste0("Subject ", subj_ids[i]," Whole-Night % Desat Trend"))+
  theme(plot.title = element_text(hjust=0.5))
desat_over_time
ggsave(plot=desat_over_time,filename = paste0("figures/",subj_ids[i],"_desat_progress_plot.png"),width=6.5,height=4,units = "in")

#dot plot whole night 
spO2<-mean_data %>% 
  filter(strata=="WN" & parameter=="mean_SpO2") %>% 
  replace_na(list(sd=0))%>% #replace the NAs with 0 to get the error bars
  ggplot(aes(x=encounter_mean,y=mean)) + 
  geom_errorbar(aes(ymin=min,ymax=max),linewidth=0.5, color=lunair_palette[3],width=0.2)+
  geom_point(color=lunair_palette[7],size=3)+
  geom_line(aes(x=encounter_mean,y=mean,group=1),color=lunair_palette[7],linewidth=1)+
  theme_lunair()+
  xlab("PSG Day")+
  ylab("Average SpO2")+
  ylim(88,max(mean_data[which(mean_data$parameter=="mean_SpO2"),"mean"])*1.5)+
  ggtitle(paste0("Subject ", subj_ids[i]," Whole-Night Oxygenation Trend"))+
  theme(plot.title = element_text(hjust=0.5))
spO2
ggsave(plot=spO2,filename = paste0("figures/",subj_ids[i],"_spO2_progress_plot.png"),width=6.5,height=4,units = "in")

}


#loop to look at the stim on vs. baseline
for (i in 1:length(subj_ids)){
  patient_data<- filtered_data %>%
    filter(patient_id==subj_ids[i])
  
  #what's the last psg they got?
  present_levels<-levels(patient_data$encounter)[which(levels(patient_data$encounter) %in% unique(patient_data$encounter))]
  last_psg<-tail(present_levels,1)
  
  #patient 005's 120 was a HST so we don't have TE; grab 90D instead:
  if (subj_ids[i]=="201-005"){patient_data<-filter(patient_data,encounter!="120D")}
  
  #group to get the patient baseline values 
  mean_factor_levels<-c("Baseline","30D","60D","90D","120D")
  mean_data<-patient_data %>%
    group_by(encounter_mean,strata, parameter)%>%
    reframe(n=n(),mean=mean(value,na.rm=T),max=max(value),min=min(value)) %>% 
    mutate(encounter_mean=as.factor(encounter_mean))->mean_data
  mean_data<-mean_data%>% 
    mutate(encounter_mean=fct_relevel(encounter_mean,mean_factor_levels[1:length(unique(mean_data$encounter_mean))])) 
  
  #make a keep variable to compare baseline to last_psg date 
  mean_data %>% 
    mutate(keep=case_when(encounter_mean=="Baseline" ~ T,
                          encounter_mean==last_psg & grepl("stim",parameter)~ T,
                          TRUE ~ F))%>%
    filter(keep)->stim_comparison_data
  
  # add AHI column and stim column
  stim_comparison_data %>%
    mutate(stim=case_when(grepl("stim",parameter)~T,
                          TRUE ~ F)) %>%
    mutate(AHI=case_when(grepl("AHI",parameter) ~T,
                         TRUE ~ F)) %>% 
    mutate(position=case_when(grepl("_supine",parameter) ~ "Supine",
                              grepl("_nonsupine",parameter) ~ "Nonsupine",
                              TRUE ~ "All"))->stim_comparison_data
  
  #make bar plots
  stim_improvement<-stim_comparison_data %>% 
    filter(parameter %in% c("AHI_supine","AHI_nonsupine","AHI_stim_supine","AHI_stim_nonsupine"))%>%
    ggplot(aes(x=stim,y=mean,fill=stim))+
    geom_col(width=0.5,position="dodge")+
    facet_wrap(~position, scales="free_x")+
    theme_lunair()+
    xlab("")+
    ylab("AHI (Events/hr)")+
    ggtitle(paste0("Patient ", subj_ids[i]))+
    theme(plot.title=element_text(hjust=0.5))+
    scale_x_discrete(labels=c("Baseline","Stim On"))+
    #ylim(0,max(stim_comparison_data$mean)+0.1*max(stim_comparison_data$mean))+
    
    scale_fill_manual(values=c(lunair_palette[2],lunair_palette[3]))
             
  ggsave(stim_improvement,filename=paste0("figures/",subj_ids[i],"_stim_improvement.png"),width=6,height=4)
}

###################################################
#below for patient 10 REM (or anyone else parsed by REM):
# patient 201-010 is highly REM sensitive so let's look at his stuff:
px10<-filtered_data %>% filter(patient_id=="201-010") 

#px10<-px10[which(grepl("REM",px10$parameter)),]


#simplify the AI vs HI events for plotting:
rem_plotting_data %>%
  mutate(parameter_simple=case_when(grepl("AI",parameter)~"Apneas",
                                    grepl("HI",parameter)~"Hypopneas",
                                    TRUE ~NA)) ->rem_plotting_data
# order the facets:
rem_facet_order <- c("All","REM","nonREM") # Replace with your actual facet names

#add an ordering val for w/in facet plots
rem_plotting_data %>%
  mutate(ordering_var=case_when(encounter=="baseline_mean" ~ 1,
                                TRUE ~ 2)) %>% 
  filter(!(encounter %in% c("N1","N2")))->rem_plotting_data

rem_plotting_data %>%
  
  filter(encounter %in% c("baseline_mean", last_psg)) %>%
  mutate(REMnonREM = factor(REMnonREM, levels = rem_facet_order)) %>%
  #mutate(encounter = factor(encounter, levels = bar_order)) %>%
  mutate(encounter=recode(encounter,baseline_mean="BL"))%>%
  ggplot(aes(x = reorder_within(encounter,ordering_var,REMnonREM), y = value, fill = parameter_simple)) +
  geom_col() +
  facet_wrap(~REMnonREM, scales = "free_x") +
  scale_x_reordered()+
  xlab("")+
  ylab("AHI")+
  labs(fill="Composition")+
  theme_lunair()+
  theme(legend.position="right")+
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[3]))->rem_bar_wn
rem_bar_wn
ggsave(plot=rem_bar_wn,"figures/201-010_byREM.png")
# 