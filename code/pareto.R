#pareto chart for maybelle



#setup
rm(list=ls())
library(lubridate)
library(hms)
library(janitor)
library(tidyverse)
library(readxl)
source(here("code/helper_fxns.R"))

lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2", "#397b96")
#read data
data<-read_xls("C:/Users/MegMcEachram/Downloads/hcp_search_4_21_2025.xls") %>% clean_names() %>%
  rename(volume="cpt_64582_volume")

data %>%
  arrange(desc(volume)) %>%
  mutate(hcp_anon=as.numeric(row_number()))->data

data %>% 
  arrange(desc(volume)) %>%
  mutate(cum_sum=cumsum(volume),
         cum_perc = cum_sum / sum(volume))->data

#add hcp quartile
data <- data %>%
  mutate(
    physician_quartile = cut(
      hcp_anon,
      breaks = quantile(hcp_anon, probs = seq(0, 1, 0.25), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("25%ile", "50%ile", "75%ile", "100%ile")
    ))%>%  
  mutate(top_physicians=cum_perc<=0.8 ) %>% #80% of implants
  mutate(top10physicians=case_when(hcp_anon < 0.1*length(data$hcp_anon) ~ T,
                                  TRUE ~ F))

#make chart
labels_to_show <- data %>% filter(hcp_anon %% 250 == 0) %>% pull(hcp_anon)

plot<-data %>% 
 #filter(hcp_anon<=100)%>%
  ggplot(aes(x=reorder(hcp_anon,-volume), y=volume,fill=top_physicians))+
  geom_col() +
  #theme_lunair()+
  xlab("")+
  ylab("Volume of Inspire implants")+
  geom_line(aes(y = cum_perc * max(volume), group = 1), color = "red", linetype = "dashed") +
  scale_x_discrete(breaks=labels_to_show)+
  scale_fill_manual(values=lunair_palette)+
  # Secondary axis for cumulative percentage:
  scale_y_continuous(
    name = "Number of Implants",
    sec.axis = sec_axis(~ . / max(data$volume), name = "Cumulative Proportion", labels = scales::percent)
  ) +
  xlab("Number of providers") +
  labs(fill="")+
  ggtitle("Pareto Chart of Implants by Provider") +
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"))
plot


plot2<-data %>% 
  #filter(hcp_anon<=100)%>%
  ggplot(aes(x=reorder(hcp_anon,-volume), y=volume,fill=top10physicians))+
  geom_col() +
  #theme_lunair()+
  xlab("")+
  ylab("Volume of Inspire implants")+
  geom_line(aes(y = cum_perc * max(volume), group = 1), color = "black", linetype = "dashed") +
  scale_x_discrete(breaks=labels_to_show)+
  scale_fill_manual(values=c(lunair_palette[4],lunair_palette[2]),
                    labels=c("40% of implants",""))+
  # Secondary axis for cumulative percentage:
  scale_y_continuous(
    name = "Number of Implants",
    sec.axis = sec_axis(~ . / max(data$volume), name = "Cumulative Proportion", labels = scales::percent,
                       breaks=c(0,.20,.40,.60,.80,1.00)
                       )
  ) +
  xlab("Number of providers") +
  labs(fill="",)+
  ggtitle("Top 10% of Implanters Perform 40% of Implants") +
  theme_minimal()+
  theme(plot.title=element_text(hjust=0.5),
        panel.background = element_rect(fill = "white",color="white"),
        plot.background = element_rect(fill = "white",color="white"),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey95"),
        legend.position = "none")
plot2


ggsave(plot2,filename="C:/Users/MegMcEachram/Downloads/pareto_plot.png")

#what proportion of inspire implants are done by the top 5% most prolific physicians?
data[last(which(data$top10physicians)),"cum_perc"]
