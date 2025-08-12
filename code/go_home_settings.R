#set up 
library(tidyverse)
library(janitor)
library(here)
library(purrr)

#read in all the different px device logs
px11<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1819-P1646-S000148-Event-20250806_013818.csv")
px13<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1785-P1612-S000135-Event-20250803_111403.csv")
px05<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1701-P1529-S000145-Event-20250730_085900.csv")
px10<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1795-P1622-S000130-Event-20250804_083541.csv")
px18<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1857-P1684-S000134-Event-20250807_071008.csv")
px24<-read.csv("C:/Users/MegMcEachram/Desktop/Downloaded Logs/T1796-P1623-S000133-Event-20250804_085103.csv")

all_dl<-list(px11,px13,px10,px18,px24,px05)
names(all_dl) <- c("px11", "px13", "px10", "px18", "px24", "px05")

#function for go home settings
get_gohome_settings<-function(x){
  #x<-clean_names(x)
  names(x)<-c("date","event","data")
  last_programmed<- x[last(which(x$event=="LogProgramming")),]
  
  last_programmed %>%
    separate_rows(data, sep = " - ") %>%
    mutate(
      key = str_trim(str_extract(data, "^.*?(?=\\s*=)")),
      val = str_trim(str_extract(data, "(?<=\\[).*(?=\\])"))
    ) %>%
    pivot_wider(id_cols = c(date, event),
                names_from  = key,
                values_from = val) %>% 
    clean_names()->last_programmed
}

all_dl |> map(~get_gohome_settings(.x)) -> gohome_list

vals_of_interest <- c(
  "phasic_amplitude", "pulse_width", "max_phasic_amplitude", "min_phasic_amplitude",
  "roll_pause", "roll_pause_mode", "upright_pause", "upright_pause_mode",
  "frequency", "onset_ramp_time", "therapy_delay", "therapy_rate", "phasic_time"
)

# Initialize empty data frame with appropriate dimensions and column names
settings_table <- as.data.frame(matrix(NA, nrow = length(all_dl), ncol = length(vals_of_interest)))
colnames(settings_table) <- vals_of_interest

# Loop over each df and column to extract the value
for (i in seq_along(gohome_list)) {
  df <- gohome_list[[i]]
  for (j in seq_along(vals_of_interest)) {
    colname <- vals_of_interest[j]
    # Safely extract value, account for missing columns
    if (colname %in% names(df)) {
      settings_table[i, j] <- df[[colname]][1]   # Extract from first row
    } else {
      settings_table[i, j] <- NA                 # Or leave as NA if not present
    }
  }
}
settings_table$patient_id<-names(all_dl)

settings_table<-settings_table[c("patient_id", setdiff(names(settings_table), "patient_id"))]
settings_table


write.csv(settings_table,"data/gohome_jul25.csv")
