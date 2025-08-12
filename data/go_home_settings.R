#set up 
library(tidyverse)
library(janitor)
library(here)
library(purrr)

#read in all the different px device logs
px11<-read.csv(here("data/T1724-P1552-S000148-Event-20250731_090155.csv"))
px13<-read.csv(here("data/T1727-P1555-S000135-Event-20250731_094309.csv"))
px05<-read.csv(here("data/DeviceLog_[SN#000145]_18_07_2025_09_28_10.csv"))
px10<-read.csv(here("data/T1675-P1503-S000130-Event-20250729_030836.csv"))
px18<-read.csv(here("data/T1732-P1560-S000134-Event-20250731_134525.csv"))
px24<-read.csv(here("data/T1733-P1561-S000133-Event-20250731_144901.csv"))

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
