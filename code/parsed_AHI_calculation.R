# Code to estimate AHI for different phases of the night
#Designed to read in the device log file and the Nox event logs
# 
# 7/8/25 last updated 

#set up and load libraries 
rm(list=ls())

library(tidyverse)        # dplyr, tidyr, stringr, readr, etc.
library(janitor)          # clean_names()
library(flextable)        # nice table with cell-level formatting
library(officer)          # save_as_docx(), if you want a Word file
library(svDialogs)        # for file picker
library(lubridate)        # ymd_hms()
library(here)

###############################################################################
# 1.  File location                                                           #
###############################################################################
default_dir <- here("data/")

file_path <- dlg_open(
  default = default_dir,
  title = "Select Device Log CSV File",
  #filters = matrix(c("CSV files", "*.csv"), ncol = 3)
)$res

#read in raw data... NOTE this will likely throw a warning, use problems(log_raw) to check but it should be fine 
log_raw <- read_csv(file_path) %>% clean_names()

#clean and parse data
log_df <- log_raw %>%
  mutate(
    date       = mdy_hms(date),
    event_type = case_when(
      str_detect(event, regex("LogProgramming",            TRUE)) ~ "LogProgramming",
      str_detect(event, regex("LogChangeAmplitudeByOrder", TRUE)) ~ "LogChangeAmp",
      str_detect(event, regex("LogTherapyStart",           TRUE)) ~ "LogTherapyStart",
      str_detect(event, regex("LogTherapyEnd",             TRUE)) ~ "LogTherapyEnd",
      str_detect(event, regex("LogPositionChange",         TRUE)) ~ "LogPositionChange",
      str_detect(event, regex("LogProgAlgoLL",          TRUE)) ~ "LogProgAlgoLL",
      TRUE ~ "OTHER")
  ) %>%
  filter(event_type != "OTHER") %>%
  arrange(date)

#separate the data column into independent columns
id_cols <- setdiff(names(log_df), "data")

log_wide <- log_df %>%
  separate_rows(data, sep = " - ") %>% #breaks into rows
  mutate(
    key = str_trim(str_extract(data, "^.*?(?=\\s*=)")), #pull the column name
    value = str_trim(str_extract(data, "(?<=\\[).*(?=\\])")) #pull the column value
  ) %>%
  select(all_of(id_cols), key, value) %>%
  pivot_wider(names_from = key, values_from = value) %>% #make wide
  clean_names() #clean the new column names

#select down some of the information we want to keep