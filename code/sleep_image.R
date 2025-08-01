#Code for pulling processing sleep image data: 

#setup 
rm(list=ls())
library(tidyverse)
library(janitor)
library(svDialogs)
library(here)
library(data.table)

source(here("code/helper_fxns.R"))

#read data 
SI_filepath<-odfp("ECLIPSE 1 - Paraguay/Analysis/all_sleep_image")

csv_files <- list.files(path = SI_filepath, pattern = "\\.csv$", full.names = TRUE)

# Read all into a list of data frames using base R
all_data_list <- lapply(csv_files, read.csv)

#bind all the rows together:
all_data<-bind_rows(all_data_list)
