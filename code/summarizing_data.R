# code for working with the summary data file: 
rm(list=ls())
library(tidyverse)
library(janitor)
library(readxl)
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
