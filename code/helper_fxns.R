#this is code that's essential for parsing large labchart exported as txt: 
# parse_labchart_txt <- function(filepath){ 
#   
#   # First have to read in the file using readLines which will be able to handle windows carriage line encoding \r\n
#   # Read all lines, removing carriage returns
#   lines <- readLines(filepath)
#   lines <- gsub("\r", "", lines)
#   
#   # Write to a temporary file
#   temp <- tempfile()
#   writeLines(lines, temp)
#   
#   # Now read the cleaned file
#   dt <- read.table(temp,
#                    sep = "\t",
#                    header = F,
#                    stringsAsFactors = FALSE,
#                    fill = TRUE,      # Allow for ragged rows
#                    quote = "",
#                    comment.char = "",
#                    strip.white = TRUE) # Optional: trim whitespace
# 
#   # dt<- read.table(filepath, 
#   #                  sep = "\t", 
#   #                  header = T,  # Set to TRUE if first row contains headers
#   #                  stringsAsFactors = FALSE,
#   #                  fill = F,     # Fill missing values
#   #                  quote = "",      # No quote characters
#   #                  comment.char = "") # No comment characters
#   return(dt)
# }

#### okay go from here: 
library(data.table)
library(dplyr)
library(tidyverse)
library(janitor)
# dt<-read_tsv(wfp,
#              #col_types = cols(.default="c"),
#              trim_ws = TRUE)

  ### Read in and name the data
parse_labchart_txt<-function(fp, downsample_rate){
  library(data.table)
  dt<-data.table::fread(fp, strip.white = TRUE, fill = T)  #fp can be the raw labchart txt file
  
  # Add channel names
  names_all<-as.character( dt[which(dt[,1]=="ChannelTitle="),])  
  names<-unlist(str_split(names_all, pattern = "\t")) %>%
   .[!.%in% c("ChannelTitle=","NA")] %>% 
    make_clean_names() # %>%  
  
  #make new col names but check you have right # of cols
  names<-c("sec_since_midnight",
          "date", #comment this out if the labchart file doesn't have date column
           names)
  
  if (length(names)!=ncol(dt)){
    stop("Number of column names does not match number of columns in the data.")
  }
  
  # Remove the header rows from the main data
  dt <- dt[-c(1:(which(dt[[1]]=="Range=")+1)), ]
  data.table::setnames(dt, names)
  
  ### Downsampling:
  if (missing(downsample_rate)==TRUE) {
    return(dt)  # No downsampling needed
  } else {
  
  # Downsample the data by taking every nth row
  downsampled_dt <- dt[seq(1, nrow(dt), by = downsample_rate), ]
  
  ds_val<-downsample_rate * 0.005 #multiply by default labchart rate
  return(downsampled_dt)
  print(paste("Downsampled data by a factor of", downsample_rate, "to", ds_val, "seconds per row."))}
}


#function to convert seconds since midnight into an actual time: 
ssm_to_time<-function(x){
  h=floor(x/3600)
  m=floor((x-(h*3600))/60)
return(as.ITime(h))
}
ssm_to_time(77982)
