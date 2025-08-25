
odfp <- function(rel_path) {
  user<-Sys.getenv("USERNAME")
  onedrive_path<-paste0("C:/Users/",user,"/Lunair Medical/R&D - Documents/FiH Data")
  #if (path == "") stop("OneDrive path not found in environment variables.")
  file.path(onedrive_path, rel_path)
}

#helper functions: 
extrafont::loadfonts()

theme_lunair <- function(textsize=18){
  
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

#custom palette: 
lunair_palette=c(
  "#6bdbdb","#143464", "#697e9c", "#ccd9e2","#7CCDF5", "#397b96","#833080")

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

#---------------date/time parsing-----------------------------------

#flexibly accommodates different types of event grid timestamps
fix_eg_times <- function(df) {
  # Coerce to character first
  st_char <- as.character(df$start_time)
  end_char<- as.character(df$end_time)
  
  # Parse and see if you're successful
  start_datetime <- suppressWarnings(mdy_hms(st_char))
  
  #if none parsed, assume you're missing a date and prompt for it: 
  if(all(is.na(start_datetime))){ 
    
    #prompt for date
    st_dt <- dlg_input(message="Please provide a start date (YYYY-MM-DD):")$res
    analysis_start_date<-as.Date(st_dt) #convert to Date
    analysis_end_date<-analysis_start_date + days(1) #assume the same date for now
    
    #parse just the time portion and store in the df:
    df$just_start_time<-lubridate::hms(st_char)
    df$just_end_time<-lubridate::hms(end_char)
    
    
    #add the dates to get start and end datetimes
    df<- df %>%
      mutate(event_start_date=if_else(as.numeric(just_start_time) < 12*60*60, analysis_end_date, analysis_start_date),
             event_end_date = if_else(as.numeric(just_end_time) < 12*60*60, analysis_end_date, analysis_start_date)) 
    
    
    #paste together the times and dates to get datetimes 
    df %>% 
      mutate(event_start_datetime=as.POSIXct(event_start_date + just_start_time),
             event_end_datetime=as.POSIXct(event_end_date + just_end_time))->df
    
    df$start_time<-as_hms(df$event_start_datetime)
    df$end_time<-as_hms(df$event_end_datetime)
    
    df<-df[,-which( grepl("just",colnames(df)))]
    
  } else {
    df %>% 
      mutate(event_start_datetime=as.POSIXct(start_time,format= "%m/%d/%Y %I:%M:%S %p"),
             event_end_datetime=as.POSIXct(end_time,format= "%m/%d/%Y %I:%M:%S %p"),
             start_time=as_hms(event_start_datetime),
             end_time=as_hms(event_end_datetime))->df
  }
  return(df)
}

