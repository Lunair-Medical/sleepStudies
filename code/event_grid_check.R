# This is code for checking the quality of scoring on PSGs by looking for double scored events
# Meg McEachran
# 6/30/2025

# Load required packages
library(dplyr)
library(readr)
library(janitor)
library(svDialogs)

# Read the CSV file
fp<-dlgOpen(default = "data/EventGrids")$res
df <- read_csv(fp) %>% 
  clean_names()


# Remove rows with missing start_epoch or end_epoch
df <- df %>% filter(!is.na(start_epoch) & !is.na(end_epoch))

# Add a row number for unique identification
df <- df %>% mutate(row_id = row_number())

#filter to just respiratory events
df <- df %>%
  filter(event %in% (c("A. Mixed", "A. Obstructive", "A. Central", "Hypopnea","Apnea",
                                           "H. Obstructive", "H.Mixed")))
# Find overlapping events
overlaps <- expand.grid(row_id1 = df$row_id, row_id2 = df$row_id) %>%
  filter(row_id1 < row_id2) %>%
  left_join(df, by = c("row_id1" = "row_id")) %>%
  left_join(df, by = c("row_id2" = "row_id"), suffix = c("_1", "_2")) %>%
  filter(
    (start_time_2 <= end_time_1) & (start_time_2 >= start_time_1))   %>% 
  select(
    event_1, start_time_1, end_time_1,
    event_2, start_time_2, end_time_2, 
  )

overlaps

#save the results to a sheet in an excel 
