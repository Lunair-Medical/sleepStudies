##GRAVEYARD


tsr <- labchart %>%
  mutate(stim_series_id = cumsum(smoothed_stim_numeric != lag(smoothed_stim_numeric, default = first(smoothed_stim_numeric) + 1))) %>%
  group_by(stim_series_id) %>%
  mutate(max_in_run = max(smoothed_stim_numeric)) %>%
  ungroup()
# Only keep the starts of runs of length 3
target_starts <- run_starts[true_stim_runs]
middle_indices<-target_starts + 1

middle_values<- run1$smoothed_stim_numeric[middle_indices] #get the middle value of each run and the time that it occurred
mv_times<-run1$datetime[middle_indices] #get the time of each middle value

# Ok so now I have series of middle values (i.e. stim peaks) and each peak represents a stim pulse. I need to identify runs of stims at particular amplitudes,
#then go back and divide the run data up categorically.
mvd<-data.frame(middle_values,mv_times)