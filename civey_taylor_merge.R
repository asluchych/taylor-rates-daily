# column Day to taylor_rates_daily for merging
taylor_rates_daily <- taylor_rates_daily %>% 
  mutate(
    Day = date,
  )

# add columns from taylor_rates_daily to Civey data 
civey_taylor <- bigger_data %>%
  left_join(taylor_rates_daily, by = "Day")