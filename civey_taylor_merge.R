# add column Day to taylor_rates_daily_smooth for merging
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>% 
  mutate(
    Day = date,
  )

# add columns from taylor_rates_daily_smooth to Civey data 
civey_taylor <- bigger_data %>%
  left_join(taylor_rates_daily_smooth, by = "Day")
