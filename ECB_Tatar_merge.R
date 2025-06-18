# add columns from ECB to taylor_rates_daily_smooth data frame  
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  left_join(ECB, by = "date")


# add deviations of deposit rate from Taylor rule rates
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  mutate(
    dr_dev_tr_hicp_pos0.5  = tr_hicp_pos0.5 - deposit_rate,
    dr_dev_tr_hicp_neg0.5  = tr_hicp_neg0.5 - deposit_rate,
    dr_dev_tr_hicp_neg1.5  = tr_hicp_neg1.5 - deposit_rate,
    dr_dev_tr_gdpd_pos0.5  = tr_gdpd_pos0.5 - deposit_rate,
    dr_dev_tr_gdpd_neg1.5  = tr_gdpd_neg1.5 - deposit_rate, 
    dr_dev_tr_gdpd_neg0.5  = tr_gdpd_neg0.5 - deposit_rate
)

# add deviations of deposit rate from Taylor rule rates
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  mutate(
    dr_dev_tr_hicp_pos0.5  = tr_hicp_pos0.5 - deposit_rate,
    dr_dev_tr_hicp_neg0.5  = tr_hicp_neg0.5 - deposit_rate,
    dr_dev_tr_hicp_neg1.5  = tr_hicp_neg1.5 - deposit_rate,
    dr_dev_tr_gdpd_pos0.5  = tr_gdpd_pos0.5 - deposit_rate,
    dr_dev_tr_gdpd_neg1.5  = tr_gdpd_neg1.5 - deposit_rate, 
    dr_dev_tr_gdpd_neg0.5  = tr_gdpd_neg0.5 - deposit_rate
  )

taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  mutate(
    dr_pos_dev_tr_hicp_pos0.5  = ifelse(tr_hicp_pos0.5 - deposit_rate >= 0, abs(tr_hicp_pos0.5 - deposit_rate), 0),
    dr_neg_dev_tr_hicp_pos0.5  = ifelse(tr_hicp_pos0.5 - deposit_rate < 0, abs(tr_hicp_pos0.5 - deposit_rate), 0),
    dr_pos_dev_tr_hicp_neg0.5  = ifelse(tr_hicp_neg0.5 - deposit_rate >= 0, abs(tr_hicp_neg0.5 - deposit_rate), 0),
    dr_neg_dev_tr_hicp_neg0.5  = ifelse(tr_hicp_neg0.5 - deposit_rate < 0, abs(tr_hicp_neg0.5 - deposit_rate), 0),
    dr_pos_dev_tr_hicp_neg1.5  = ifelse(tr_hicp_neg1.5 - deposit_rate >= 0, abs(tr_hicp_neg1.5 - deposit_rate), 0),
    dr_neg_dev_tr_hicp_neg1.5  = ifelse(tr_hicp_neg1.5 - deposit_rate < 0, abs(tr_hicp_neg1.5 - deposit_rate), 0),
    
    dr_pos_dev_tr_gdpd_pos0.5  = ifelse(tr_gdpd_pos0.5 - deposit_rate >= 0, abs(tr_gdpd_pos0.5 - deposit_rate), 0),
    dr_neg_dev_tr_gdpd_pos0.5  = ifelse(tr_gdpd_pos0.5 - deposit_rate < 0, abs(tr_gdpd_pos0.5 - deposit_rate), 0),
    dr_pos_dev_tr_gdpd_neg0.5  = ifelse(tr_gdpd_neg0.5 - deposit_rate >= 0, abs(tr_gdpd_neg0.5 - deposit_rate), 0),
    dr_neg_dev_tr_gdpd_neg0.5  = ifelse(tr_gdpd_neg0.5 - deposit_rate < 0, abs(tr_gdpd_neg0.5 - deposit_rate), 0),
    dr_pos_dev_tr_gdpd_neg1.5  = ifelse(tr_gdpd_neg1.5 - deposit_rate >= 0, abs(tr_gdpd_neg1.5 - deposit_rate), 0),
    dr_neg_dev_tr_gdpd_neg1.5  = ifelse(tr_gdpd_neg1.5 - deposit_rate < 0, abs(tr_gdpd_neg1.5 - deposit_rate), 0)
  )
