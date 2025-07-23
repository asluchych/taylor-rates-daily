# add columns from ECB to taylor_rates_daily data frame  
taylor_rates_daily <- taylor_rates_daily %>%
  left_join(ECB, by = "date")

# add deviations Taylor rule rates from deposit rate
taylor_rates_daily <- taylor_rates_daily %>%
  mutate(
    tr_hicp_pos0.5_dev_dr = tr_hicp_pos0.5 - deposit_rate,
    tr_hicp_neg0.5_dev_dr = tr_hicp_neg0.5 - deposit_rate,
    tr_hicp_neg1.5_dev_dr = tr_hicp_neg1.5 - deposit_rate,
    tr_gdpd_pos0.5_dev_dr = tr_gdpd_pos0.5 - deposit_rate,
    tr_gdpd_neg1.5_dev_dr = tr_gdpd_neg1.5 - deposit_rate, 
    tr_gdpd_neg0.5_dev_dr = tr_gdpd_neg0.5 - deposit_rate
)

# add absolute deviations Taylor rule rates from deposit rates
taylor_rates_daily <- taylor_rates_daily %>%
  mutate(
    tr_hicp_pos0.5_dev_dr_abs = abs(tr_hicp_pos0.5 - deposit_rate),
    tr_hicp_neg0.5_dev_dr_abs = abs(tr_hicp_neg0.5 - deposit_rate),
    tr_hicp_neg1.5_dev_dr_abs = abs(tr_hicp_neg1.5 - deposit_rate),
    tr_gdpd_pos0.5_dev_dr_abs = abs(tr_gdpd_pos0.5 - deposit_rate),
    tr_gdpd_neg1.5_dev_dr_abs = abs(tr_gdpd_neg1.5 - deposit_rate), 
    tr_gdpd_neg0.5_dev_dr_abs = abs(tr_gdpd_neg0.5 - deposit_rate)
  )

# add positive deviations Taylor rule rates from deposit rates
taylor_rates_daily <- taylor_rates_daily %>%
  mutate(
    tr_hicp_pos0.5_dev_dr_pos = ifelse(tr_hicp_pos0.5 - deposit_rate >= 0, abs(tr_hicp_pos0.5 - deposit_rate), 0),
    tr_hicp_neg0.5_dev_dr_pos = ifelse(tr_hicp_neg0.5 - deposit_rate >= 0, abs(tr_hicp_neg0.5 - deposit_rate), 0),
    tr_hicp_neg1.5_dev_dr_pos = ifelse(tr_hicp_neg1.5 - deposit_rate >= 0, abs(tr_hicp_neg1.5 - deposit_rate), 0),
    tr_gdpd_pos0.5_dev_dr_pos = ifelse(tr_gdpd_pos0.5 - deposit_rate >= 0, abs(tr_gdpd_pos0.5 - deposit_rate), 0),
    tr_gdpd_neg0.5_dev_dr_pos = ifelse(tr_gdpd_neg0.5 - deposit_rate >= 0, abs(tr_gdpd_neg0.5 - deposit_rate), 0),
    tr_gdpd_neg1.5_dev_dr_pos = ifelse(tr_gdpd_neg1.5 - deposit_rate >= 0, abs(tr_gdpd_neg1.5 - deposit_rate), 0) 
  )

# add negative deviations Taylor rule rates from deposit rates
taylor_rates_daily <- taylor_rates_daily %>%
  mutate(
    tr_hicp_pos0.5_dev_dr_neg = ifelse(tr_hicp_pos0.5 - deposit_rate < 0, abs(tr_hicp_pos0.5 - deposit_rate), 0),
    tr_hicp_neg0.5_dev_dr_neg = ifelse(tr_hicp_neg0.5 - deposit_rate < 0, abs(tr_hicp_neg0.5 - deposit_rate), 0),
    tr_hicp_neg1.5_dev_dr_neg = ifelse(tr_hicp_neg1.5 - deposit_rate < 0, abs(tr_hicp_neg1.5 - deposit_rate), 0),
    tr_gdpd_pos0.5_dev_dr_neg = ifelse(tr_gdpd_pos0.5 - deposit_rate < 0, abs(tr_gdpd_pos0.5 - deposit_rate), 0),
    tr_gdpd_neg0.5_dev_dr_neg = ifelse(tr_gdpd_neg0.5 - deposit_rate < 0, abs(tr_gdpd_neg0.5 - deposit_rate), 0),
    tr_gdpd_neg1.5_dev_dr_neg = ifelse(tr_gdpd_neg1.5 - deposit_rate < 0, abs(tr_gdpd_neg1.5 - deposit_rate), 0) 
  )