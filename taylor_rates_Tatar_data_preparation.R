# rename  columns to more sensible names
taylor_rates_Tatar <- taylor_rates_Tatar %>% 
  rename(
    quarter = "...1",
    deposit_rate = "Deposit facility rate",
    tr_hicp_pos0.5 = "TR - HICP, r*=0.5%",
    tr_hicp_neg1.5 = "TR - HICP, r*=-1.5%",
    tr_gdpd_pos0.5 = "TR - GDP deflator, r*=0.5%",
    tr_gdpd_neg1.5 = "TR - GDP deflator, r*=-1.5%"
  )

# convert  column quarter from string to yearqtr (year-quarter) object 
taylor_rates_Tatar <- taylor_rates_Tatar %>%
  mutate(
    quarter = ifelse(
      nchar(quarter) == 4,  # if it is year without quarter
      paste0(quarter, "-Q1"),  # add -Q1 to the end for consistency
      quarter  # otherwise leave unchanged
    ),
    quarter = as.yearqtr(quarter, format = "%Y-Q%q") # convert string to
    # yearqtr (year-quarter) object
  )

# drop deposit rate column, since it's just quarterly data: we will use
# daily data later anyways
taylor_rates_Tatar <- taylor_rates_Tatar %>% select(-deposit_rate)

# make sure quarters are sorted correctly
taylor_rates_Tatar <- taylor_rates_Tatar %>%
  arrange(quarter)

# create daily data based on quarterly data
taylor_rates_daily_not_smooth <- taylor_rates_Tatar %>%
  mutate(
    # create column for first day of every quarter
    start_date = as.Date(quarter),
    # create column for last day of every quarter (frac = 1)
    end_date = as.Date(quarter, frac = 1)
  ) %>%
  # for each row:
  rowwise() %>%
  do(
    # copy rows's quarter into a new column
    quarter = .$quarter,
    # create sequence of daily dates from start of the quarter to its end
    date = seq(from = as.Date(.$quarter), 
               to = as.Date(.$quarter, frac = 1), 
               by = "day"),
    # and copy current row's quarterly values to each of the daily date
    tr_hicp_pos0.5 = .$tr_hicp_pos0.5,
    tr_hicp_neg1.5 = .$tr_hicp_neg1.5,
    tr_gdpd_pos0.5 = .$tr_gdpd_pos0.5,
    tr_gdpd_neg1.5 = .$tr_gdpd_neg1.5
  ) %>%
  # make list of daily sequences to rows
  unnest(date) %>%
  select(-quarter) # remove quarter column

# convert columns to numeric
taylor_rates_daily_not_smooth <- taylor_rates_daily_not_smooth %>%
  mutate(
    tr_hicp_pos0.5 = as.numeric(tr_hicp_pos0.5),
    tr_hicp_neg1.5 = as.numeric(tr_hicp_neg1.5),
    tr_gdpd_pos0.5 = as.numeric(tr_gdpd_pos0.5),
    tr_gdpd_neg1.5 = as.numeric(tr_gdpd_neg1.5)
  )


# add lagged value (previous quarter) for each variable
taylor_rates_daily_smooth <- taylor_rates_Tatar %>%
  mutate(
    tr_hicp_pos0.5_lag = lag(tr_hicp_pos0.5),
    tr_hicp_neg1.5_lag = lag(tr_hicp_neg1.5),
    tr_gdpd_pos0.5_lag = lag(tr_gdpd_pos0.5),
    tr_gdpd_neg1.5_lag = lag(tr_gdpd_neg1.5)
  )

# create daily data based on quarterly data and interpolate data
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  # for each row
  rowwise() %>%
  mutate(
    # create column for first day of every quarter
    start_date = as.Date(quarter),
    # create column for last day of every quarter
    end_date = as.Date(quarter, frac = 1),
    # create column for the number of days in quarter 
    days = as.integer(end_date - start_date + 1)
  ) %>%
  # create sequence of daily dates from start of the quarter to its end
  # day_index: day number in the quarter 
  # weight_current: share of the days in the quarter that already passed
  # weight_previous: 1 - weight_current, weight applied to previous quarter value
  mutate(data = list({
    dates <- seq.Date(start_date, end_date, by = "day")
    day_index <- seq_along(dates)
    weight_current <- day_index / days
    weight_previous <- 1 - weight_current
    # create tibble for weighted values
    tibble(
      date = dates,
      tr_hicp_pos0.5 = weight_previous * tr_hicp_pos0.5_lag + weight_current * tr_hicp_pos0.5,
      tr_hicp_neg1.5 = weight_previous * tr_hicp_neg1.5_lag + weight_current * tr_hicp_neg1.5,
      tr_gdpd_pos0.5 = weight_previous * tr_gdpd_pos0.5_lag + weight_current * tr_gdpd_pos0.5,
      tr_gdpd_neg1.5 = weight_previous * tr_gdpd_neg1.5_lag + weight_current * tr_gdpd_neg1.5
    )
  })) %>%
  # keep only the data column
  select(data) %>%
  unnest(cols = c(data)) %>%
  ungroup() #remove the rowwise() structure


# add version with natural rate of interest of -0.5
taylor_rates_daily_smooth <- taylor_rates_daily_smooth %>%
  mutate(
    tr_hicp_neg0.5  = tr_hicp_pos0.5 - 1,
    tr_gdpd_neg0.5  = tr_gdpd_pos0.5 - 1,
  )

