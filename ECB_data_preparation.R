# give more sensible names to columns 
ECB <- ECB %>% 
  rename(
    date = "DATE",
    deposit_rate = "Deposit.facility...date.of.changes..raw.data....Level..FM.D.U2.EUR.4F.KR.DFR.LEV.",
  )
# drop TIME.PERIOD column
ECB <- ECB %>% select(-TIME.PERIOD)

# convert deposit_rate values to numeric and date to Date
ECB <- ECB %>%
  mutate(
    deposit_rate = as.numeric(deposit_rate),
    date = as.Date(date)
    )