daily_avg <- working_data %>% select(Cred, Weight, date, cpi_lag1_over2_dummy,
                                     dr_dev_tr_hicp_neg0.5, cpi_lag1_under2_dummy)

daily_avg <- na.omit(daily_avg)

daily_avg <- daily_avg %>%
  group_by(date) %>%
  summarise(
    weighted_cred = sum(Cred * Weight) / sum(Weight),
    cpi_lag1_over2_dummy = first(cpi_lag1_over2_dummy),
    cpi_lag1_under2_dummy = first(cpi_lag1_under2_dummy),
    dr_dev_tr_hicp_neg0.5 = first(dr_dev_tr_hicp_neg0.5)
  ) %>%
  ungroup()

daily_avg <- daily_avg %>%
  arrange(date) %>%
  mutate(
    smooth_cred = rollmean(weighted_cred, k = 45, fill = NA, align = "right")
  )

ggplot(daily_avg, aes(x = date)) +
  geom_line(aes(y = smooth_cred), color = "green", size = 2, linetype = "solid") + 
  geom_line(aes(y = smooth_cred), color = "green", size = 2, linetype = "solid") + 
  labs(
    x = "Day",
    y = "Rate"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01", "2025-01-01")),
    date_labels = "%d-%m-%Y",
    expand = expansion(mult = c(0.02, 0.005))
  ) +
  scale_y_continuous(
    breaks = c(0, 20, 40, 60, 80),
    expand = expansion(mult = c(0, 0.12))
  ) +
  geom_hline(
    yintercept = 0,  
    color = "black", 
    linetype = "solid",
    size = 0.5  
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    axis.text.y = element_text(size = 30), 
    axis.title = element_text(size = 30), 
    plot.title = element_text(size = 30, hjust = 0.5)            
  )

# STEP 4: Save
ggsave(
  filename = "./plots/inflation_credibility_smoothed.pdf",
  plot = last_plot(),        
  device = "pdf",
  width = 40, height = 8, 
  dpi = 300                  
)
