daily_tatar <- taylor_rates_Tatar %>%
  mutate(start_date = as.Date(quarter, frac = 0)) %>%
  rowwise() %>%
  mutate(
    date = list(seq.Date(start_date, by = "day", length.out = 91))
  ) %>%
  unnest(date) %>%
  select(-start_date, -quarter, -deposit_rate)

taylor_rates_Tim <- taylor_rates_Tim %>%
  mutate(date = as.Date(date))

merged_data <- left_join(taylor_rates_Tim, daily_tatar, by = "date")




ggplot(merged_data, aes(x = date)) +
  geom_line(aes(y = taylor_rate_0), color = "blue", size = 2) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  labs(
    x = "Day",
    y = "Rate"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01", "2025-01-01")),
    date_labels = "%d-%m-%Y",
    expand = c(0, 30)
  ) +
  scale_y_continuous(
    breaks = c(-8.0, -4.0,  0.0,  4.0,  8.0, 12.0),
    expand = c(0, 2.4)
  ) +
  geom_hline(
    # position of horizontal line
    yintercept = 0,  
    color = "black", 
    linetype = "solid", 
    # line thickness
    size = 0.5  
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5)            
  )
ggsave(
  filename = "./plots/taylor_deposit_shadow_plot.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,
  # resolution
  dpi = 300                 
)