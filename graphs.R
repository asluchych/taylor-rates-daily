################################################################################
####               Taylor Rate, HICP, r* = -0.5 vs deposit rate            #####
################################################################################

ggplot(civey_taylor, aes(x = Day)) +
  #### plot lines with legends via named values in aes() ####
geom_line(aes(y = tr_hicp_neg0.5, color = "Taylor Rate, r* = -0.5"), size = 2) +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), size = 2) +
  #### axis labels ####
labs(x = "Day", y = "Percent", color = NULL) +
  #### x axis settings ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.05, 0.03))
) +
  #### y axis settings ####
scale_y_continuous(
  breaks = c(-2.0, 0.0, 2.0, 4.0, 6.0, 8.0),
  expand = expansion(mult = c(0.08, 0.14))
) +
  
  #### manual colors for legend items ####
scale_color_manual(
  values = c(
    "Taylor Rate, r* = -0.5" = "blue",
    "Deposit Rate" = "green"
    )
) +
  
  #### reference lines and shaded area ####
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  annotate("rect",
           xmin = as.Date("2020-01-01"),
           xmax = as.Date("2021-09-30"),
           ymin = -Inf, ymax = Inf,
           alpha = 0.2, fill = "lightgrey") +

  #### theme and styling ####
theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 44),
    axis.text.y = element_text(size = 44), 
    axis.title = element_text(size = 44), 
    plot.title = element_text(size = 44, hjust = 0.5),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 44),
    legend.title = element_text(size = 44)
  )

# save plot
ggsave(
  filename = "./Plots/tr_hicp_neg0.5_vs_deposit_rate.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 15,   
  # resolution
  dpi = 300                  
)













ggplot(civey_taylor, aes(x = Day)) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### add plot lines ####
geom_line(aes(y = tr_hicp_neg0.5, color = "Taylor Rate, r* = -0.5"), size = 2) +  
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), size = 2) + 
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.05, 0.03))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c( -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = expansion(mult = c(0.1, 0.2))
) +
  #### manual colors for legend items ####
scale_color_manual(
  values = c(
    "Taylor Rate, r* = -0.5" = "blue",
    "Deposit Rate" = "green"
   ) 
  ) + 
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 44),
    axis.text.y = element_text(size = 44), 
    axis.title = element_text(size = 44), 
    plot.title = element_text(size = 44, hjust = 0.5),
    legend.position = c(1, 1),
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 44),
    legend.title = element_text(size = 44)
  )
# save plot
ggsave(
  filename = "./Plots/tr_hicp_neg0.5_vs_deposit_rate.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)



#### HICP -0.5, deposit rate and deviations ####
ggplot(civey_taylor, aes(x = Day)) +
  #### add plot lines ####
geom_line(aes(y = tr_hicp_pos0.5), color = "blue", size = 2) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = dr_dev_tr_hicp_neg0.5), 
            color = "red",  size = 2) +  
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.03, 0.005))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2026-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/hicp_neg0.5_deposit_rate_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)
#### HICP -1.5, deposit rate and deviations ####
ggplot(civey_taylor, aes(x = Day)) +
  #### add plot lines ####
geom_line(aes(y = tr_hicp_neg1.5), color = "orange", size = 2) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = dr_dev_tr_hicp_neg1.5), 
            color = "red",  size = 2) +  
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.03, 0.005))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2026-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/hicp_neg1.5_deposit_rate_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)


#### GDP deflator +0.5, deposit rate and deviations ####
ggplot(civey_taylor, aes(x = Day)) +
  #### add plot lines ####
geom_line(aes(y = tr_gdpd_pos0.5), color = "purple", size = 2, linetype = 5) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = dr_dev_tr_gdpd_pos0.5), 
            color = "red",  size = 2) +  
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.03, 0.005))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2026-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/gdpd_pos0.5_deposit_rate_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)


#### GDP deflator -0.5, deposit rate and deviations ####
ggplot(civey_taylor, aes(x = Day)) +
  #### add plot lines ####
geom_line(aes(y = tr_gdpd_pos0.5), color = "purple", size = 2, linetype = 5) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = dr_dev_tr_gdpd_neg0.5), 
            color = "red",  size = 2) +  
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.03, 0.005))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2026-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/gdpd_neg0.5_deposit_rate_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)

#### GDP deflator -1.5, deposit rate and deviations ####
ggplot(civey_taylor, aes(x = Day)) +
  #### add plot lines ####
geom_line(aes(y = tr_gdpd_neg1.5), color = "brown4", size = 2, linetype = 5) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = dr_dev_tr_gdpd_neg1.5), 
            color = "red",  size = 2) +  
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.03, 0.005))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2026-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/gdpd_neg1.5_deposit_rate_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)

















####                              CPI: cpi_plot                             ####
ggplot(inflation, aes(x = Date, y = CPI)) +
  geom_line(color = "blue", size = 2) +  
  labs(
    x = "Month",
    y = "CPI, in %"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
    date_labels = "%b %Y",
    expand = c(0, 10)
  ) +
  scale_y_continuous(
    breaks = c(-2.0, 0.0, 2.0, 4.0, 6.0, 8.0, 10.0),
    expand = c(0, 2)
  ) +
  geom_hline(
    # position of horizontal line
    yintercept = 2,  
    color = "green", 
    linetype = "dashed",
    # line thickness
    size = 2  
  ) +
  geom_hline(
    # position of horizontal line
    yintercept = 0,  
    color = "black", 
    linetype = "solid", 
    # line thickness
    size = 0.5  
  ) +
  geom_vline(
    xintercept =  as.Date("2021-07-08"),
    color = "grey", 
    linetype = "dashed",
    # line thickness
    size = 1  
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
  filename = "./plots/cpi_plot.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)

####              absolute inflation target deviations: itd_abs_plot        ####
ggplot(inflation, aes(x = Date, y = ITD.abs)) +
  geom_line(color = "blue", size = 2) +  
  labs(
    x = "Month",
    y = "ITD (absolute)"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
    date_labels = "%d-%m-%Y",
    expand = c(0, 20)
  ) +
  scale_y_continuous(
    breaks = c(0.0, 2.0, 4.0, 6.0, 8.0),
    expand = c(0, 0.2)
  ) +
  geom_vline(
    xintercept =  as.Date("2021-07-08"),
    color = "grey", 
    linetype = "dashed", 
    # line thickness
    size = 1  
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
  filename = "./plots/itd_abs_plot.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8, 
  # resolution
  dpi = 300                  
)
####           Taylor rate and deposit rate: taylor_deposit_plot           ####
ggplot(interest_daily_data, aes(x = date)) +
  geom_line(aes(y = taylor_rate_0), color = "green", size = 2) +  
  geom_line(aes(y = deposit_rate), color = "blue", size = 2) +  
  labs(
    x = "Day",
    y = "Rate"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
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
  filename = "./plots/taylor_deposit_plot.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8, 
  # resolution
  dpi = 300                  
)


#### Taylor rate, deposit rate and shadow rate: taylor_deposit_shadow_plot  ####
ggplot(interest_daily_data, aes(x = date)) +
  geom_line(aes(y = taylor_rate_0), color = "blue", size = 2) +  
  geom_line(aes(y = deposit_rate), color = "green", size = 2) + 
  geom_line(aes(y = shadow_rate), color = "orange", size = 2) +  
  labs(
    x = "Day",
    y = "Rate"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
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


#### absolute deviations of deposit rate from Taylor rate: deposit_deviations_plot ####
ggplot(interest_daily_data, aes(x = date)) +
  geom_line(aes(y = DRD.abs), color = "blue", size = 2) + 
  labs(
    x = "Day",
    y = "Rate"
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
    date_labels = "%d-%m-%Y",
    expand = c(0, 30)
  ) +
  scale_y_continuous(
    breaks = c(0.0, 2.0, 4.0, 6.0, 8.0, 10.0, 12.0),
    expand = c(0, 0.5)
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
  filename = "./plots/deposit_deviations_plot.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8, 
  # resolution
  dpi = 300                  
)


####                plot CPI over time: cpi_deviations                      ####
ggplot(inflation[1:63, ], aes(x = Date, y = CPI - 2)) +
  geom_line(color = "red", size = 2) +  
  labs(
    x = "",
    y = "p. p."
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
    date_labels = "%d-%m-%Y",
    expand = c(0, 40)
  ) +
  scale_y_continuous(
    breaks = c(-4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 8.0, 10.0),
    expand = c(0, 2)
  ) +
  geom_hline(
    # position of horizontal line
    yintercept = 0,  
    color = "black", 
    linetype = "solid", 
    # line thickness
    size = 0.5  
  ) +
  geom_vline(
    xintercept =  as.Date("2021-07-08"),
    color = "grey", 
    linetype = "dashed", 
    # line thickness
    size = 1  
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
  filename = "./plots/cpi_deviations.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)


####               rate differences: deposit_deviations                     ####
ggplot(interest_daily_data, aes(x = date)) +
  geom_line(aes(y = deposit_rate - taylor_rate_0), color = "orange", size = 2) +  
  labs(
    x = "",
    y = "Rate diff."
  ) +
  scale_x_date(
    breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01", "2022-01-01",
                       "2023-01-01", "2024-01-01")),
    date_labels = "%d-%m-%Y",
    expand = c(0, 40)
  ) +
  scale_y_continuous(
    breaks = c(-10.0, -8.0, -6.0, -4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 8.0),
    expand = c(0, 0.5)
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
  filename = "./plots/deposit_deviations.pdf",
  plot = last_plot(),       
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)








################################################################################
####                              Graphs                                   #####
################################################################################

#### HICP based Taylor rates smoothed vs not smoothed ####
ggplot(taylor_rates_daily_smooth, aes(x = date)) +
  #### add plot lines ####
geom_line(aes(y = tr_hicp_pos0.5), color = "blue", size = 2) +  
  geom_line(aes(y = tr_hicp_neg1.5), color = "orange", size = 2) + 
  geom_line(aes(y = taylor_rates_daily_not_smooth$tr_hicp_pos0.5), 
            color = "blue",  size = 1, linetype = 2) +  
  geom_line(aes(y = taylor_rates_daily_not_smooth$tr_hicp_neg1.5), 
            color = "orange", size = 1, linetype = 2) +    
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01", "2027-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.015, 0.0025))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2027-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/hicp_smooth_not_smooth.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)

#### HICP based Taylor rates smoothed vs not smoothed ####
ggplot(taylor_rates_daily_smooth, aes(x = date)) +
  #### add plot lines ####
geom_line(aes(y = tr_gdpd_pos0.5), color = "purple", size = 2, linetype = 5) +  
  geom_line(aes(y = tr_gdpd_neg1.5), color = "brown4", size = 2, linetype = 5) + 
  geom_line(aes(y = taylor_rates_daily_not_smooth$tr_gdpd_pos0.5), 
            color = "purple",  size = 1, linetype = 2) +  
  geom_line(aes(y = taylor_rates_daily_not_smooth$tr_gdpd_neg1.5), 
            color = "brown4", size = 1, linetype = 2) +    
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01", "2027-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.015, 0.0025))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2027-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/gdp_defl_smooth_not_smooth.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)

####  Taylor rates smoothed  ####
ggplot(taylor_rates_daily_smooth, aes(x = date)) +
  #### add plot lines ####
geom_line(aes(y = tr_gdpd_pos0.5), color = "purple", size = 2, linetype = 5) +  
  geom_line(aes(y = tr_gdpd_neg1.5), color = "brown4", size = 2, linetype = 5) + 
  geom_line(aes(y = tr_hicp_pos0.5), color = "blue", size = 2) +  
  geom_line(aes(y = tr_hicp_neg1.5), color = "orange", size = 2) +    
  #### give axis names ####
labs(x = "Day", y = "Percent") +
  #### adjust x axis ####
scale_x_date(
  breaks = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01",
                     "2026-01-01", "2027-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.015, 0.0025))
) +
  #### adjust y axis ####
scale_y_continuous(
  breaks = c(-4.0, -2.0,  0.0, 2.0, 6.0,  4.0,  8.0),
  expand = c(0, 0.5)
) +
  #### add custom lines ####
# add clearly (thickness of 0.5) visible y-axis
geom_hline(yintercept = 0, color = "black", linetype = "solid", size = 0.5) +
  # add missing grid line at y=3
  geom_hline(yintercept = 3, color = "grey90", linetype = "solid", size = 0.5) +
  # add missing grid line at y=7
  geom_hline(yintercept = 7, color = "grey90", linetype = "solid", size = 0.5) +
  # add border for start of adjusted output gap
  geom_vline(xintercept = as.Date("2020-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for end of adjusted output gap
  geom_vline(xintercept = as.Date("2021-09-30"), color = "black", linetype = "dashed", size = 0.5) +
  # add border for start of projected data
  geom_vline(xintercept = as.Date("2025-01-01"), color = "black", linetype = "dashed", size = 0.5) +
  #### fill the area for adjusted output gap ####
annotate("rect",
         xmin = as.Date("2020-01-01"),
         xmax = as.Date("2021-09-30"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightgrey") +
  #### fill the area for projected data ####
annotate("rect",
         xmin = as.Date("2025-01-01"),
         xmax = as.Date("2027-01-01"),
         ymin = -Inf, ymax = Inf,
         alpha = 0.2, fill = "lightpink") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 30),
    # y-axis font size
    axis.text.y = element_text(size = 30), 
    # axis title size
    axis.title = element_text(size = 30), 
    # plot title size
    plot.title = element_text(size = 30, hjust = 0.5),
  )
# save plot
ggsave(
  filename = "./Plots/taylor_rates.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 8,   
  # resolution
  dpi = 300                  
)