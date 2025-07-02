library(strucchange)

sample.end.date <- as.Date("2024-12-31")
civey_taylor$Day <- as.Date(civey_taylor$Day)
data <- civey_taylor[civey_taylor$Day < sample.end.date, ]


################################################################################
####                              Estimation                               #####
################################################################################

symmetric.model <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev + ", 
                                                                   paste(control.vars, collapse= "+"))))

symmetric.results <- lm(symmetric.model, weights = Weight, data = data)
summary(symmetric.results)

symmetric.results.robust <- coeftest(symmetric.results, vcov = vcovCL(symmetric.results, cluster = ~year_month))
symmetric.results.robust

################################################################################
####                              Breakpoint Test                          #####
################################################################################

breakpoint.model <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev |", 
                                                      paste(control.vars, collapse= "+"))))

bp_partial <- breakpoints(breakpoint.model, breaks = 1, data = data)

summary(bp_partial)
plot(bp_partial)

################################################################################
####                              Graphs                                   #####
################################################################################

library(ggplot2)

ggplot(data, aes(x = Day)) +
  #### plot lines with legends via named values in aes() ####
geom_line(aes(y = tr_hicp_neg0.5, color = "TR HICP (r* = -0.5)"), size = 2) +
  geom_line(aes(y = deposit_rate, color = "Deposit Rate"), size = 2) +
  geom_line(aes(y = cpi_yoy_lag, color = "CPI YoY (lagged)"), size = 2) +
  
  #### axis labels ####
labs(x = "Day", y = "Percent", color = NULL) +
  
  #### x axis settings ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.05, 0.01))
) +
  
  #### y axis settings ####
scale_y_continuous(
  breaks = c(-4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 8.0),
  expand = c(0, 0.5)
) +
  
  #### manual colors for legend items ####
scale_color_manual(
  values = c(
    "TR HICP (r* = -0.5)" = "orange",
    "Deposit Rate" = "green",
    "CPI YoY (lagged)" = "purple"
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
  geom_hline(yintercept = 2, color = "blue", linetype = "dashed", size = 1) +
  
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
  filename = "./Plots/tr_dr_cpi.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 15,   
  # resolution
  dpi = 300                  
)



ggplot(data, aes(x = Day)) +
  #### plot lines with legends via named values in aes() ####
geom_line(aes(y = dr_dev_tr_hicp_neg0.5, color = "TR HICP (r* = -0.5) - Deposit Rate"), size = 2) +
  geom_line(aes(y = cpi_lag1_dev, color = "CPI YoY (lagged) - 2"), size = 2) +
  
  #### axis labels ####
labs(x = "Day", y = "Percent", color = NULL) +
  
  #### x axis settings ####
scale_x_date(
  breaks = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01",
                     "2022-01-01","2023-01-01", "2024-01-01", "2025-01-01")),
  date_labels = "%d-%m-%Y", 
  expand = expansion(mult = c(0.05, 0.01))
) +
  
  #### y axis settings ####
scale_y_continuous(
  breaks = c(-4.0, -2.0, 0.0, 2.0, 4.0, 6.0, 8.0),
  expand = c(0, 0.5)
) +
  
  #### manual colors for legend items ####
scale_color_manual(
  values = c(
    "TR HICP (r* = -0.5) - Deposit Rate" = "red",
    "CPI YoY (lagged) - 2" = "darkblue"
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
  filename = "./Plots/cpi_dev_tr_dev.pdf",
  plot = last_plot(),        
  device = "pdf",
  # plot dimensions (in inches)
  width = 40, height = 15,   
  # resolution
  dpi = 300                  
)

