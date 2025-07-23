library(strucchange)

sample.end.date <- as.Date("2024-12-31")
civey_taylor$Day <- as.Date(civey_taylor$Day)
breakpoint_data <- civey_taylor[civey_taylor$Day <= sample.end.date, ]

break.date <- as.Date("2021-02-01")

################################################################################
####                              Estimation: full sample                  #####
################################################################################

symmetric.model <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev + ", 
                                                                   paste(control.vars, collapse= "+"))))

symmetric.full.sample.results <- lm(symmetric.model, weights = Weight, data = breakpoint_data)
summary(symmetric.full.sample.results)

symmetric.full.sample.results.robust <- coeftest(symmetric.full.sample.results, vcov = vcovCL(symmetric.full.sample.results, cluster = ~year_month))
symmetric.full.sample.results.robust




symmetric.model.cpi.dummy <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5 + I(cpi_lag1_over4_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev + ", 
                                                     paste(control.vars, collapse= "+"))))
symmetric.model.cpi.dummy.results <- lm(symmetric.model.cpi.dummy, weights = Weight, data = breakpoint_data)
summary(symmetric.model.cpi.dummy.results)

symmetric.model.cpi.dummy.results.robust <- coeftest(symmetric.model.cpi.dummy.results, vcov = vcovCL(symmetric.model.cpi.dummy.results, cluster = ~year_month))
symmetric.model.cpi.dummy.results.robust




symmetric.model.dr.cpi.interaction <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5 + I(cpi_lag1_dev*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev + ", 
                                                               paste(control.vars, collapse= "+"))))
symmetric.model.dr.cpi.interaction.results <- lm(symmetric.model.dr.cpi.interaction, weights = Weight, data = breakpoint_data)
summary(symmetric.model.dr.cpi.interaction.results)

symmetric.model.dr.cpi.interaction.results <- coeftest(symmetric.model.dr.cpi.interaction.results, vcov = vcovCL(symmetric.model.dr.cpi.interaction.results, cluster = ~year_month))
symmetric.model.dr.cpi.interaction.results

symmetric.model.cpi.dummy <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5 + I(cpi_lag1_over4_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev + ", 
                                                               paste(control.vars, collapse= "+"))))
symmetric.model.cpi.dummy.results <- lm(symmetric.model.cpi.dummy, weights = Weight, data = breakpoint_data)
summary(symmetric.model.cpi.dummy.results)

symmetric.model.cpi.dummy.results.robust <- coeftest(symmetric.model.cpi.dummy.results, vcov = vcovCL(symmetric.model.cpi.dummy.results, cluster = ~year_month))
symmetric.model.cpi.dummy.results.robust






################################################################################
####                              Estimation:  before break                #####
################################################################################

before.break.sample.results <- lm(symmetric.model, weights = Weight, data = breakpoint_data[breakpoint_data$Day <= break.date, ])
summary(before.break.sample.results)

before.break.sample.results.robust <- coeftest(before.break.sample.results, vcov = vcovCL(before.break.sample.results, cluster = ~year_month))
before.break.sample.results.robust


################################################################################
####                              Estimation:  after break                 #####
################################################################################

after.break.sample.results <- lm(symmetric.model, weights = Weight, data = breakpoint_data[breakpoint_data$Day > break.date, ])
summary(after.break.sample.results)

after.break.sample.results.robust <- coeftest(after.break.sample.results, vcov = vcovCL(after.break.sample.results, cluster = ~year_month))
after.break.sample.results.robust

################################################################################
####                              Estimation:  Extended Model              #####
################################################################################


extended.model <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_over2_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_under2_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev_abs +", 
                                                               paste(control.vars, collapse= "+"))))
extended.model.results <- lm(extended.model, weights = Weight, data =  breakpoint_data)
summary(extended.model.results)

extended.model.results.robust <- coeftest(extended.model.results, vcov = vcovCL(extended.model.results, cluster = ~year_month))
extended.model.results.robust


################################################################################
####                              Breakpoint Test                          #####
################################################################################

breakpoint.model <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev |", 
                                                      paste(control.vars, collapse= "+"))))

bp_partial <- breakpoints(breakpoint.model, breaks = 1, data = breakpoint_data)

summary(bp_partial)
plot(bp_partial)

################################################################################
####                              Graphs                                   #####
################################################################################

library(ggplot2)

ggplot(breakpoint_data, aes(x = Day)) +
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



ggplot(breakpoint_data, aes(x = Day)) +
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

extended.model <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_under4_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_over4_dummy*dr_dev_tr_hicp_neg0.5) + 
                                                    cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                    paste(control.vars, collapse= "+"))))
extended.model.results <- lm(extended.model, weights = Weight, data = breakpoint_data)
summary(extended.model.results)

extended.model.results.robust <- coeftest(extended.model.results, vcov = vcovCL(extended.model.results, cluster = ~year_month))
extended.model.results.robust
