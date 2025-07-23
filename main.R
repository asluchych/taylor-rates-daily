library(ggplot2) # plotting
library(readxl) # read xlsx files
library(dplyr) # data manipulation
library(zoo) # working with dates
library(tidyr) # data manipulation
library(purrr) # functional programming toolkit
library(sandwich) # clustered standard errors
library(lmtest) # regression output with clustered standard errors


################################################################################
####                              Data Loading                             #####
################################################################################

# start date: start of Civey survey,  January 16, 2019
sample.start.date <- as.Date("2019-01-16")
# start date: as in Coleman and Nautz (2025),  November 28, 2024
sample.end.date <- as.Date("2024-11-28")

# read data from Tatar and Wieland (2025)
taylor_rates_Tatar <- read_excel("./Data/TR_Data_Update_202503.xlsx")
# read deposit rate facility data from ECB
ECB <- read.csv("./Data/ECB Data Portal_20250618081104.csv")
# load civey data
load("./Data/civey_april.RData")

#  prepare data from Tatar and Wieland (2025)
source("taylor_rates_Tatar_data_preparation.R")
# prepare data from ECB
source("ECB_data_preparation.R")
# merge ECB and Tatar and Wieland (2025) data and add deposit rate deviations
source("ECB_Tatar_merge.R")
# add deposit rate, Taylor rates and their deviations to Civey data
source("civey_taylor_merge.R")
# prepare data for estimation
source("civey_taylor_data_preparation.R")

# drop data not used for estimation and plotting
rm(bigger_data, ECB, taylor_rates_Tatar,  taylor_rates_daily, taylor_rates_quarterly_to_daily)

################################################################################
####                            Create Graphs                              #####
################################################################################
# run to create graphs
source("graphs.R")

################################################################################
####                           Specify Models                              #####
################################################################################

# control variables
control.vars <- c("Female", "East", "College", "Age50Plus", "JobPosition", 
                  "MaritalStatus", "EmploymentStatus")

# Coleman and Nautz (2025)
cn2025.formula <- as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                  paste(control.vars, collapse= "+"))))
# Taylor rate deviations based on HICP with r* = -1.5 + Coleman and Nautz (2025)
tr_hicp_neg1.5_dev_dr_abs.formula <- as.formula(paste("Cred ~ ", paste("tr_hicp_neg1.5_dev_dr_abs + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                    paste(control.vars, collapse= "+"))))
# Taylor rate deviations based on HICP with r* = -0.5 + Coleman and Nautz (2025)
tr_hicp_neg0.5_dev_dr_abs.formula <- as.formula(paste("Cred ~ ", paste("tr_hicp_neg0.5_dev_dr_abs + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                       paste(control.vars, collapse= "+"))))
# Taylor rate deviations based on HICP with r* = 0.5 + Coleman and Nautz (2025)
tr_hicp_pos0.5_dev_dr_abs.formula <- as.formula(paste("Cred ~ ", paste("tr_hicp_pos0.5_dev_dr_abs + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                       paste(control.vars, collapse= "+"))))
################################################################################
####                           Estimate Models                             #####
################################################################################
# run to estimate linear probability models
source("lpm.R")



################################################################################
####                            Missing Data Treatment                     #####
################################################################################

# calculate share of rows with missing values
mean(rowSums(is.na(civey)) > 0)
# around 12.39% of rows have missing values

# drop rows with with at least one missing value
civey <- civey[complete.cases(civey), ]

################################################################################
####                        Response Variable Treatment 'N'               #####
################################################################################

# calculate share of Responses 'N'
mean(civey$Response == "N")
# around 8.39% of responses are 'N'

# drop Responses 'N'
civey <- civey[civey$Response != "N", ]

# convert Response variable to factor
civey$Response <- as.factor(civey$Response)

civey$infl_expect_ord <- civey$Response

civey$infl_expect_ord <- fct_collapse(civey$infl_expect_ord,
                                      "ITC" = c("B", "C"))
# make D the reference category
civey$infl_expect_ord <- relevel(civey$infl_expect_ord, ref = "D")

# ordered factor for Response variable
civey$infl_expect_ord <- as.ordered(civey$infl_expect_ord)
# change ordering of factor levels: A>ITCC>D
civey$infl_expect_ord <- factor(civey$infl_expect_ord, levels = c("D", "ITC", "A"), 
                                 ordered = TRUE)