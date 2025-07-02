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

# sample.start.date <- as.Date("2021-06-01")
# sample.end.date <- as.Date("2024-12-31")
# civey_taylor$Day <- as.Date(civey_taylor$Day)
# sample <- civey_taylor[civey_taylor$Day > sample.start.date & civey_taylor$Day < sample.end.date, ]



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

# add deposit rate, Taylor rates and their absolute deviations to Civey data
source("civey_taylor_merge.R")

# prepare data for estimation
source("civey_taylor_data_preparation.R")



################################################################################
####                           Specify Models                              #####
################################################################################

# control variables
control.vars <- c("Female", "East", "College", "Age50Plus", "JobPosition", 
                  "MaritalStatus", "EmploymentStatus")

#### model with inflation target deviations: Coleman and Nautz (2025): Equation (3) ####

# binary inflation target credibility: symmetric interest rate deviations
itc.symmetric.ir <- list(itc.base = as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                       paste(control.vars, collapse= "+")))),
                         itc.symmetric.hicp_pos0.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_pos0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+")))),
                         itc.symmetric.hicp_neg0.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+")))),
                         itc.symmetric.hicp_neg0.5_no_infl = as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.symmetric.hicp_neg0.5_infl = as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev + ", 
                                                                                               paste(control.vars, collapse= "+")))),
                         itc.symmetric.hicp_neg1.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg1.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+")))),
                         itc.symmetric.gdpd_pos0.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_gdpd_pos0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+")))),
                         itc.symmetric.gdpd_neg0.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_gdpd_neg0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+")))),
                         itc.symmetric.gdpd_neg1.5 = as.formula(paste("Cred ~ ", paste("dr_dev_tr_gdpd_neg1.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                        paste(control.vars, collapse= "+"))))
                         )


itc.asymmetric.ir <- list(itc.base = as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                      paste(control.vars, collapse= "+")))),
                         itc.asymmetric.hicp_pos0.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_hicp_pos0.5  + dr_neg_dev_tr_hicp_pos0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.asymmetric.hicp_neg0.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_hicp_neg0.5  + dr_neg_dev_tr_hicp_neg0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.asymmetric.hicp_neg1.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_hicp_neg1.5  + dr_neg_dev_tr_hicp_neg1.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.asymmetric.gdpd_pos0.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_gdpd_pos0.5  + dr_neg_dev_tr_gdpd_pos0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.asymmetric.gdpd_neg0.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_gdpd_neg0.5   +  dr_neg_dev_tr_gdpd_neg0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+")))),
                         itc.asymmetric.gdpd_neg1.5 = as.formula(paste("Cred ~ ", paste("dr_pos_dev_tr_gdpd_neg1.5  +  dr_neg_dev_tr_gdpd_neg1.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                                                       paste(control.vars, collapse= "+"))))
)

################################################################################
####                           Estimate Models                             #####
################################################################################
# run to estimate linear probability models
source("lpm.R")

# run to estimate logit models
source("logit.R")

# run to estimate ordered logit models
source("ordered.R")

################################################################################
####                            Create Graphs                              #####
################################################################################
# run to create graphs
source("graphs.R")


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

################################################################################
####                           Specify Models                              #####
################################################################################

# control variables
control.vars <- c("Female", "East", "College", "Age50Plus", "JobPosition", 
                  "MaritalStatus", "EmploymentStatus")



#### model with inflation target deviations: Coleman and Nautz (2025): Equation (3) ####

# binary inflation target credibility
itc.m1 <- as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                           paste(control.vars, collapse= "+"))))
# ordered response
ordered.m1 <- as.formula(paste("infl_expect_ord ~ ",
                               paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                     paste(control.vars, collapse= "+"))))
#### model with inflation target deviations and deviations of deposit rate from ####
# Taylor rule implied rate

# binary inflation target credibility
itc.m2 <- as.formula(paste("Cred ~ ", 
                           paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ir_dev +", 
                                 paste(control.vars, collapse= "+"))))
# ordered response
ordered.m2 <- as.formula(paste("infl_expect_ord ~ ", 
                               paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ir_dev +", 
                                     paste(control.vars, collapse= "+"))))

##### model with inflation target deviations, deviations of deposit ####
# rate from Taylor rule implied rate and deflationary months

# binary inflation target credibility:
itc.m3 <- as.formula(paste("Cred ~ ", 
                           paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ir_dev + cpi_lag1_dev_abs_defl +", 
                                 paste(control.vars, collapse= "+"))))

# ordered response
ordered.m3 <- as.formula(paste("infl_expect_ord ~ ", 
                               paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ir_dev + cpi_lag1_dev_abs_defl +", 
                                     paste(control.vars, collapse= "+"))))

################################################################################
####                           Estimate Models                             #####
################################################################################
# run to estimate linear probability models
source("lpm.R")

# run to estimate logit models
source("logit.R")

# run to estimate ordered logit models
source("ordered.R")

################################################################################
####                            Create Graphs                              #####
################################################################################
# run to create graphs
source("graphs.R")

