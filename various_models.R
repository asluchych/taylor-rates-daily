lpm.estimation <- function(formula, data, weight, cluster) {
  # Add weights column to a new local copy of data with fixed column name
  data$.__weight__. <- data[[weight]]
  
  # Fit model using renamed weight column
  model <- lm(formula, weights = .__weight__., data = data)
  
  # Robust standard errors with clustering
  robust_se <- coeftest(model, vcov = vcovCL(model, cluster = data[[cluster]]))
  
  return(list(model = model, robust = robust_se))
}


results.symmetric.ir <- lapply(itc.symmetric.ir, function(f) {
  lpm.estimation(formula = f, data = sample, weight = "Weight", cluster = "year_month")
})




results.asymmetric.ir <- lapply(itc.asymmetric.ir, function(f) {
  lpm.estimation(formula = f, data = civey_taylor, weight = "Weight", cluster = "year_month")
})


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
####    Linear Probability Models: Symmetric Interest Rate Deviations      #####
################################################################################

#### (1) inflation target deviations (Coleman, Nautz (2025), Equation (3)) ####
lpm.base <- lm(itc.base, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.base)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.base.robust <- coeftest(lpm.base, vcov =  vcovCL(lpm.base, 
                                                     cluster = ~year_month))
lpm.base.robust


#### (2)  ####
lpm.hicp_pos0.5 <- lm(itc.hicp_pos0.5, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.hicp_pos0.5.robust <- coeftest(lpm.hicp_pos0.5, vcov =  vcovCL(lpm.hicp_pos0.5, 
                                                                   cluster = ~year_month))
lpm.hicp_pos0.5.robust

#### (3)  ####
lpm.hicp_neg0.5 <- lm(as.formula(paste("Cred ~ ", paste("dr_dev_abs_tr_hicp_pos0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                        paste(control.vars, collapse= "+")))), weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.hicp_neg0.5.robust <- coeftest(lpm.hicp_neg0.5, vcov =  vcovCL(lpm.hicp_neg0.5, 
                                                                   cluster = ~year_month))
lpm.hicp_neg0.5.robust
AIC(lpm.hicp_neg0.5.robust)

#### (4)  ####
lpm.hicp_neg1.5 <- lm(itc.hicp_neg1.5, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.hicp_neg1.5.robust <- coeftest(lpm.hicp_neg1.5, vcov =  vcovCL(lpm.hicp_neg1.5, 
                                                                   cluster = ~year_month))
lpm.hicp_neg1.5.robust
AIC(lpm.hicp_neg1.5.robust)


#### (5)  ####
lpm.gdpd_pos0.5 <- lm(itc.gdpd_pos0.5, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.gdpd_pos0.5.robust <- coeftest(lpm.gdpd_pos0.5, vcov =  vcovCL(lpm.gdpd_pos0.5, 
                                                                   cluster = ~year_month))
lpm.gdpd_pos0.5.robust

#### (6)  ####
lpm.gdpd_neg0.5 <- lm(itc.gdpd_neg0.5, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.gdpd_neg0.5.robust <- coeftest(lpm.gdpd_neg0.5, vcov =  vcovCL(lpm.gdpd_neg0.5, 
                                                                   cluster = ~year_month))
lpm.gdpd_neg0.5.robust

#### (7)  ####
lpm.gdpd_neg1.5 <- lm(itc.gdpd_neg1.5, weights = Weight, data = civey_taylor)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.gdpd_neg1.5.robust <- coeftest(lpm.gdpd_neg1.5, vcov =  vcovCL(lpm.gdpd_neg1.5, 
                                                                   cluster = ~year_month))
lpm.gdpd_neg1.5.robust
AIC(lpm.gdpd_neg1.5.robust)


##########







lpm.base <- lm(as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                               paste(control.vars, collapse= "+")))), weights = Weight, data = working_data)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.base)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.base.robust <- coeftest(lpm.base, vcov =  vcovCL(lpm.base, cluster = ~year_month))
lpm.base.robust


lpm.dr.dev.hicp_neg1.5 <- lm(as.formula(paste("Cred ~ ", paste("dr_dev_abs_tr_hicp_neg1.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                        paste(control.vars, collapse= "+")))), weights = Weight, data = working_data)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.dr.dev.hicp_neg1.5.robust <- coeftest(lpm.dr.dev.hicp_neg1.5, vcov =  vcovCL(lpm.dr.dev.hicp_neg1.5, 
                                                                   cluster = ~year_month))
lpm.dr.dev.hicp_neg1.5.robust


lpm.hicp_neg0.5 <- lm(as.formula(paste("Cred ~ ", paste("dr_dev_abs_tr_hicp_neg0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                        paste(control.vars, collapse= "+")))), weights = Weight, data = working_data)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.hicp_neg0.5.robust <- coeftest(lpm.hicp_neg0.5, vcov =  vcovCL(lpm.hicp_neg0.5, 
                                                                   cluster = ~year_month))
lpm.hicp_neg0.5.robust


lpm.hicp_pos0.5 <- lm(as.formula(paste("Cred ~ ", paste("dr_dev_abs_tr_hicp_pos0.5  + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                        paste(control.vars, collapse= "+")))), weights = Weight, data = working_data)
# warning: wrong standard errors not clustered at monthly level
# summary(lpm.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
lpm.hicp_pos0.5.robust <- coeftest(lpm.hicp_pos0.5, vcov =  vcovCL(lpm.hicp_pos0.5, 
                                                                   cluster = ~year_month))
lpm.hicp_pos0.5.robust


working.model <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_over2_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_under2_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev_abs +", 
                                                    paste(control.vars, collapse= "+"))))

################################################################################
####                               Full Sample                             #####
################################################################################

working.model.full.sample <- lm(working.model, weights = Weight, data =  working_data)
summary(working.model.full.sample)

working.model.full.sample.robust <- coeftest(working.model.full.sample, vcov = vcovCL(working.model.full.sample, cluster = ~year_month))
working.model.full.sample.robust

################################################################################
####                               Female                                  #####
################################################################################

working.model.female <- lm(working.model, weights = Weight, data =  working_data[working_data$Female == 1, ])
summary(working.model.female)

working.model.female.robust <- coeftest(working.model.female, vcov = vcovCL(working.model.female, cluster = ~year_month))
working.model.female.robust

################################################################################
####                               Male                                    #####
################################################################################

working.model.male <- lm(working.model, weights = Weight, data =  working_data[working_data$Female == 0, ])
summary(working.model.male)

working.model.male.robust <- coeftest(working.model.male, vcov = vcovCL(working.model.male, cluster = ~year_month))
working.model.male.robust

################################################################################
####                               East                                    #####
################################################################################

working.model.east <- lm(working.model, weights = Weight, data =  working_data[working_data$East == 1, ])
summary(working.model.east)

working.model.east.robust <- coeftest(working.model.east, vcov = vcovCL(working.model.east, cluster = ~year_month))
working.model.east.robust

################################################################################
####                               West                                    #####
################################################################################

working.model.west <- lm(working.model, weights = Weight, data =  working_data[working_data$East == 0, ])
summary(working.model.west)

working.model.west.robust <- coeftest(working.model.west, vcov = vcovCL(working.model.west, cluster = ~year_month))
working.model.west.robust

################################################################################
####                               College                                #####
################################################################################

working.model.college <- lm(working.model, weights = Weight, data =  working_data[working_data$College == 1, ])
summary(working.model.college)

working.model.college.robust <- coeftest(working.model.college, vcov = vcovCL(working.model.college, cluster = ~year_month))
working.model.college.robust

################################################################################
####                               No College                              #####
################################################################################

working.model.nocollege <- lm(working.model, weights = Weight, data =  working_data[working_data$College == 0, ])
summary(working.model.nocollege)

working.model.nocollege.robust <- coeftest(working.model.nocollege, vcov = vcovCL(working.model.nocollege, cluster = ~year_month))
working.model.nocollege.robust

################################################################################
####                             Age over 50                               #####
################################################################################

working.model.over50 <- lm(working.model, weights = Weight, data =  working_data[working_data$Age50Plus == 1, ])
summary(working.model.over50)

working.model.over50.robust <- coeftest(working.model.over50, vcov = vcovCL(working.model.over50, cluster = ~year_month))
working.model.over50.robust

################################################################################
####                             Age under 50                              #####
################################################################################

working.model.under50 <- lm(working.model, weights = Weight, data =  working_data[working_data$Age50Plus == 0, ])
summary(working.model.under50)

working.model.under50.robust <- coeftest(working.model.under50, vcov = vcovCL(working.model.under50, cluster = ~year_month))
working.model.under50.robust

################################################################################
####                               No AfD                                  #####
################################################################################

working.model.noafd <- lm(working.model, weights = Weight, data =  working_data[working_data$Vote != "AfD", ])
summary(working.model.noafd)

working.model.noafd.robust <- coeftest(working.model.noafd, vcov = vcovCL(working.model.noafd, cluster = ~year_month))
working.model.noafd.robust

################################################################################
####                               AfD                                     #####
################################################################################

working.model.afd <- lm(working.model, weights = Weight, data =  working_data[working_data$Vote == "AfD", ])
summary(working.model.afd)

working.model.afd.robust <- coeftest(working.model.afd, vcov = vcovCL(working.model.afd, cluster = ~year_month))
working.model.afd.robust

################################################################################
####                               Basic                                   #####
################################################################################

basic.model <- as.formula(paste("Cred ~ ", paste("cpi_lag1_dev_abs +", 
                                                   paste(control.vars, collapse= "+"))))


basic.model.full.sample <- lm(basic.model, weights = Weight, data =  working_data)
summary(basic.model.full.sample)

basic.model.full.sample.robust <- coeftest(basic.model.full.sample, vcov = vcovCL(basic.model.full.sample, cluster = ~year_month))
basic.model.full.sample.robust




working.model <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_over2_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_under2_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev_abs +", 
                                                   paste(control.vars, collapse= "+"))))

################################################################################
####                               Full Sample                             #####
################################################################################
model2 <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_over2_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_under2_dummy*dr_dev_tr_hicp_neg0.5) + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                                   paste(control.vars, collapse= "+"))))


model2.full.sample <- lm(model2, weights = Weight, data =  working_data)
summary(model2.full.sample)

model2.full.sample.robust <- coeftest(model2.full.sample, vcov = vcovCL(model2.full.sample, cluster = ~year_month))
model2.full.sample.robust


################################################################################
####                               Full Sample                             #####
################################################################################
model3 <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev_abs + ", 
                                            paste(control.vars, collapse= "+"))))


model3.full.sample <- lm(model3, weights = Weight, data =  working_data[working_data$cpi_lag1_over2_dummy == 1, ])
summary(model3.full.sample)

model3.full.sample.robust <- coeftest(model3.full.sample, vcov = vcovCL(model3.full.sample, cluster = ~year_month))
model3.full.sample.robust


################################################################################
####                               Full Sample                             #####
################################################################################
model4 <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5  + cpi_lag1_dev_abs + ", 
                                            paste(control.vars, collapse= "+"))))


model4.full.sample <- lm(model4, weights = Weight, data =  working_data[working_data$cpi_lag1_over2_dummy == 0, ])
summary(model4.full.sample)

model4.full.sample.robust <- coeftest(model4.full.sample, vcov = vcovCL(model4.full.sample, cluster = ~year_month))
model4.full.sample.robust


working.model <- 

################################################################################
####                               Full Sample                             #####
################################################################################
model5 <- as.formula(paste("Cred ~ ", paste("dr_dev_tr_hicp_neg0.5 + cpi_lag1_dev_pos + cpi_lag1_dev_neg + ", 
                                            paste(control.vars, collapse= "+"))))


model5.full.sample <- lm(model5, weights = Weight, data =  working_data)
summary(model5.full.sample)

model5.full.sample.robust <- coeftest(model5.full.sample, vcov = vcovCL(model5.full.sample, cluster = ~year_month))
model5.full.sample.robust


################################################################################
####                               Full Sample                             #####
################################################################################
model6 <- as.formula(paste("Cred ~ ", paste("tr_hicp_neg0.5 + deposit_rate +  cpi_lag1_dev_pos + cpi_lag1_dev_neg +", 
                                            paste(control.vars, collapse= "+"))))


model6.full.sample <- lm(model6, weights = Weight, data =  working_data)
summary(model6.full.sample)

model6.full.sample.robust <- coeftest(model6.full.sample, vcov = vcovCL(model6.full.sample, cluster = ~year_month))
model6.full.sample.robust


################################################################################
####                               Full Sample                             #####
################################################################################
working_data$cpi_lag1_under4_dummy <- ifelse(working_data$cpi_lag1_over4_dummy == 1, 0, 1)

model7 <- as.formula(paste("Cred ~ ", paste("I(cpi_lag1_over4_dummy*dr_dev_tr_hicp_neg0.5) + I(cpi_lag1_under4_dummy*dr_dev_tr_hicp_neg0.5) +  cpi_lag1_dev_pos + cpi_lag1_dev_neg +", 
                                            paste(control.vars, collapse= "+"))))


model7.full.sample <- lm(model7, weights = Weight, data =  working_data)
summary(model7.full.sample)

model7.full.sample.robust <- coeftest(model7.full.sample, vcov = vcovCL(model7.full.sample, cluster = ~year_month))
model7.full.sample.robust



