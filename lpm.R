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



