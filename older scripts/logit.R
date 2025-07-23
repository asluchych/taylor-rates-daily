################################################################################
####                           Logit Models                                #####
################################################################################

#### (1) inflation target deviations (Coleman, Nautz (2025), Equation (3)) ####
logit.itd <- glm(itc.m1, weights = Weight, family = binomial(link = "logit"),
                 data = civey)

# warning: wrong standard errors not clustered at monthly level
# summary(logit.itd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
logit.itd.robust <- coeftest(logit.itd, vcov =  vcovCL(logit.itd, 
                                              cluster = ~year_month))
print(logit.itd.robust)
PseudoR2(logit.itd, which = "McFadden")
AIC(logit.itd)

#### (2) inflation target deviations and deviations of deposit rate from Taylor rule implied rate ####
logit.itd.drd <- glm(itc.m2, weights = Weight,
                     family = binomial(link = "logit"),  data = civey)

# warning: wrong standard errors not clustered at monthly level
# summary(logit.itd.drd)

# cluster standard errors at monthly level following  Abadie et al. (2023)
logit.itd.drd.robust <- coeftest(logit.itd.drd, vcov =  vcovCL(logit.itd.drd, 
                                                  cluster = ~year_month))

logit.itd.drd.robust
PseudoR2(logit.itd.drd, which = "McFadden")
AIC(logit.itd.drd)

#### (3) inflation target deviations, deviations of deposit rate from Taylor rule implied rate and deflationary months ####
logit.itd.drd.defl <- glm(itc.m3, weights = Weight,
                          family = binomial(link = "logit"),  data = civey)

# warning: wrong standard errors not clustered at monthly level
# summary(logit.itd.drd.defl)

# cluster standard errors at monthly level following  Abadie et al. (2023)
logit.itd.drd.defl.robust <- coeftest(logit.itd.drd.defl,
                                      vcov =  vcovCL(logit.itd.drd.defl, 
                                      cluster = ~year_month))

logit.itd.drd.defl.robust
PseudoR2(logit.itd.drd.defl, which = "McFadden")
AIC(logit.itd.drd.defl)
