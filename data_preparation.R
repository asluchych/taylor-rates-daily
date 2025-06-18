################################################################################
####                           Data Preparation                            #####
################################################################################


####                              civey data                                ####    
# copy civey data set to work on
# civey <- civey_new_filled
civey <- data


# keep only columns of interest
# civey <- civey[, civey.vars]

# сopy Day column to Date column
# civey$Date <- civey$Day
# create Year variable  
# civey$Year <- format(civey$Date, "%Y")
# create Month variable  
# civey$Month <- format(civey$Date, "%m")
# create YearMonth variable  
# civey$YearMonth <- format(civey$Date, "%Y-%m")
# overwrite Day variable  
# civey$Day <- format(civey$Date, "%d")


####                              inflation data                            ####    
# create Date variable in Date format
# inflation$Date <- as.Date(paste(inflation$Year, inflation$Month, "01"),
#                          "%Y %B %d")
# convert CPI variable to numeric
# inflation$CPI <- as.numeric(inflation$CPI)
# create variable for absolute deviation from inflation target of 2 % (ITD)
# inflation$ITD.abs <- abs(inflation$CPI - 2)
# overwrite Year variable in data set inflation
# inflation$Year <- format(inflation$Date, "%Y")
# overwrite Month variable in data set inflation
# inflation$Month <- format(inflation$Date, "%m")
# drop rows with missing values
# inflation <- na.omit(inflation)

####                Taylor rule and deposit rate data                     ####

# convert date variable to Date format
interest_daily_data$date <- as.Date(interest_daily_data$date)
interest_daily_data <- interest_daily_data[interest_daily_data$date <= sample.end.date, ]

####                   civey and inflation merge                            ####
# merge  civey and inflation data conditioned on Year and Month
# civey <- merge(civey, inflation[, c("Year", "Month", "CPI")], 
#               by = c("Year", "Month"), all.x = TRUE)

# WARNING: may take a while
# check if merge was successful
# variable for counting wrong CPI values in civey data set
# cpi.wrong <- 0
# variable for counting correct CPI values in civey data set
# cpi.correct <- 0
# loop over CPI values in civey data set and compare them to the ones in 
# inflation data set conditioned on Year and Month
# for (year in unique(civey$Year)) {
#  for (month in unique(civey$Month)) {
#    for (row in 1:nrow(civey[civey$Year == year & civey$Month == month, ])) {
#      if (civey[civey$Year == year & civey$Month == month, "CPI"][row] != 
#          inflation[inflation$Year == year & inflation$Month == month, "CPI"]) {
#        cpi.wrong <- cpi.wrong + 1
#      } else {
#        cpi.correct <- cpi.correct + 1
#     }
#    }
#  }
#}
# c(cpi.wrong, cpi.correct)

# remove temporary variables
# rm(year, month, row)


####                            cpi and inflation                           ####
# CPI in the month before survey response for convenience
# for (year in unique(civey$Year)) {
#  for (month in unique(civey$Month)) {
#    first.day.month <- as.Date(paste(year, month, "01"), "%Y %m %d")
#    last.day.month <- first.day.month - 1
#    if (nrow(civey[civey$Date == first.day.month, ]) != 0) {
#      for (row in 1:nrow(civey[civey$Date == first.day.month, ])) {
#        last.day.month.cpi <- civey[civey$Date == last.day.month, "CPI"][1]
#        civey[civey$Date == first.day.month, "CPI"][row] <- last.day.month.cpi
#     }
#    }
#  }
# }

# remove temporary variables
# rm(year, month, row, first.day.month, last.day.month, last.day.month.cpi)

# check if CPI in the first day of month equals CPI in the previous month
# variable for counting wrong first day of month CPI 
# cpi.first.day.wrong <- 0
# variable for counting correct first day of month CPI 
# cpi.first.day.correct <- 0
# loop over first day of month CPI values in civey data set and
# compare them the CPI value in the previous month in inflation data set
# for (year in unique(civey$Year)) {
#  for (month in unique(civey$Month)) {
#    first.day.month <- as.Date(paste(year, month, "01"), "%Y %m %d")
#    previous.month <- as.Date(paste(format(first.day.month - 1, "%Y"),
#                                    format(first.day.month - 1, "%m"),
#                                    "01"), "%Y %m %d")
#    if (nrow(civey[civey$Date == first.day.month, ]) != 0) {
#      for (row in 1:nrow(civey[civey$Date == first.day.month, ])) {
#        if (civey[civey$Date == first.day.month, "CPI"][row] != 
#            inflation[inflation$Date == previous.month, "CPI"]) {
#          cpi.first.day.wrong <- cpi.first.day.wrong + 1
#        } else {
#          cpi.first.day.correct <- cpi.first.day.correct + 1
#        }
#     }
#    }
#  }
#}

# c(cpi.first.day.correct, cpi.first.day.wrong)

# remove temporary variables
# rm(month, year, row, first.day.month, previous.month)





####               add Taylor rate and deposit rate to civey data set       ####

# try to change to dplyr instead of for loop
# for (row in 1:nrow(civey)) {
#  if (length(interest_daily_data[interest_daily_data$date == 
#                                 civey[row, "Date"], "taylor_rate_0"][[1]])) {
#    civey[row, "taylor_rate_0"] <- interest_daily_data[interest_daily_data$date == 
#                                                      civey[row, "Date"], "taylor_rate_0"][[1]]
#    civey[row, "taylor_rate_0_lag_15"] <- interest_daily_data[interest_daily_data$date == 
#                                                         civey[row, "Date"], "taylor_rate_0_lag_15"][[1]]
#    civey[row, "shadow_rate"] <- interest_daily_data[interest_daily_data$date == 
#                                                       civey[row, "Date"], "shadow_rate"][[1]]
#  }
#  else {
#    civey[row, "taylor_rate_0"] <- NA
#    civey[row, "taylor_rate_0_lag_15"] <- NA
#    civey[row, "shadow_rate"] <- NA
#  }
#  
#}

# civey_temp <- civey 

civey <- civey %>%
  left_join(interest_daily_data %>%
              select(date, taylor_rate_0, taylor_rate_0_lag, shadow_rate), 
            by = c("Day" = "date"))

civey$ir_dev <- civey$shadow_rate -  civey$taylor_rate_0_lag

# variable for absolute positive deviations of deposit rate from Taylor rate
# civey$DRD.pos <- ifelse((civey$DepositRate -  civey$TaylorRate) >= 0, 
#                        abs(civey$DepositRate -  civey$TaylorRate), 0)
# create variable for absolute negative deviations of deposit rate from Taylor rate
#civey$DRD.neg <- ifelse((civey$DepositRate -  civey$TaylorRate) < 0, 
#                        abs(civey$DepositRate -  civey$TaylorRate), 0)

# civey$ITD.abs.defl <- ifelse(civey$CPI < 0, civey$ITD.abs, 0)

#  variable for absolute deviations from inflation target of 2 % (ITD)
# civey$ITD.abs <- abs(civey$CPI - 2)
#  variable for positive deviations from inflation target of 2 % (ITD)
# civey$ITD.pos <- ifelse(civey$CPI >= 2, abs(civey$CPI - 2), 0)
#  variable for negative deviations from inflation target of 2 % (ITD)
# civey$ITD.neg <- ifelse(civey$CPI < 2, abs(civey$CPI - 2), 0)

# create dummy variable for females
civey$Female <- ifelse(civey$Gender == "Female", 1, 0)

# check if transformation is correct
sum(civey$Gender == "Female") / sum(table(civey$Gender))
mean(civey$Female)
# share of Female in variable Gender  equals mean of variable Female
# conclusion: correct transformation

# create dummy variable for East Germany
civey$East <- ifelse(civey$EastWest == "East", 1, 0)
# check if transformation is correct
sum(civey$EastWest == "East") / sum(table(civey$EastWest))
mean(civey$East)
# share of East in variable East  equals mean of variable East
# conclusion: correct transformation

# create dummy variable for College degree
civey$College <- ifelse(civey$ProfessionalEducation == "Degree", 1, 0)
# check if transformation is correct
sum(civey$ProfessionalEducation == "Degree") / sum(table(civey$ProfessionalEducation))
mean(civey$College)


# share of Degree in variable ProfessionalEducation  equals mean of variable College
# conclusion: correct transformation

# create dummy variable for Age greater or equal 50
civey$Age50Plus <- ifelse(civey$Age == "50 - 64" | civey$Age == "65 +" , 1, 0)
# check if transformation is correct
sum(civey$Age == "50 - 64" | civey$Age == "65 +") / sum(table(civey$Age))
mean(civey$Age50Plus)
# share of respondents 50+ in variable Age equals mean of variable Age50Plus
# conclusion: correct transformation

civey$cpi_lag1_dev_abs_defl <- ifelse(civey$cpi_lag1_dev_neg > 2, civey$cpi_lag1_dev_neg, 0)

civey$JobPosition <- relevel(civey$JobPosition, ref = "Not employed")
civey$MaritalStatus <- relevel(civey$MaritalStatus, ref = "Single")
civey$EmploymentStatus <- relevel(civey$EmploymentStatus, ref = "Not employed")
