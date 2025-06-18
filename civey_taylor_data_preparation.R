# create dummy variable for females
civey_taylor$Female <- ifelse(civey_taylor$Gender == "Female", 1, 0)

# check if transformation is correct
sum(civey_taylor$Gender == "Female") / sum(table(civey_taylor$Gender))
mean(civey_taylor$Female)
# share of Female in variable Gender  equals mean of variable Female
# conclusion: correct transformation

# create dummy variable for East Germany
civey_taylor$East <- ifelse(civey_taylor$EastWest == "East", 1, 0)
# check if transformation is correct
sum(civey_taylor$EastWest == "East") / sum(table(civey_taylor$EastWest))
mean(civey_taylor$East)
# share of East in variable East  equals mean of variable East
# conclusion: correct transformation

# create dummy variable for College degree
civey_taylor$College <- ifelse(civey_taylor$ProfessionalEducation == "Degree", 1, 0)
# check if transformation is correct
sum(civey_taylor$ProfessionalEducation == "Degree") / sum(table(civey_taylor$ProfessionalEducation))
mean(civey_taylor$College)


# share of Degree in variable ProfessionalEducation  equals mean of variable College
# conclusion: correct transformation

# create dummy variable for Age greater or equal 50
civey_taylor$Age50Plus <- ifelse(civey_taylor$Age == "50 - 64" | civey_taylor$Age == "65 +" , 1, 0)
# check if transformation is correct
sum(civey_taylor$Age == "50 - 64" | civey_taylor$Age == "65 +") / sum(table(civey_taylor$Age))
mean(civey_taylor$Age50Plus)
# share of respondents 50+ in variable Age equals mean of variable Age50Plus
# conclusion: correct transformation

civey_taylor$Cred <- ifelse(civey_taylor$Response == "B" | civey_taylor$Response == "C" , 1, 0)

civey_taylor$cpi_lag1_dev_pos <- ifelse((civey_taylor$cpi_yoy_lag - 2) >= 0, 
                                        abs(civey_taylor$cpi_yoy_lag - 2), 0)

civey_taylor$cpi_lag1_dev_neg <- ifelse((civey_taylor$cpi_yoy_lag - 2)  < 0, 
                                        abs(civey_taylor$cpi_yoy_lag - 2), 0)

civey_taylor$JobPosition <- relevel(civey_taylor$JobPosition, ref = "Not employed")
civey_taylor$MaritalStatus <- relevel(civey_taylor$MaritalStatus, ref = "Single")
civey_taylor$EmploymentStatus <- relevel(civey_taylor$EmploymentStatus, ref = "Not employed")

civey_taylor <- civey_taylor %>%
  mutate(year_month = format(Day, "%Y-%m"))
