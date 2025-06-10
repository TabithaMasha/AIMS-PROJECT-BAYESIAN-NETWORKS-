#Loading libraries
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
#Rename variables
rename_var <- c("age_group" = "age_group2")
GPSJS_dataset <- GPSJS_dataset %>%
  rename(!!!rename_var)
#Chi-square tests and residuals
crime_table<-table(GPSJS_dataCleaned$maritalstatus,GPSJS_dataCleaned$allCrimes)
crime_table
chi_test <- chisq.test(crime_table)
chi_test
residuals <- chisq.test(crime_table)$stdres
print(residuals)
#rates were calculated manually
#tables for calculating tables
crime_table<-table(GPSJS_dataCleaned$variable,GPSJS_dataCleaned$Crime variable)
