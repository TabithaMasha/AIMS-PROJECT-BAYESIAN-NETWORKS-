#Loading libraries
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
# Plot for provincial counts
# Plot
ggplot(GPSJS_dataset, aes(x = prov, fill = prov)) +
  geom_bar() +
  labs(
    title = "Distribution of Households by Province",
    x = "Province",
    y = "Count",
    fill = "Province"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#Personal Property rates
PPT_rates <- GPSJS_dataset %>%
  group_by(prov) %>%
  summarise(
    Yes = sum(Personal_Property== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
# View the table
print(PPT_rates)
#Robbery rates in provinces
Rob_rates <- GPSJS_dataset %>%
  group_by(prov) %>%
  summarise(
    Yes = sum(Robbery== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Consumer Fraud rates in provinces
CF_rates <- GPSJS_dataset %>%
  group_by(prov) %>%
  summarise(
    Yes = sum(Consumer_Fraud== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Hijacking/Other rates in provinces
Hij/Other_rates <- GPSJS_dataset %>%
  group_by(prov) %>%
  summarise(
    Yes = sum(Hijacking_Other== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Rates by Education level
#Personal Property rates by education level
PPT_rates <- GPSJS_dataset %>%
  group_by(education_level) %>%
  summarise(
    Yes = sum(Personal_Property== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
# View the table
print(PPT_rates)
#Robbery rates by education level
Rob_rates <- GPSJS_dataset %>%
  group_by(education_level) %>%
  summarise(
    Yes = sum(Robbery== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Consumer Fraud rates by education level
CF_rates <- GPSJS_dataset %>%
  group_by(education_level) %>%
  summarise(
    Yes = sum(Consumer_Fraud== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Hijacking/Other rates by education level
Hij/Other_rates <- GPSJS_dataset %>%
  group_by(education_level) %>%
  summarise(
    Yes = sum(Hijacking_Other== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Rates by marital status
#Personal Property rates by marital status
PPT_rates <- GPSJS_dataset %>%
  group_by(maritalstatus) %>%
  summarise(
    Yes = sum(Personal_Property== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
# View the table
print(PPT_rates)
#Robbery rates by marital status
Rob_rates <- GPSJS_dataset %>%
  group_by(maritalstatus) %>%
  summarise(
    Yes = sum(Robbery== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Consumer Fraud rates by marital status
CF_rates <- GPSJS_dataset %>%
  group_by(maritalstatus) %>%
  summarise(
    Yes = sum(Consumer_Fraud== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#Hijacking/Other rates by marital status
Hij/Other_rates <- GPSJS_dataset %>%
  group_by(maritalstatus) %>%
  summarise(
    Yes = sum(Hijacking_Other== "1", na.rm = TRUE),
    TotalResponses = n(),
    Rate = Yes / TotalResponses
  )
#ercentage of crime occurrence distribution in province
crime_vars <- c("Robbery", "Personal_Property", "Consumer_Fraud","Hijacking_Other")
# Calculating the  total number of crimes
total_crimes <- GPSJS_dataset %>%
  pivot_longer(cols = all_of(crime_vars), names_to = "Crimetype", values_to = "Response") %>%
  filter(Response == "1") %>%
  nrow()

# Calculating the number of crimes and rate  in a province by crime type
crime_rates <- GPSJS_dataset %>%
  pivot_longer(cols = all_of(crime_vars), names_to = "Crimetype", values_to = "Response") %>%
  filter(Response == "1") %>%
  group_by(prov, Crimetype) %>%
  summarise(
    NumCrimes = n(),
    TotalCrimes = total_crimes,
    Rate = NumCrimes / TotalCrimes
  )

# View the table
print(crime_rates)



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
