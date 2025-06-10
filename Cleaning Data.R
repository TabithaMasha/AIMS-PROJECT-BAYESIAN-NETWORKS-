library(dplyr)# load library
# Removing missing values
colSums(is.na(gpsjs_2020_2021_individual_v1))

# Identify columns where more than 70% of values are "Not applicable"
threshold <- 0.70
remove_cols <- sapply(gpsjs_2020_2021_individual_v1, function(col) {
  mean(col == "Not applicable", na.rm = TRUE) > threshold
})
# Removing  those columns
gpsjs_2020_2021_individual_v1 <- gpsjs_2020_2021_individual_v1[, !remove_cols]
colnames(gpsjs_2020_2021_individual_v1)
#Removing variables not related to the study
remove_vars <- c("Q51Ohealth", "Q52HHhealth","Q53Vulnhealth","Q54healthSyst","...")
#Renaming crime variables
gpsjs_2020_2021_individual_v1 <- gpsjs_2020_2021_individual_v1[, !(names(gpsjs_2020_2021_individual_v1) %in% remove_vars)]
rename_vars <- c(
  "Personal Property" = "Q31past5yrs__1",
  "Motorvehicle hijacking" = "Q31past5yrs__2",
  "Robbery" = "Q31past5yrs__3",
  "Consumer Fraud" = "Q31past5yrs__4",
  "Others" = "Q31past5yrs__5"
)
gpsjs_2020_2021_individual_v1 <- gpsjs_2020_2021_individual_v1 %>%
  rename(!!!rename_vars)
#Reducing education categories and creating age group variable
gpsjs_2020_2021_individual_v1$education_level <- factor(gpsjs_2020_2021_individual_v1$education,
                                                        levels = c("No schooling", "Grade R/0", "Grade 1/Sub A/Class 1", "Grade 2/Sub B/Class 2",
                                                                   "Grade 3/Standard 1/ABET1/AET 1", "Grade 4/Standard 2", "Grade 5/Standard 3/AET 2",
                                                                   "Grade 6/Standard 4", "Grade 7/Standard 5/AET 3", "Grade 8/Standard 6/Form 1",
                                                                   "Grade 9/Standard 7/Form 2/AET 4", "Grade 10/Standard 8/Form 3",
                                                                   "Grade 11/Standard 9/Form 4", "Grade 12/Standard 10/Form 5/Matric",
                                                                   "Certificate with less than Grade 12/Std 10", "Diploma with less than Grade 12/Std 10",
                                                                   "Higher/National/Advanced Certificate with Grade 12/Std 10/Occupational Certificate-NQF Level 5",
                                                                   "Diploma with Grade 12/Std 10/Occupational Certificate-NQF Level 6",
                                                                   "Bachelor's Degree and post/ Occupational Certificate-NQF Level 7",
                                                                   "Higher Diploma/Occupational Certificate(B-Tech Diploma)-NQF Level 7",
                                                                   "Honours Degree/Postgraduate Diploma/Occupational Certificate-NQF Level 8",
                                                                   "Post Higher Diploma (Masters Diploma and Master's Degree)-NQF Level 9",
                                                                   "Doctoral Degrees (Doctoral Diploma and PhD)-NQF Level 10",
                                                                   "N4/NTC 4", "N5/NTC 5", "N6/NTC 6", "NTC 1/N1", "NTC 2/N2/NC (V)/Level 3",
                                                                   "NTC 3/N3/NC (V)/Level 4", "Other(specify)", "Unspecified", "Do not know"),
                                                        labels = c("No Schooling", "Primary", "Primary", "Primary",
                                                                   "Primary", "Primary", "Primary", "Primary", "Primary", "Lower Secondary",
                                                                   "Lower Secondary", "Upper Secondary", "Upper Secondary", "Upper Secondary",
                                                                   "Certificate", "Certificate", "Certificate", "Diploma",
                                                                   "Bachelor's", "Bachelor's", "Honours", "Masters", "Doctorate",
                                                                   "Technical", "Technical", "Technical", "Technical", "Technical",
                                                                   "Technical", "Other", "Unspecified", "Unknown"),
                                                        ordered = TRUE)
gpsjs_2020_2021_individual_v1$education_level <- mapvalues(gpsjs_2020_2021_individual_v1$education_level, 
                                                           from = c("Bachelor's", "Certificate", "Diploma", "Honours", 
                                                                    "Lower Secondary", "Masters","No Schooling","Other","Primary","Technical",  "Unknown","Unspecified","Upper Secondary","Doctorate"),
                                                           to = c("Ba.", "Cert", "Dip", "Hons", "LS", 
                                                                  "Msc","NS","Ot+","Pri", "Tech",  "Un","Uns","US","Dr"))
gpsjs_2020_2021_individual_v1$education_level <- mapvalues(gpsjs_2020_2021_individual_v1$education_level, 
                                                           from = c("Ba.", "Cert", "Dip", "Hons", "LS", 
                                                                    "Msc","NS","Ot+","Pri", "Tech",  "Un","Uns","US","Dr"),
                                                           to = c("Degree", "Vocational", "Vocational", "Degree", "Secondary", 
                                                                "Degree","Noschool","Other","Primary", "Vocational",  "Other","Other","Secondary","Degree"))
#placing age into agegroups and creating age group variable
gpsjs_2020_2021_individual_v1$age_group <- cut(gpsjs_2020_2021_individual_v1$age,
                                               breaks = c(17, 24, 34, 44, 54, 64, 74, 84, 94, 108),
                                               labels = c("17-24", "25-34", "35-44", "45-54",
                                                          "55-64", "65-74", "75-84", "85-94", "95+"),include.lowest = TRUE, right = TRUE)
gpsjs_2020_2021_individual_v1$age_group <- mapvalues(gpsjs_2020_2021_individual_v1$age_group, 
                                                     from = c("17-24", "25-34", "35-44", "45-54", "55-64", 
                                                              "65-74", "75-84","85-94","95+"),
                                                     to = c("17-24", "25-34", "35-44", "45-54", "55-64", 
                                                            "65-74","75+","75+","75+"))
#Reducing income variable to fewer categories
gpsjs_2020_2021_individual_v1$IncomeMa <- mapvalues(gpsjs_2020_2021_individual_v1$IncomeMa, 
                                                    from = c(
                                                      "Salaries and wages",
                                                      "Net profit from business or professional practice/ activities of commercial farming",
                                                      "Income from subsistence farming",
                                                      "Income from letting of fixed property",
                                                      "Social grants (including old age grant)",
                                                      "Maintenance, spousal support and similar allowances from divorced spouse, family members, etc., living elsewhere",
                                                      "Regular payments from pension fund from previous employment and pension from annuity  funds",
                                                      "Regular allowance/ remittances received from non-household members",
                                                      "Other income (e.g. royalties, interest, dividends on shares, income from share trading)",
                                                      "Not applicable",
                                                      "Unspecified"
                                                    ),
                                                    to = c(
                                                      "Employment",
                                                      "Business Income",
                                                      "Business Income",
                                                      "Property Income",
                                                      "Government Transfers",
                                                      "Government Transfers",
                                                      "Government Transfers",
                                                      "Government Transfers",
                                                      "Other Income",
                                                      "Not Applicable",
                                                      "Not Applicable"
                                                    ))
# Converting crime variables to binary(1/0)
cols_to_convert <- c("Personal_Property", "Motorvehicle_hijacking", "Robbery", "Consumer_Fraud", "Others")
gpsjs_2020_2021_individual_v1[cols_to_convert] <- lapply(gpsjs_2020_2021_individual_v1[cols_to_convert], function(x) ifelse(x == "Yes", 1, 0))
#Combining hijacking and other to hijacking/other
gpsjs_2020_2021_individual_v1$Hijacking_Other <- ifelse(gpsjs_2020_2021_individual_v1$Motorvehicle_hijacking == 1 | gpsjs_2020_2021_individual_v1$Others == 1, 1, 0)
#Combining all the 5 crimes
gpsjs_2020_2021_individual_v1$allCrimes <- ifelse(gpsjs_2020_2021_individual_v1$Others == 1 |gpsjs_2020_2021_individual_v1$Motorvehicle_hijacking == 1 |gpsjs_2020_2021_individual_v1$Robbery == 1 | gpsjs_2020_2021_individual_v1$Consumer_Fraud == 1| gpsjs_2020_2021_individual_v1$Personal_Property == 1, 1, 0)
#saving the cleaned dataset
write.csv(gpsjs_2020_2021_individual_v1, "/home/tabitha/GPSJS_dataset.csv", row.names = FALSE)