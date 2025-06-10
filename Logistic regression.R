#Converting variables to factors
GPSJS_dataset$prov <- as.factor(GPSJS_dataset$prov)
GPSJS_dataset$IncomeMa <- as.factor(GPSJS_dataset$IncomeMa)
GPSJS_dataset$maritalstatus <- as.factor(GPSJS_dataset$maritalstatus)
GPSJS_dataset$education_level <- as.factor(GPSJS_dataset$education_level)
GPSJS_dataset$age_group <-as.factor(GPSJS_dataset$age_group)
GPSJS_dataset$ethnicity <-as.factor(GPSJS_dataset$ethnicity)
#Personal property 
model1 <- glm(Personal_Property ~ prov+ethnicity + education_level + 
                income,
              family = binomial(), 
              data = GPSJS_dataset)
summary(model1)
coef(model1)
exp(coef(model1))
#Consumer Fraud
model2 <- glm(Consumer_Fraud ~ prov+ethnicity + education_level,
              family = binomial(), 
              data = GPSJS_dataset)
summary(model2)
coef(model2)
exp(coef(model2))
#Hijacking
model3 <- glm(Hijacking_Other ~ethnicity,
              family = binomial(),
              data = GPSJS_dataset)

summary(model3)
coef(model3)
exp(coef(model3))
#All crimes
model4 <- glm(allCrimes ~ prov+ethnicity + education_level + 
                income,
              family = binomial(), 
              data = GPSJS_dataset)
summary(model4)
coef(model4)
exp(coef(model4))
