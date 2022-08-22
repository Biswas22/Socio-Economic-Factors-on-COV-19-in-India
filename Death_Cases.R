#Reading csv files

othinf = data.frame(read.csv(file = "..//R_Data//Other infoDeaths.csv"))
#other data
othinf

labourf = data.frame(read.csv(file = "..//R_Data//LabourForce.csv"))
#other info pt.2
labourf

pubawa = data.frame(read.csv(file = "..//R_Data//PubAwareness.csv"))
#other info pt.3
pubawa

#Poisson Regression
#spatial series
detld = data.frame(read.csv(file = "..//R_Data//DeathsLockdown.csv"))
#lockdown cases
detfld

detpld = data.frame(read.csv(file = "..//R_Data//DeathsPostLockdown.csv"))
#post-lockdown cases
detpld

lddet = confld$Deaths.Lockdown

plddet = confpld$Deaths.Post.Lockdown

lit = othinf$Literacy.Rate

labour = labourf$Labour

pawe = pubawa$Public.Awareness

liter = lit
liter

lab = labour
lab
pawe

length(lddet)
length(liter)
length(lab)
length(pawe)

modellockdown = glm(formula = lddet ~ liter + lab + pawe)
modellockdown2 = glm(formula = sqrt(lddet) ~ liter + lab + pawe)
modellockdown.poi = glm(formula = lddet ~ liter + lab + pawe, family = poisson(link = "log"))

summary(modellockdown, correlation = TRUE)
summary(modellockdown2)
summary(modellockdown.poi, correlation = TRUE)
options(scipen = 999)

#Post-Lockdown Phase

modelpostlockdown = glm(formula = plddet ~ liter + lab + pawe)
modelpostlockdown2 = glm(formula = sqrt(plddet) ~ liter + lab + pawe)
modelpostlockdown.poi = glm(formula = plddet ~ liter + lab + pawe, family = poisson(link = "log"))

summary(modelpostlockdown, correlation = TRUE)
summary(modelpostlockdown2)
summary(modelpostlockdown.poi, correlation = TRUE)


#Further study
#Confidence Intervals
confint(modellockdown.poi)
confint(modelpostlockdown.poi)

#Predicted values
predict(modellockdown.poi, type="response")
predict(modelpostlockdown.poi, type="response")

#Residuals
residuals(modellockdown.poi, type="deviance")
residuals(modelpostlockdown.poi, type="deviance")

#Graph
plot(residuals(modellockdown.poi, type="deviance"), ylab = "Residuals",xlab = "Fitted Values", main = "Residual plot for Poisson Regression model for Death cases in Lockdown phase" )
plot(residuals(modelpostlockdown.poi, type="deviance"), ylab = "Residuals",xlab = "Fitted Values", main = "Residual plot for Poisson Regression model for Death cases in Post-Lockdown phase")

