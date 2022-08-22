rm(list = ls())

#reading the files

casesld = data.frame(read.csv(file = "..//R_Data//New Cases Lockdown.csv"))
#lockdown cases
casesld

casespld = data.frame(read.csv(file = "..//R_Data//New Cases Post Lockdown.csv"))
#post-lockdown cases
casespld

othinf = data.frame(read.csv(file = "..//R_Data//Other info.csv"))
#other data
othinf

labourf = data.frame(read.csv(file = "..//R_Data//LabourForce.csv"))
#other info pt.2
labourf

pubawa = data.frame(read.csv(file = "..//R_Data//PubAwareness.csv"))
#other info pt.3
pubawa

#population per state
pop = othinf$Pop..Inc.
pop

#for checking
length(casesld$States)
ncol(casesld)

#creating proportion dataframes
propcasesld = casesld[,-1]
propcasespld = casespld[,-1]

for(i in 1:28)
{
  
  propcasesld[i,] = (casesld[i,-1]/pop[i])
  propcasespld[i,] = (casespld[i,-1]/pop[i])
  
}
#viewing the dataframes
View(as.data.frame(propcasesld))
View(as.data.frame(propcasespld))

library("Hmisc")
library(Matrix)

#converting to matrices
arrcld = data.matrix(propcasesld)
arrcld

arrcpld = data.matrix(propcasespld)
arrcpld

#For rsq analysis
lit = othinf$Literacy.Rate
#x1 values
lit

labour = labourf$Labour
#x2 values
labour 

pawe = pubawa$Public.Awareness
#x3 values
pawe

ot = data.frame(read.csv(file = "..//R_Data//OldTest.csv"))
ott = data.matrix(ot)

#Poisson Regression
#spatial series
confld = data.frame(read.csv(file = "..//R_Data//LockdownConfirmed.csv"))
#lockdown cases
confld

confpld = data.frame(read.csv(file = "..//R_Data//PostLockdownConfirmed.csv"))
#post-lockdown cases
confpld

ldconf = confld$Cases.Lockdown

pldconf = confpld$Cases.Post.Lockdown

liter = lit
liter

lab = labour
lab
pawe

length(ldconf)
length(liter)
length(lab)
length(pawe)
sqliter = sqrt(liter)
sqlab = sqrt(lab)
sqpawe = sqrt(pawe)
modellockdown.poi = glm(formula = ldconf ~ liter + lab + pawe, family = poisson(link = "log"))
modellockdown = glm(formula = ldconf ~ liter + lab + pawe)
summary(modellockdown, correlation = TRUE)
summary(modellockdown.poi, correlation = TRUE)
options(scipen = 999)

#plot(liter,ldconf, xlab = "literacy", ylab = "Confirmed cases statewise" )
#plot(lab,ldconf, xlab = "labour force", ylab = "Confirmed cases statewise" )

modelpostlockdown.poi = glm(formula = pldconf ~ liter + lab + pawe, family = poisson(link = "log"))
modelpostlockdown = glm(formula = pldconf ~ liter + lab + pawe)
summary(modelpostlockdown, correlation = TRUE)
summary(modelpostlockdown.poi, correlation = TRUE)

modellockdown2 = glm(formula = sqrt(ldconf) ~ liter + lab + pawe)
summary(modellockdown2)

modelpostlockdown2 = glm(formula = sqrt(pldconf) ~ liter + lab + pawe)
summary(modelpostlockdown2)

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
plot(residuals(modellockdown.poi, type="deviance"), ylab = "Residuals",xlab = "Fitted Values", main = "Residual plot for Poisson Regression model for Confirmed cases in Lockdown phase" )
plot(residuals(modelpostlockdown.poi, type="deviance"), ylab = "Residuals",xlab = "Fitted Values", main = "Residual plot for Poisson Regression model for Confirmed cases in Post-Lockdown phase")
