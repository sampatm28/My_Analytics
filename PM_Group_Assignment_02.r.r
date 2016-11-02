#Install Packages
library('car')
require('car')

#install.packages('car')
#Create Directory
setwd("G:\\Predictive Modelling\\GA")
#Import file
cons_research_1 <- read.csv('consumer-1.csv', header = T)

#attach(data:cons_research_1)
attach(cons_research_1)

#Descriptive statistics for data

str(cons_research_1)
names(cons_research_1)
summary(cons_research_1)
sapply(cons_research_1, class)
head(cons_research_1)

table(Household.Size)
table(Income)
table(Amount.charged)
plot(Income ~ Household.Size)

#Multiple Linear Regression model

cons_model1 <-
  lm(Amount.charged ~ Income + Household.Size , data = cons_research_1)
summary(cons_model1)
vif(cons_model1)
AIC(cons_model1)
confint(cons_model1)
plot(cons_model1)
outlierTest(cons_model1)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(cons_model1, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(cons_model1)
hist(sresid, freq = FALSE,
     main = "Distribution of Studentized Residuals")
xfit <- seq(min(sresid), max(sresid), length = 50)
yfit <- dnorm(xfit)
lines(xfit, yfit)

library(car)
# Deletion Diagnostics
influence.measures(cons_model1)
# Index Plots of the influence measures
influenceIndexPlot(cons_model1, id.n = 3)
# A user friendly representation of the above
influencePlot(cons_model1, id.n = 3)

# identify D values > 4/(n-k-1) by calculating cutoff for cook's distance
cutoff <-
  4 / ((nrow(cons_research_1) - length(cons_model1$coefficients) - 2))
plot(cons_model1, which = 4, cook.levels = cutoff)



# Pedicton for avg Income=40 (40000$),Household.Size=4
newdata <- data.frame(Income = 40, Household.Size = 4)
predict(
  cons_model1,
  newdata,
  type = "response",
  interval = "confidence" ,
  level = 0.95
)
predict(
  cons_model1,
  newdata,
  type = "response",
  interval = "predict",
  level = 0.95
)


## Performing Regression by removing influential observations 3,5,11
cons_research_2 <- cons_research_1[-c(3, 5, 11),]
nrow(cons_research_2)

#Multiple Linear Regression model post removal of 3 outliers
plot(cons_research_2)
cons_model2 <-
  lm(Amount.charged ~ Income + Household.Size , data = cons_research_2)
summary(cons_model2)

vif(cons_model2)
AIC(cons_model2)
confint(cons_model2)
plot(cons_model2)
outlierTest(cons_model2)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(cons_model2, main = "QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(cons_model2)
hist(sresid, freq = FALSE,
     main = "Distribution of Studentized Residuals")
xfit <- seq(min(sresid), max(sresid), length = 50)
yfit <- dnorm(xfit)
lines(xfit, yfit)

# Pedicton for avg Income=40 (40000$),Household.Size=4
newdata1 <- data.frame(Income = 40, Household.Size = 4)
predict(
  cons_model2,
  newdata1,
  type = "response",
  interval = "confidence" ,
  level = 0.95
)
predict(
  cons_model2,
  newdata1,
  type = "response",
  interval = "predict",
  level = 0.95
)
