#Install Required libraries

library(aod)
#install.packages('aod')
library(ggplot2)
library(Rcpp)

# A researcher is interested in how variables, such as GRE 
#(Graduate Record Exam scores), GPA (grade point average) and prestige 
#of the undergraduate institution, effect admission into graduate school. 
#The response variable, admit/don't admit, is a binary variable.

setwd('G:\\Predictive Modelling\\self')
#Read GRE  data from http://www.ats.ucla.edu

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
nrow(mydata)

str(mydata)
head(mydata)

#This dataset has a binary response (outcome, dependent) variable called admit.
#There are three predictor variables: gre, gpa and rank. We will treat the variables gre and gpa as continuous.
#The variable rank takes on the values 1 through 4. 
#Institutions with a rank of 1 have the highest prestige, while those with a rank of 4 have the lowest

#Summary
summary(mydata)
sapply(mydata, sd)
sapply(mydata,var)

attach(mydata)

## two-way contingency table of categorical outcome and predictors
## we want to make sure there are not 0 cells

xtabs(~ admit + rank, data = mydata)

#Formulation of Logistics model
#Convert Rank to Factor variable

mydata$rank <- factor(mydata$rank)
#Logit Model
mylogit <- glm(admit~gre+gpa+rank,data=mydata,family='binomial')

summary(mylogit)

#interpretation-

#For every one unit change in gre, the log odds of admission (versus non-admission) increases by 0.002.
#For a one unit increase in gpa, the log odds of being admitted to graduate school increases by 0.804.
#The indicator variables for rank have a slightly different interpretation. For example, having attended an undergraduate institution with rank of 2, 
#versus an institution with a rank of 1, changes the log odds of admission by -0.675.




## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

#We can test for an overall effect of rank using the wald.test function of the aod library.
#The order in which the coefficients are given in the table of coefficients is the same as 
#the order of the terms in the model.
require('aod')

vcov(mylogit)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

#Chi-squared test:
#X2 = 20.9, df = 3, P(> X2) = 0.00011

#The chi-squared test statistic of 20.9, with three degrees of freedom is associated with a p-value of 0.00011 
#indicating that the overall effect of  rank is statistically significant.

#We can also test additional hypotheses about the differences in the coefficients for the different levels of rank

l <- cbind(0,0,0,1,-1,0)
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), L = l)

#p value 0.019 indicates difference between the coefficient for rank=2 
#and the coefficient for rank=3 is statistically significant.

coef(mylogit)

## odds ratios only
exp(coef(mylogit))

## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#We will start by calculating the predicted probability of admission at each value of rank, 
#holding gre and gpa at their means. First we create and view the data frame.



newdata1 <- with(mydata,data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

#Predict Probablities of for admissions 

newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")

newdata2 <- with(mydata,
                 data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4),
                            gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type="link", se=TRUE))

?predict
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
?plogis()
head(newdata3)

ggplot(newdata3, aes(x = gre, y = PredictedProb)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = .2) +
  geom_line(aes(colour = rank), size=1)

#To find the difference in deviance for the two models (i.e., the test statistic) we can use the command: 
with(mylogit, null.deviance - deviance)

## [1] 41.5
#The degrees of freedom for the difference between the two models is equal to the number of predictor variables 
#in the mode, and can be obtained using:

with(mylogit, df.null - df.residual)

## [1] 5

#Finally, the p-value can be obtained using:

with(mylogit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))

#7.578194e-08

#The chi-square of 41.46 with 5 degrees of freedom and an associated p-value of less than 0.001 tells us that our model as 
#a whole fits significantly better than an empty model

#To see the model's log likelihood
logLik(mylogit)


pdata <- read.csv("p-odds.csv")

pdata <- pdata[,-21]

plot(pdata$log.odds~pdata$odds)

xrange <- range(pdata$odds) 
yrange <- range(pdata$log.odds) 
print(p)

ggplot(pdata, aes(x = pdata$odds, y = pdata$log.odds)) +
  theme_bw() +
  geom_point(data = pdata, aes(x = pdata$odds, y = pdata$log.odds)) +
  geom_line(aes(colour = pdata), size = 1)








 