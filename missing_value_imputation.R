library(mice)
library(missForest)
install.packages('missForest')
install.packages('MICE')
#Get summary
summary(iris)

#Generate 10% missing values at Random 
iris.mis <- prodNA(iris, noNA = 0.1)
nrow(iris)
summary(iris.mis)
nrow(iris.mis)
str(iris.mis)
iris.mis <- subset(iris.mis,select = -c(Species))
md.pattern(iris.mis)

install.packages('VIM')
require(VIM)
library(VIM)

mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))


#let's impute the missing values.



imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(imputed_Data)

#check imputed values
 imputed_Data$imp$Sepal.Width

 #get complete data ( 2nd out of 5)
 completeData <- complete(imputed_Data,2)
 
 #build predictive model
  fit <- with(data = imputed_Data, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width))
  summary(fit)
  
  imputed_Data
  
  #combine results of all 5 models
  combine <- pool(fit)
  summary(combine)
  ?pool
  
 

