## Author: Sampat Malu
## Company Name: HR Attrition Model Development-CHAID and CART Classfication technique
## Email : sampat.malu@gmail.com

## Let us first set the working directory path

setwd("G:\\DataMining\\Resi5\\Assignment")
getwd()

install.packages("popbio")
install.packages("mice")
install.packages("ineq")
install.packages("caret")
install.packages('NCStats')
install.packages('ROCR')

#Load required libraries
library(CHAID)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(mice)
library(ROSE)
library(popbio)
library(partykit)
library(ROCR)
library(rpart)
library(data.table)
library(ineq)



## Let us import the data that we intend to use for modeling

Emp_attrition = read.csv("1452762979_586__HR_Employee_Attrition_Data.csv")

str(Emp_attrition)


# Rearrange the columns so that the dependent variable 'Attrition' is column #1

Attri_DF = data.frame(Emp_attrition[,2],Emp_attrition[,1], Emp_attrition[,3:35])
View(Attri_DF)

colnames(Attri_DF)[2] = "Age"
colnames(Attri_DF)[1] = "Attrition"

Emp_attrition = Attri_DF

str(Attri_DF)


#CART Model

## loading the library
library(rpart)
library(rpart.plot)


 ## setting the control paramter inputs for rpart
  r.ctrl = rpart.control(minsplit=40, minbucket = 5, cp = 0, xval = 10)
  
  ## calling the rpart function to build the tree
  cart.tree1 <- rpart(formula = Attri_DF$Attrition ~ ., data = Attri_DF[,-1], method = "class", control = r.ctrl)
  
  #Summary to get variable importance from cart tree
  summary(cart.tree1)
  #print Splits and steps performed in cart tree
  print(cart.tree1)
  
  #Bucketing and classification scheame fo Cart
  fancyRpartPlot( cart.tree1)
 

 ## to find how the tree performs
  printcp(cart.tree1)
  plotcp(cart.tree1)
  
 ## Pruning  till best cp value
  
  bestcp <- cart.tree1$cptable[which.min(cart.tree1$cptable[,"xerror"]),"CP"]
  cart.tree1.pruned <- prune(cart.tree1, cp = bestcp)
 #cart.tree1.pruned<- prune( cart.tree1.pruned, cp= 0.0056259,"CP")

 #Print prunned tree
   printcp(cart.tree1.pruned )
   fancyRpartPlot(cart.tree1.pruned , uniform=TRUE,  main="Pruned Classification Tree")

 #CART model with minimum cp value 


   r.ctrl1 = rpart.control(minsplit=40, minbucket = 5, cp = bestcp, xval = 10)

   cart.tree2.bestcp <- rpart(formula = Attri_DF$Attrition ~ ., data = Attri_DF[,-1], 
                                  method = "class", control = r.ctrl1)
 
   print(cart.tree2.bestcp )
   summary(cart.tree2.bestcp )

   fancyRpartPlot(cart.tree2.bestcp , uniform=TRUE,  main="Pruned Classification Tree best cp")

## Scoring syntax
  Attri_DF$predict.class <- predict(cart.tree2.bestcp, Attri_DF, type="class")
  Attri_DF$predict.score <- predict(cart.tree2.bestcp, Attri_DF)
  attach(Attri_DF)
  
  library(ROCR)
  pred <- prediction(Attri_DF$predict.score[,2], Attri_DF$Attrition)

  perf <- performance(pred, "tpr", "fpr")
  plot(perf)
  KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
  auc <- performance(pred,"auc"); 
  auc <- as.numeric(auc@y.values)

 library(ineq)
  gini = ineq(Attri_DF$predict.score[,2], type="Gini")
  with(Attri_DF, table(Attrition, predict.class))
  auc
  KS
  gini



#Missclassification

nrow(Attri_DF)
table(Attrition)

total = 2940
total

mc <- (296+50)/ total
print(mc)
#11.76%



#Accuracy 
(1-mc)*100
#88.23%


#confusion matrix (training data)

conf.matrix <- table(Attri_DF$Attrition, predict(cart.tree2.bestcp,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#Miss Classfication 11.76%
#Accuracy 88.23%


#predict.class
            #  Pred:No Pred:Yes
#Actual:No     2416       50
#Actual:Yes     296      178

###############
#Optimum Model with Important variables obtained in Prunned Ptree Summary


# Split data into Train and Test

library(caret)
train = createDataPartition(Attri_DF$Attrition, list=FALSE, times=1, p=0.7)

train.data = Attri_DF[train,]
test.data = Attri_DF[-train,]


str(train.data)
str(test.data)
attach(train.data)
detach(Attri_DF)

#Making sure teh partition is right
prop.table((table(Attri_DF$Attrition)))
prop.table((table(train.data$Attrition)))
prop.table((table(test.data$Attrition)))

#CART model with minimum cp value with Training dataset (Development sample)

r.ctrl3 = rpart.control(minsplit=10, minbucket = 5, cp = 0, xval = 10)

cart.train.tree3 <- rpart(formula = Attrition ~ MonthlyIncome+OverTime+JobRole+TotalWorkingYears+Age+StockOptionLevel+HourlyRate
                                 +DailyRate+MaritalStatus+Department+MonthlyRate+YearsAtCompany+YearsInCurrentRole
                                +YearsWithCurrManager+EducationField+WorkLifeBalance+EnvironmentSatisfaction 
                                +DistanceFromHome+DistanceFromHome+ TrainingTimesLastYear, data = train.data[,-1], 
                                 method = "class", control = r.ctrl1)

#Summary to get variable importance from cart tree
summary(cart.train.tree3)
#print Splits and steps performed in cart tree
print(cart.train.tree3)

#Bucketing and classification scheame fo Cart
fancyRpartPlot(cart.train.tree3)


## to find how the tree performs
 printcp(cart.train.tree3)
 plotcp(cart.train.tree3)

 bestcp.train <- cart.train.tree3$cptable[which.min(cart.train.tree3$cptable[,"xerror"]),"CP"]
 cart.train.tree3.pruned <- prune(cart.train.tree3, cp = bestcp.train)

 r.ctrl3 = rpart.control(minsplit=10, minbucket = 5, cp = bestcp.train, xval = 10)

 cart.train.tree3.pruned1 <- rpart(formula = Attrition ~ MonthlyIncome+OverTime+JobRole+TotalWorkingYears+Age+StockOptionLevel+HourlyRate
                          +DailyRate+MaritalStatus+Department+MonthlyRate+YearsAtCompany+YearsInCurrentRole
                          +YearsWithCurrManager+EducationField+WorkLifeBalance+EnvironmentSatisfaction 
                          +DistanceFromHome+DistanceFromHome+ TrainingTimesLastYear, data = train.data[,-1], 
                          method = "class", control = r.ctrl1)

#Summary to get variable importance from cart tree
summary(cart.train.tree3.pruned1)
#print Splits and steps performed in cart tree
print(cart.train.tree3.pruned1)

#Bucketing and classification scheame fo Cart
fancyRpartPlot(cart.train.tree3.pruned1)


## to find how the tree performs
printcp(cart.train.tree3.pruned1)
plotcp(cart.train.tree3.pruned1)



conf.matrix <- table(train.data$Attrition, predict(cart.train.tree3.pruned1,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

nrow(train.data)

#                         Pred:Pred:No Pred:Pred:Yes
#Actual:Actual:No          1665            62
#Actual:Actual:Yes          218           114

#> (218+62)/2059
#[1]13.60%
#> (1-0.1359)*100
#[1] 86.41%

#Post prnning 
#Pred:No Pred:Yes
#Actual:No     1705       22
#Actual:Yes     198      134

detach(test.data)
attach(train.data)
 ## Scoring syntax
 train.data$predict.class <- predict(cart.train.tree3.pruned1, train.data, type="class")
 train.data$predict.score <- predict(cart.train.tree3.pruned1, train.data)

 library(ROCR)
 pred <- prediction(train.data$predict.score[,2], train.data$Attrition)
 summary(pred)
 perf <- performance(pred, "tpr", "fpr")
 plot(perf)
 KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
 auc <- performance(pred,"auc"); 
 auc <- as.numeric(auc@y.values)

 library(ineq)
 gini = ineq(train.data$predict.score[,2], type="Gini")
 with(train.data, table(Attrition, predict.class))
 auc
 KS
 gini

#predict.class
#Attrition   No  Yes
#No  1665   62
#Yes  218  114
#> auc
#[1] 0.6834742
#> KS
#[1] 0.3325148
#> gini
#[1] 0.3077804

  #CART model with minimum cp value with Test dataset (Validation sample sample)
  detach(train.data)
  attach(test.data)
  nrow(test.data)
  
  r.ctrl3 = rpart.control(minsplit=5, minbucket = 2, cp = 0, xval = 10)

 cart.test.tree3 <- rpart(formula = Attrition ~ MonthlyIncome+OverTime+JobRole+TotalWorkingYears+Age+StockOptionLevel+HourlyRate
                          +DailyRate+MaritalStatus+Department+MonthlyRate+YearsAtCompany+YearsInCurrentRole
                          +YearsWithCurrManager+EducationField+WorkLifeBalance+EnvironmentSatisfaction 
                          +DistanceFromHome+DistanceFromHome+ TrainingTimesLastYear, data = test.data[,-1], 
                          method = "class", control = r.ctrl1)

 #Summary to get variable importance from cart tree
  summary(cart.test.tree3)
 #print Splits and steps performed in cart tree
  print(cart.test.tree3)

 #Bucketing and classification scheame fo Cart
  fancyRpartPlot(cart.test.tree3)


  ## to find how the tree performs and prune
  printcp(cart.test.tree3)
  plotcp(cart.test.tree3)

  bestcp.test <- cart.test.tree3$cptable[which.min(cart.test.tree3$cptable[,"xerror"]),"CP"]
  cart.test.tree3.pruned <- prune(cart.test.tree3, cp = bestcp.test)

  r.ctrl3 = rpart.control(minsplit=10, minbucket = 5, cp = bestcp.test, xval = 10)

  cart.test.tree3.pruned1 <- rpart(formula = Attrition ~ MonthlyIncome+OverTime+JobRole+TotalWorkingYears+Age+StockOptionLevel+HourlyRate
                                  +DailyRate+MaritalStatus+Department+MonthlyRate+YearsAtCompany+YearsInCurrentRole
                                  +YearsWithCurrManager+EducationField+WorkLifeBalance+EnvironmentSatisfaction 
                                  +DistanceFromHome+DistanceFromHome+ TrainingTimesLastYear, data = test.data[,-1], 
                                  method = "class", control = r.ctrl1)

  #print Splits and steps performed in cart tree
  print(cart.test.tree3.pruned1)
  
  #Bucketing and classification scheame fo Cart
  fancyRpartPlot(cart.test.tree3.pruned1)

conf.matrix <- table(test.data$Attrition, predict(cart.test.tree3.pruned1,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

nrow(test.data)
 ##             Pred:No Pred:Yes
#Actual:No      714       25
#Actual:Yes      94       48

#> (96+25)/881
#[1]13.73%
#> (1-0.1373)*100
#[1] 86.27%

detach(train.data)
attach(test.data)
 ## Scoring syntax
 test.data$predict.class <- predict(cart.test.tree3.pruned1, test.data, type="class")
 test.data$predict.score <- predict(cart.test.tree3.pruned1, test.data)

 ibrary(ROCR)
 pred <- prediction(test.data$predict.score[,2], test.data$Attrition)
 summary(pred)
 perf <- performance(pred, "tpr", "fpr")
 plot(perf)
 KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
 auc <- performance(pred,"auc"); 
 auc <- as.numeric(auc@y.values)

 library(ineq)
 gini = ineq(test.data$predict.score[,2], type="Gini")
 with(test.data, table(Attrition, predict.class))
 auc
 KS
 gini

##> auc
#[1] 0.6836465
#> KS
#[1] 0.3278698
#> gini
#[1] 0.3080926
 nrow(train.data)





