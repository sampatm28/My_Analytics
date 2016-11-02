#install.packages("popbio")
#install.packages("mice")
#install.packages("ineq")
#install.packages("caret")
#install.packages('NCStats')
#install.packages('ROCR')
#install.packages('ROSE')

#library('mlr')
#detach("package:mlr",unload=TRUE)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)
library(mice)
library(ROSE)
library(popbio)
library(partykit)
library(ROCR)
library(data.table)
library(ineq)
detach("package:InformationValue",unload=TRUE)

setwd("G:\\DataMining\\Resi5\\Assignment")
getwd()


## Let us import the data that we intend to use for modeling

Emp_attr = read.csv("1452762979_586__HR_Employee_Attrition_Data.csv")

Emp_attrition <-
  subset(Emp_attr, select = -c(StandardHours, EmployeeCount, Over18))

str(Emp_attrition)
attach(Emp_attrition)

summarizeColumns(Emp_attrition)

decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
  deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
  ifelse(x<deciles[1], 1,
         ifelse(x<deciles[2], 2,
                ifelse(x<deciles[3], 3,
                       ifelse(x<deciles[4], 4,
                              ifelse(x<deciles[5], 5,
                                     ifelse(x<deciles[6], 6,
                                            ifelse(x<deciles[7], 7,
                                                   ifelse(x<deciles[8], 8,
                                                          ifelse(x<deciles[9], 9, 10
                                                          ))))))))))
}

# Split data into Train and Test

library(caret)
train = createDataPartition(Attrition, list=FALSE, times=1, p=0.7)

train.data = Emp_attrition[train,]
test.data = Emp_attrition[-train,]


str(train.data)
nrow(test.data)
attach(train.data)

#Making sure teh partition is right
prop.table((table( Emp_attrition$Attrition)))
prop.table((table(train.data$Attrition)))
prop.table((table(test.data$Attrition)))


 ##install.packages("randomForest")
 library(randomForest)

 ## Calling syntax to build the Random Forest

    RF <- randomForest(Attrition~., data = train.data, 
                       ntree=500, mtrystart = 1, 
                       stepFactor = 1.5, 
                       nodesize = 10,
                       improve = 0.001,
                       trace=TRUE, 
                       plot = TRUE,
                       doBest = TRUE,
                       importance=TRUE)

?randomForest

print(RF)
varImp(RF)
varImpPlot(RF)
plot(RF)
importance(RF)
varImpPlot(RF,type=2)

plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Training Sample")
RF$err.rate

#Predict on Training (dev) Sample
train.data$predict.class <- predict(RF, train.data, type="class")
train.data$predict.score <- predict(RF, train.data,type="prob")
head(train.data)

r <- table(train.data$predict.class,train.data$Attrition)

#install.packages('e1071')
require('e1071')
library('e1071')

confusionMatrix(r)

pred <- prediction(train.data$predict.score[,2], train.data$Attrition)
summary(pred)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(train.data$predict.score[,2], type="Gini")
with(train.data, table(Attrition, predict.class))
auc
KS
gini


#confusion matrix (training data)

conf.matrix <- table(train.data$Attrition, predict(RF,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#Pred:No Pred:Yes
#Actual:No     1721        6
#Actual:Yes     190      142

#Prediction on Test sample 

#Model Validation on test data
detach(train.data)
attach(test.data)

test.data$predict.class <- NULL
test.data$predict.score <- NULL

RF_Test <- randomForest(Attrition~., data = test.data, 
                   ntree=200, mtrystart = 3, stepFactor = 1.5, nodesize = 10,improve = 0.001,
                   trace=TRUE, 
                   plot = TRUE,
                   doBest = TRUE,
                   importance=TRUE)

nrow(test.data)

#Plot Error rates for Validation or test sample

plot(RF_Test, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest Validation Sample")

print(RF_Test)

test.data$predict.class <- predict(RF, test.data, type="class")
test.data$predict.score <- predict(RF, test.data, type="prob")
View(test.data)



#KS,Gini,AUC for test.data

pred <- prediction(test.data$predict.score[,2], test.data$Attrition)
summary(pred)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

gini = ineq(test.data$predict.score[,2], type="Gini")
with(test.data, table(Attrition, predict.class))
auc
KS
gini


#predict.class
##Attrition  No Yes
#No  737   2
#Yes  83  59
#> auc
#[1] 0.9116192
#> KS
#[1] 0.7067697
#> gini
#[1] 0.5421213
#> 


#confusion matrix (testing data)

conf.matrix.test <- table(test.data$Attrition, predict(RF_Test,type="class"))
rownames(conf.matrix.test) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix.test) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix.test)

r <- table(test.data$predict.class,test.data$Attrition)

#install.packages('e1071')
require('e1071')
library('e1071')

confusionMatrix(r)

#Pred:Pred:No Pred:Pred:Yes
#Actual:Actual:No           734             5
#Actual:Actual:Yes          106            36
#Missclassification 12.60%
#Accuracy 88.40%



##class(randomForest::importance(RF))
## List the importance of the variables.
##impVar <- round(randomForest::importance(RF), 2)
##impVar[order(impVar[,3], decreasing=TRUE),]

## Tuning Random Forest
tRF <- tuneRF(x = test.data, 
              y = Attrition,
              mtryStart = 3, 
              ntreeTry=50, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 10, 
              importance=TRUE
              )
print(tRF)
train.data$predict.class <- predict(tRF, train.data, type="class")
train.data$predict.score <- predict(tRF, train.data,type="prob")
head(train.data)


test.data$predict.class <- predict(tRF, test.data, type="class")
test.data$predict.score <- predict(tRF, test.data, type="prob")
View(test.data)

table(test.data$Attrition,test.data$predict.class)
table(train.data$Attrition,train.data$predict.class)


## deciling
train.data$deciles <- decile(train.data$predict.score[,2])
View(train.data)

library(data.table)
tmp_DT = data.table(train.data)
rank <- tmp_DT[, list(
  cnt = length(Attrition), 
  cnt_Retention = sum(Attrition == 'No'), 
  cnt_Attrtion = sum(Attrition == 'Yes')) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_Retention * 100 / rank$cnt,2);
rank$cnt_Retention <- cumsum(rank$cnt_Retention)
rank$cnt_Attrtion <- cumsum(rank$cnt_Attrtion)
rank$cum_perct_Retention <- round(rank$cnt_Retention * 100 / sum(rank$cnt_Retention),2);
rank$cum_perct_Attrtion <- round(rank$cnt_Attrtion * 100 / sum(rank$cnt_Attrtion),2);
rank$ks <- abs(rank$cum_perct_Retention - rank$cum_perct_Attrtion);
View(rank)






