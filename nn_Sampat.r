#install.packages("popbio")
#install.packages("mice")
#install.packages("ineq")
#install.packages("caret")
#install.packages('NCStats')
#install.packages('ROCR')
#install.packages('ROSE')
#install.packages('woe')
#install.packages('riv')
#install.packages("InformationValue")  

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
library(caret)
library(car)
library(woe)
library(riv)
#library(InformationValue)
library(neuralnet)
require('e1071')
library('e1071')
library('corrplot')
#detach("package:InformationValue",unload=TRUE)

setwd("G:\\DataMining\\Resi5\\Assignment")
getwd()

Emp_attrition = read.csv("1452762979_586__HR_Employee_Attrition_Data.csv")

str(Emp_attrition)
attach(Emp_attrition)

iv<-function(predit,target, details) 
{
  data<-data.frame(predit,target);
  data_sort<-data[order(predit),]
  
  temp_str <- c("Bin", "Good_pct", "Bad_pct", "IV_Bin")
  ifelse((details=="Yes"),print(temp_str),"")
  
  ttl_num<-length(target);
  bin<-10;
  n<-ttl_num%/%bin;
  iv_bin<-rep(0,times=bin);
  good<-rep(0,times=bin);
  bad<-rep(0,times=bin);
  for (i in 1:bin) # calculate PSI for ith bin
  {
    if(i!=bin) {good[i]<-sum(data_sort$target[((i-1)*n+1):(n*i)]);bad[i]<-n-good[i]} else
    {good[i]<-sum(data_sort$target[((i-1)*n+1):ttl_num]);bad[i]<-ttl_num-n*(i-1)-good[i]}
  }
  
  good_pct<-good/sum(good)
  bad_pct<-bad/sum(bad)
  
  for (i in 1:bin)
  {
    iv_bin[i]<-(bad_pct[i]-good_pct[i])*log(bad_pct[i]/good_pct[i])
    
    temp1=    c(round(i,0), round(good_pct[i],2),round(bad_pct[i],2)
                ,round(iv_bin[i],2))
    ifelse((details=="Yes"),print(temp1),"")
    
    
  }
  IV_Data <- read.table(textConnection(
    temp_str), header = TRUE) 
  
  head(IV_Data,10)
  iv=sum(iv_bin)
  return (iv)
}
Emp_attrition$Resp<-ifelse(Emp_attrition$Attrition=="No",0,1)

# Explain IV formula
?riv

iv(Emp_attrition$DistanceFromHome,Emp_attrition$Resp,"Yes") -#0.0713
iv(Emp_attrition$Age,Emp_attrition$Resp,"Yes")  #0.311
iv(Emp_attrition$BusinessTravel,Emp_attrition$Resp,"Yes") #0.133
iv(Emp_attrition$DailyRate,Emp_attrition$Resp,"Yes") #0.08
iv(Emp_attrition$Department,Emp_attrition$Resp,"Yes") #0.078
iv(Emp_attrition$Education,Emp_attrition$Resp,"Yes") #0.016
iv(Emp_attrition$EducationField,Emp_attrition$Resp,"Yes") #0.040
iv(Emp_attrition$EmployeeCount,Emp_attrition$Resp,"Yes") #0.0214
iv(Emp_attrition$EmployeeNumber,Emp_attrition$Resp,"Yes") #0.0214
iv(Emp_attrition$EnvironmentSatisfaction,Emp_attrition$Resp,"Yes") #0.106
iv(Emp_attrition$Gender,Emp_attrition$Resp,"Yes") #0.012
iv(Emp_attrition$HourlyRate,Emp_attrition$Resp,"Yes") #0.0602
iv(Emp_attrition$JobInvolvement,Emp_attrition$Resp,"Yes") #0.122
iv(Emp_attrition$JobLevel,Emp_attrition$Resp,"Yes")  #0.328
iv(Emp_attrition$JobRole,Emp_attrition$Resp,"Yes") #0.3862
iv(Emp_attrition$JobSatisfaction,Emp_attrition$Resp,"Yes") #0.0922
iv(Emp_attrition$MaritalStatus,Emp_attrition$Resp,"Yes") #0.20
iv(Emp_attrition$MonthlyIncome,Emp_attrition$Resp,"Yes") #0.41
iv(Emp_attrition$MonthlyRate,Emp_attrition$Resp,"Yes") #0.05411
iv(Emp_attrition$NumCompaniesWorked,Emp_attrition$Resp,"Yes") #0.11
iv(Emp_attrition$Over18,Emp_attrition$Resp,"Yes")  #0.021
iv(Emp_attrition$OverTime,Emp_attrition$Resp,"Yes") #0.37
iv(Emp_attrition$PercentSalaryHike,Emp_attrition$Resp,"Yes") #0.0317
iv(Emp_attrition$PerformanceRating,Emp_attrition$Resp,"Yes")  #0.013
iv(Emp_attrition$RelationshipSatisfaction,Emp_attrition$Resp,"Yes") #0.033
iv(Emp_attrition$StandardHours,Emp_attrition$Resp,"Yes") #0.0214
iv(Emp_attrition$StockOptionLevel,Emp_attrition$Resp,"Yes")  #0.2876
iv(Emp_attrition$TotalWorkingYears,Emp_attrition$Resp,"Yes") #0.3682
iv(Emp_attrition$TrainingTimesLastYear,Emp_attrition$Resp,"Yes") #0.0624
iv(Emp_attrition$WorkLifeBalance,Emp_attrition$Resp,"Yes")  #0.0626
iv(Emp_attrition$YearsAtCompany,Emp_attrition$Resp,"Yes") #0.31 
iv(Emp_attrition$YearsInCurrentRole,Emp_attrition$Resp,"Yes") #0.295
iv(Emp_attrition$YearsSinceLastPromotion,Emp_attrition$Resp,"Yes")  #0.0311
iv(Emp_attrition$YearsWithCurrManager,Emp_attrition$Resp,"Yes") #0.294

iv(Emp_attrition$Age,Emp_attrition$Resp,"Yes")  #0.311
iv(Emp_attrition$BusinessTravel,Emp_attrition$Resp,"Yes") #0.133
iv(Emp_attrition$EnvironmentSatisfaction,Emp_attrition$Resp,"Yes") #0.106
iv(Emp_attrition$JobInvolvement,Emp_attrition$Resp,"Yes") #0.122
iv(Emp_attrition$JobLevel,Emp_attrition$Resp,"Yes")  #0.328
iv(Emp_attrition$JobRole,Emp_attrition$Resp,"Yes") #0.3862
iv(Emp_attrition$JobSatisfaction,Emp_attrition$Resp,"Yes") #0.0922
iv(Emp_attrition$MaritalStatus,Emp_attrition$Resp,"Yes") #0.20
iv(Emp_attrition$MonthlyIncome,Emp_attrition$Resp,"Yes") #0.41
iv(Emp_attrition$NumCompaniesWorked,Emp_attrition$Resp,"Yes") #0.11
iv(Emp_attrition$OverTime,Emp_attrition$Resp,"Yes") #0.37
iv(Emp_attrition$StockOptionLevel,Emp_attrition$Resp,"Yes")  #0.2876
iv(Emp_attrition$TotalWorkingYears,Emp_attrition$Resp,"Yes") #0.3682
iv(Emp_attrition$YearsAtCompany,Emp_attrition$Resp,"Yes") #0.31 
iv(Emp_attrition$YearsInCurrentRole,Emp_attrition$Resp,"Yes") #0.295
iv(Emp_attrition$YearsWithCurrManager,Emp_attrition$Resp,"Yes") #0.294


# drop the Variables that will be of limited significance for Analyzing the Attrition trends.

Emp_attrition$DailyRate=NULL
Emp_attrition$DailyRate=NULL 
Emp_attrition$EmployeeCount=NULL 
Emp_attrition$EmployeeNumber=NULL 
Emp_attrition$MonthlyRate=NULL 
Emp_attrition$DailyRate=NULL 
Emp_attrition$Education=NULL 
Emp_attrition$EducationField=NULL 
Emp_attrition$HourlyRate=NULL 
Emp_attrition$Over18=NULL 
Emp_attrition$RelationshipSatisfaction=NULL
Emp_attrition$StandardHours=NULL
Emp_attrition$TotalWorkingYears=NULL
Emp_attrition$PercentSalaryHike=NULL
Emp_attrition$PerformanceRating =NULL
Emp_attrition$MonthlyRate=NULL
Emp_attrition$Gender=NULL
Emp_attrition$DistanceFromHome=NULL
Emp_attrition$TrainingTimesLastYear=NULL
Emp_attrition$Department=NULL
Emp_attrition$YearsSinceLastPromotion=NULL
names(Emp_attrition)
str(Emp_attrition)


#As per iv metric we will consider variable with more than 0.1 IV value as Imporatant varible 
# for prediction in Neura network model.

#From correlation matrix Jobleveland Monthl income are corelated ,so we will choose joblevel.
#Also YearsAtCompany,YearsInCurrentRole,YearsWithCurrManager are corelated so,we will take 
#YearsAtCompany as oneo f variable out of these 3

Emp_attrition$YearsInCurrentRole=NULL
Emp_attrition$YearsWithCurrManager=NULL
Emp_attrition$MonthlyIncome=NULL

nrow(Emp_attrition)
str(Emp_attrition)




#Information Value Predictive Power 
#< 0.02 useless for prediction 
#0.02 to 0.1 Weak predictor 
#0.1 to 0.3 Medium predictor 
#0.3 to 0.5 Strong predictor 
#>0.5 Suspicious or too good to be true 

attach(Emp_attrition)

# Split data into Train and Test

library(caret)
train.nn = createDataPartition(Attrition, list=FALSE, times=1, p=0.7)

train.data.nn = Emp_attrition[train.nn,]
test.data.nn = Emp_attrition[-train.nn,]


str(train.data.nn)
nrow(test.data.nn)
nrow(train.data.nn)
detach(Emp_attrition)
attach(train.data.nn)

#Making sure teh partition is right
prop.table((table( Emp_attrition$Attrition)))
prop.table((table(train.data.nn$Attrition)))
prop.table((table(test.data.nn$Attrition)))


#Running  NN on Trainining dataset
library(neuralnet)
nn2 <- neuralnet(Resp~Age+as.numeric(BusinessTravel)+EnvironmentSatisfaction+JobInvolvement+
                   JobLevel+as.numeric(JobRole)+JobSatisfaction+as.numeric(MaritalStatus)
                 +NumCompaniesWorked+as.numeric(OverTime)+StockOptionLevel+YearsAtCompany,
                  data=train.data.nn,hidden = 4,err.fct = "sse",linear.output = FALSE,
                 lifesign.step = 10,lifesign = "full" ,
                 threshold = 0.1,stepmax = 20000
                 #learningrate = 0.001,
                # algorithm = "backprop"
                )



quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

print(nn2)
plot(nn2)

nn2$result.matrix

misClassTable = data.frame(class= train.data.nn$Attrition, Prediction = nn2$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.5,1,0)
with(misClassTable, table(Attrition, Classification))

#Classification
#Attrition    0    1
#No  1684   43
#Yes  168  164
#MC 10.24%
#Accuracy 89.80%

#Confusion metrix

r <- table(Resp, misClassTable$Classification)
confusionMatrix(r)



#Classification
#Resp    0    1
#0 2402   64
#1  308  166

#MC 12.65% Acc 87.35%

#Running  NN on Testing dataset

detach(train.data.nn)
attach(test.data.nn)
nn2.test <- neuralnet(Resp~Age+as.numeric(BusinessTravel)+EnvironmentSatisfaction+JobInvolvement+
                   JobLevel+as.numeric(JobRole)+JobSatisfaction+as.numeric(MaritalStatus)
                 +NumCompaniesWorked+as.numeric(OverTime)+StockOptionLevel+YearsAtCompany,
                 data=test.data.nn,hidden = 4,err.fct = "sse",linear.output = FALSE,
                 lifesign.step = 10,lifesign = "full" ,
                 threshold = 0.1,stepmax = 20000
                 #learningrate = 0.001,
                 # algorithm = "backprop"
)


print(nn2)
plot(nn2)

nn2$result.matrix

quantile(nn2$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable = data.frame(class= test.data.nn$Attrition, Prediction = nn2$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.5,1,0)
with(misClassTable, table(Attrition, Classification))

#Classification
#Attrition    0    1
#No  1684   43
#Yes  168  164
#MC 10.24%
#Accuracy 89.80%

#Confusion metrix

r <- table(Resp, misClassTable$Classification)
confusionMatrix(r)




str(Emp_attrition)
#learningrate = 0.005,
#algorithm = "backprop",
#stepmax = 2000)
#learningrate = 0.005,
#algorithm = "backprop",
#stepmax = 2000)


nn3 <- neuralnet(Resp~Age+as.numeric(BusinessTravel)+EnvironmentSatisfaction+JobInvolvement+
                   JobLevel+as.numeric(JobRole)+JobSatisfaction+as.numeric(MaritalStatus)+MonthlyIncome
                 +NumCompaniesWorked+as.numeric(OverTime)+StockOptionLevel+TotalWorkingYears+YearsAtCompany
                 +YearsInCurrentRole+YearsWithCurrManager,
                 data=Emp_attrition,hidden = 5,err.fct = "sse",linear.output = FALSE,
                 lifesign.step = 10,lifesign = "full" ,
                 threshold = 0.1,stepmax = 20000, learningrate = 0.005,
                 algorithm = "backprop")

quantile(nn3$net.result[[1]], c(0,1,5,10,25,50,75,90,95,99,100)/100)

misClassTable = data.frame(class= Emp_attrition$Resp, Prediction = nn3$net.result[[1]] )
misClassTable$Classification = ifelse(misClassTable$Prediction>0.5,1,0)
with(misClassTable, table(Attrition, Classification))
nrow(Emp_attrition) #2940

rattle()

