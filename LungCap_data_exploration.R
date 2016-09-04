# First convert the dataset into csv file
# Read.csv tells R to read csv file and 
# File.choose helps you to select the file you need with a dialogue prompt
# Leave the filename blank, so that you can choose the file
setwd('G:\\R-Programming\\Assignments\\Graded_assignment\\2')
getwd()
# YOu can either keep file.choose as blank so as to choose fil at Run time or set the wrking directory and then 
# From working directory give the exact file name. 

#LungCapData1 <- read.csv(file.choose(),header = TRUE)

LugCapData1 <- read.csv('LungCapData-2.csv',header = TRUE)

#LungCapData2 <- read.table(file.choose(),header = T ,sep = ",")
# See file content
str(LungCapData1)

#Length of Lungcap
length(LungCapData1$Smoke)

#Importing TAB delimitted file
#LungCapData3 <- read.delim(file.choose(),header = T)

# Dimension will give bservations and variable count from dataset
dim(LungCapData1)
head(LungCapData1)
tail(LungCapData1)

# Viewing particular Rows from Dataset
LungCapData1[c(11,12,13,14,15), ]
LungCapData1[c(20:30), ]

#check names of variables
names(LungCapData1)

#Getting Maean of variable AGE from LungCapData2

mean(LungCapData1$Age..years.)
#Second way to get Age mean directly without $

attach(LungCapData1)
mean(Age)
#detach(LungCapData1)
mean(LungCap.cc.)
mean(LungCap.cc.,trim = 0.10)
#Median for Lungcapcity variable
median(LungCap.cc.)
#variance of Lungcapacity variable
var(LungCap.cc.)
sd(LungCap.cc.)
min(LungCap.cc.)
max(LungCap.cc.)
range(LungCap.cc.)

summary(LungCapData1)

#Command to see Datatype of Data set Variables
class(LungCap.cc.)
class(Age..years.)
class(Gender)
class(Caesarean)
#Command to see differnt categories or levels in any variable.

levels(Gender)
levels(Smoke)

#summary of Data set

summary(LungCapData1)
#Claculating Mean only for Females
mean(LungCapData1$Age..years.[Gender == 'female'])
mean(LungCapData1$Age..years.[Gender == 'male'])
#Calculating mean LungCapacity for Male and Female
mean(LungCapData1$LungCap.cc.[Gender == 'female'])
mean(LungCapData1$LungCap.cc.[Gender == 'male'])

#Calculating mean LungCapacity for Male and Female who smokes
mean(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'yes'])
mean(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'yes'])

#Calculating median LungCapacity for Male and Female who smokes

median(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'yes'])
median(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'yes'])

#Calculating variance LungCapacity for Male and Female who smokes

var(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'yes'])
var(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'yes'])

#Calculating standard deviation LungCapacity for Male and Female who smokes

sd(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'yes'])
sd(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'yes'])

#NOn smokers Aggregates -


#Calculating mean LungCapacity for Male and Female who do not smoke smokes
mean(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'no'])
mean(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'no'])

#Calculating median LungCapacity for Male and Female who dont smokes

median(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'no'])
median(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'no'])

#Calculating variance LungCapacity for Male and Female who dont smokes

var(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'no'])
var(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'no'])

#Calculating standard deviation LungCapacity for Male and Female who dont smokes

sd(LungCapData1$LungCap.cc.[Gender == 'female' & Smoke == 'no'])
sd(LungCapData1$LungCap.cc.[Gender == 'male' & Smoke == 'no'])

count(LungCapData1$Gender[Gender == 'male' & Caesarean == 'yes'])

#Claculating summarised metrix Grouped by all factpr variables with aggregate on Numeric variables.

library(plyr)
lun_agg <- ddply(LungCapData1,.(Gender,Smoke,Caesarean),summarize,Avg_lung_cap=mean(LungCap.cc.),
                 Std_lung_cap=sd(LungCap.cc.),Max_lungcap=max(LungCap.cc.),Min_lungcap=min(LungCap.cc.),avg_age=mean(Age..years.),
                 max_age=max(Age..years.),min_age=min(Age..years.),
                avg_height=mean(Height.inches.),max_height=max(Height.inches.),min_height=min(Height.inches.))

lun_agg


#Activating xtable in library
library("xtable")

#new x table vector variable to strore xtable object of Prodsumm
new_lungdata <- xtable(lun_agg)

#printing HTML output as per assignment requirment with grouping variables channel,region.
print.xtable(new_lungdata, type="html", file="Lung_aggregate_report2.html")
str(LungCapData1)
getwd()

MaleData <-LungCapData1 [(Gender =='male'),]
FeMaleData <-LungCapData1 [(Gender =='female'),]
dim(FeMaleData)
dim(MaleData)
summary(Gender)

#Calculate how many females smoke.

FemSmoke <- Gender =="female" & Smoke== "yes"
summary(FemSmoke)

#Graphical Exploration

#plot Gender -genric count of Male female

plot(Gender)

plot(Gender,Smoke, bg="black", col="white")

#Plotting scatter plot
plot(Age..years.,Height.inches., main = "Scatterplot",font.main=4,font.lab=2,col = c(2,8))
abline(lm(Height.inches.~Age..years.),col=4,lty=2,lwd=2)
summary(Gender)

#Scatter Plot for Males only
plot(Age..years.[Gender == "male"],Height.inches.[Gender  =="male"],col = 4 ,pch="m",xlab="Age",ylab="Height"
     ,main =  "Height Vs Age Male")

#Scatter plot for Female 

plot(Age..years.[Gender == "female"],Height.inches.[Gender  =="female"],col = 4 ,pch="f",xlab="Age",ylab="Height"
     ,main =  "Height Vs Age Male")

#Dividing screen in two parts to accomodate Male and Female chart in diffrnt scatter chart.
#par(mfrow=c(1,2))

par(no.readonly=FALSE)
par()


#Scatter Plot for Males only
plot(Age..years.[Gender == "male"],Height.inches.[Gender  =="male"],xlab="Age",ylab="Height"
     ,main =  "Height Vs Age Male",xlim=c(0,20),ylim=c(45,85))

#Scatter plot for Female 

plot(Age..years.[Gender == "female"],Height.inches.[Gender  =="female"],xlab="Age",ylab="Height"
     ,main =  "Height Vs Age FeMale",xlim=c(0,20),ylim=c(45,85))


# Create cross tab

table(Gender,Smoke)

#boxplot

boxplot(MaleData,FeMaleData)
boxplot(LungCapData1$Age..years.,LungCapData1$Gender,LungCapData1$Smoke)

boxplot(LungCap.cc.)
quantile(LungCap.cc.,probs = c(0,0.25,0.5,0.75,1))

boxplot(LungCap.cc.,main='BoxPlot',ylab='lung Capacity',ylim =c(0,16) ,las=1)

boxplot(LungCap.cc.~ Gender,main='BoxPlot Lungcap Male vs Female' )

boxplot(LungCap.cc.[Gender == 'Female'],LungCap.cc.[Gender == 'Male'] )

#Barplots
barplot(LungCapData1[,1])
barplot(LungCapData1[,1],beside = TRUE)

table(Gender)
count_gen <- table(Gender)

#get proportion of male and Female in total population
summary(Gender)
Percent <- count_gen/725
Percent

#Bar chart for Actual count of male and female along with Proprtions
barplot(count_gen,main = "Male Vs Female count",xlab = "Gender" ,ylab = "Count")

# Barplot male and female along with Proprtions

barplot(Percent,main = "Male Vs Female count",xlab = "Gender" ,ylab = "%")

#align Yaxis % number horizontally

barplot(Percent,main = "Male Vs Female count",xlab = "Gender" ,ylab = "%" , las = 1)

# label the Male female properly

barplot(Percent,main = "Male Vs Female count",xlab = "Gender" ,ylab = "%" , las = 1,
        names.arg = c("Female","Male"))

# Change Bars fro vertical to horizontal

barplot(Percent,main = "Male Vs Female count",ylab = "Gender" ,xlab = "%" , las = 1,
        names.arg = c("Female","Male"),horiz = TRUE)

#Plotting piechart

pie(count_gen )

pie(Percent,main = 'Male Vs Female count')


#Histogram of Age vs heights
hist(Age..years.)
hist(LungCap.cc.)
#Density Histogram
hist(LungCap.cc.,frequency(Age..years.) ,breaks = 10)
hist(LungCap.cc.,frequency(Age..years.) ,breaks = 5)

#Histogram with Density and no Freq
hist(LungCap.cc.,frequency(Age..years.) ,breaks = 5,freq = FALSE)
hist(LungCap.cc.,prob=T,ylim=c(0,0.2),breaks = seq(from=0,to=16,by=2),main = "Hist of LungCap",
xlab="LungCapacity",las=1)

lines(density(LungCap.cc.))

#change color ofdensity line and also increase width

lines(density(LungCap.cc.),col =4 ,lwd=3)

hist(Age..years. ,breaks = 10 )
min(Age..years.)
mean(LungCap.cc.)
head(Height.inches.)

#Barplot for comparing men and women smokers
smoke_table1 <- table(Smoke,Gender)
smoke_table1

#extended Bar pot wth Tiltle,x axis title,and vertical values rearranged on yaxis.
barplot(smoke_table1,beside=T,legend.text = c("Non-Smoker","Smoker"),xlab = "Gender" ,
        main = "Gender and SMoking",las=1)
#Change color of Female Smoking and non smoking bars
barplot(smoke_table1,beside=T,legend.text = c("Non-Smoker","Smoker"),xlab = "Gender" ,
  
              main = "Gender and SMoking",las=1,col=c(4,6))
#Mosaic Plot
mosaicplot(smoke_table1,beside=T,legend.text = c("Non-Smoker","Smoker"),xlab = "Gender" ,
        main = "Gender and SMoking",las=1,col=c(2,6))

#Summary Statistics
#Smok summary total
table(Smoke)/725
table(Smoke)/725

table(Smoke,Gender)
table(Smoke,Gender)/length(Smoke)*100

