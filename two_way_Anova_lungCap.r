                 #Two way Anova in R using LungCapacity Dataset #


# Read csv file

LungCapData<-read.csv(file.choose(), header=T)

#See how many and what type of variables LungCapdata holds

str(LungCapData)
attach(LungCapData)

#Count the Number of records in LungCap
nrow(LungCapData)
nrow(na.omit(LungCapData))

#see how many data points are in a given level of a variable
xtabs(~Smoke+Gender+Caesarean, LungCapData)

#Descriptive stats Summary 

lung_cc <- LungCapData %>% 
  group_by(Smoke,Gender) %>%
  summarise( MinCount = min(LungCap.cc.), 
             MaxCount = max(LungCap.cc.),
             MedianCount= median(LungCap.cc.),
             CountRange = IQR(LungCap.cc.))
lung_cc

#Two way Anova to see whether thre Independent variable Smoke+gender+Caesarean holds any significant impact on Lung Capacity
twoway_anova<- aov(LungCap.cc.~ Caesarean*Smoke*Gender,LungCapData,projections = TRUE)
summary(twoway_anova)

#The Anova table produced by Summary,indicate Only Gender and Smoke independently has impact
# over LungCapcity,Born with Caesarean independently or interaction with SMoke and Gender doesn't have any
# significant impact on LungCapacity


#So we will dig more into Impact of SMoke and Gender on LungCapacity
#Two way Anova test with 2 factors( )Smoke,Gender) and one dependent variable (LungCap.CC.)

twoway_anova_1<- aov(LungCap.cc.~ Smoke*Gender,LungCapData)
summary(twoway_anova_1)

#From the above Output Smoke have Significant impact on LungCacpacity cc with 99.99 % Confidence Ineterval
#Gender has Significant impact on LungCapacity with 99.999 % Confidence Ineterval.
#Smoke and Gender interaction doesn't have significant impact on LungCapacity.

table(Smoke,Gender)

#From above crosstab ,there are 6 categories in combination of these 2 factors
#Two see which one holds significant effect on LungCapacity ,We need a post-hoc test. 
#Tukey HSD(Honestly Significant Difference) is provided in R

TukeyHSD(twoway_anova_1,conf.level = 0.99)

# The table/output shows us the difference between pairs, 
# the 99% confidence interval(s) and the p-value of the pairwise comparisons.
#From this Output it can be deduced that Only (Nonsmoker Male and Nonsmoker Female ) and 
# Smoker male with non smoker Female does have significant effect on Lung Capacity


#Box plot distribution of Lung Capacity across The Smoke and Gender factors.
b <-boxplot(LungCap.cc.~ Gender*Smoke,main='BoxPlot Lungcap Male vs Female' )









