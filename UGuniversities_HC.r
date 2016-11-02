setwd("G:\\DataMining\\practAss")
getwd()


library(ggplot2)
library(car)
library(cluster)

univer <- read.csv("UGuniversities.csv")
nrow(univer)
summary(univer)
str(univer)

View(univer)
attach(univer)
#Removing commas from Expenses and make Expenses as Numeric from factor.
univer$Expenses <- as.numeric(gsub(",", "", univer$Expenses))

attach(univer)
plot(SAT~Expenses,univer,labels=Univ,pos=1,cex=.3)

plot(SAT~Expenses,univer)
with(univer,text(SAT~Expenses,labels=Univ,pos=3,cex=.8))

plot(SAT~Top10,univer)
with(univer,text(SAT~Top10,labels=Univ,pos=3,cex=.8))

with(univer,text(SAT~ GradRate,labels=Univ,pos=3,cex=.6))

plot(Expenses~Top10,univer)
with(univer,text(Expenses~Top10,labels=Univ,pos=3,cex=.6))

plot(SFRatio~Top10,univer)
with(univer,text(SFRatio~Top10,labels=Univ,pos=3,cex=.6))

plot(SFRatio~Expenses,univer)
with(univer,text(SFRatio~Expenses,labels=Univ,pos=3,cex=.6))

#Clustering required only Quantitative Features,Removing Compnay as Factor/character variable
univer1 <- univer[,-c(1,1)]

#There is quite variation in Numbers and integears across Expenses ,SFRatio and GradRate,NOrmalizatio needed here

m <- apply(univer1, 2,mean)
s <- apply(univer1, 2, sd)

univer2 <- scale(univer1,m,s)

#View normalized University dataset
fix(univer2)

#Calculating Euclidean distance

uni_distance =dist(univer2)
print(uni_distance,digits = 3)

summary(uni_distance ,digits = 3)
hist(uni_distance)

#Cluster dendogram with Complete  Linkage
univer2.c <- hclust(uni_distance)
plot(univer2.c)
plot(univer2.c,univer$Univ)
print(univer2.c)

#Cluster dendogram with Avarage Linkage

univer2.a <- hclust(uni_distance ,method = "average")
plot(univer2.a)
plot(univer2.a,univer$Univ)

#Cluster dendogram with Single Linkage

univer2.s <- hclust(uni_distance ,method = "single")
plot(univer2.s)
plot(univer2.s,univer$Univ)


#Cluster membership
member.c <-cutree(univer2.c,3)
member.a <-cutree(univer2.a,3)
member.s<-cutree(univer2.s,3)
print(member.c)

tca <- table(member.c,member.a)
View(tca)

table(member.c,member.s)

table(member.a,member.s)

aggregate(univer2,list(member.c),mean)
aggregate(univer2,list(member.a),mean)
aggregate(univer2,list(member.s),mean)

aggregate(univer[,-c(1,1)],list(member.c),mean)

#Silhouette plot
plot(silhouette(cutree(univer2.c,3),uni_distance))
plot(silhouette(cutree(univer2.a,3),uni_distance))
plot(silhouette(cutree(univer2.s,3),uni_distance))






