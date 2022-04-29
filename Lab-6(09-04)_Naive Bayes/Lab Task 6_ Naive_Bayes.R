rm(list=ls())

install.packages('naivebayes')
install.packages('psych') #For personality research
install.packages('dplyr')
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
dt<-read.csv('D:\\Lab Task 5.csv')
str(dt)
dt$admit<-as.factor(dt$admit)
dt$rank<-as.factor(dt$rank)

pairs.panels(dt) # to check the independence of attributes among them

dt %>% # to have initial look up of 
  ggplot(aes(x=admit,y=gre,fill=admit))+
  geom_boxplot()+
  ggtitle('Admit Box Plot based on GRE Score')
dt %>%
  ggplot(aes(x=gre,fill=admit))+
  geom_density(alpha=0.75,color='black')+
  ggtitle('Density Plot based on GRE Score')

set.seed(234)
smpl<-sample(2,nrow(dt),replace=T,prob=c(0.8,0.2))
train<-dt[smpl==1,]
test<-dt[smpl==2, ]

#P(Admit=1|Rank=1)= P(Admit=1)*P(Rank=1|Admit=1)/P(Rank=1)

mdl<-naive_bayes(admit~ .,data=train)
mdl
plot(mdl)

p<-predict(mdl,train,type='prob')
head(cbind(p,train))

#To find the accuracy of prediction

p1<-predict(mdl,train)
(tab1<-table(p1,train$admit))
1-sum(diag(tab1))/sum(tab1)
