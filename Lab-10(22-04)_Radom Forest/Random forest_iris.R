rm(list=ls())
#install.packages("stats19")
#install.packages("dplyr")
#install.packages("randomForest")
library('stats19')
library('dplyr')
library('randomForest')
mydata=iris
View(mydata)
str(mydata)
index=sample(2,nrow(mydata), replace=TRUE,prob=c(0.7,0.3))
training=mydata[index==1,]
testing=mydata[index==2,]
RFM <- randomForest(Species ~ .,data=training, importance=T, proximity=T)
Species_Pred=predict(RFM,testing)
testing$Species_Pred=Species_Pred
View(testing)

CFM=table(testing$Species,testing$Species_Pred)
CFM

             