rm(list=ls())
install.packages('ClusterR')
install.packages('cluster')
dt<-read.csv('E:\\Winter 21-22\\CSE3506_Lab\\Lab sessions\\autos_K-Means.csv')
str(dt)
summary(dt)
dt$system<-as.factor(dt$system)
str(dt)
#the price distribution based on cylinder diameter(bore)
pdt<-dt[,c(3,12)]
plot(pdt, main="Price based on cyl bore")
km<-kmeans(pdt,3)
plot(pdt,col=(km$cluster+2))
km

#Checking for optimal 'K'

dt2<-pdt
ss<-(nrow(dt2)-1)*sum(apply(dt2,2,var))
for(i in 2:10) ss[i]<-sum(kmeans(dt2,centers = i)$withinss)
plot(1:10,ss,type='b',xlab='K',ylab='distortion')
