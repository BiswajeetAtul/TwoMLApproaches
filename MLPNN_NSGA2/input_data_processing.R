rm(list = ls())
library(ggplot2)
library(caret)
library(nnet)
library(quantmod) #for Lag

#Read the dataset into the workspace
train_data=read.csv("D:/BTECHPROJECT/projectdataset/testing.csv",header=T)
test_data=read.csv("D:/BTECHPROJECT/projectdataset/testing_1.csv",header=T)
#the input dataset is in dataframe format
class(train_data)

#convert it to time series format
train=ts(train_data$windspd, start=c(1), freq=24)
class(train)

plot(train)

#Perform ACF on the dataset
acf(train)
pacf(train)

#Generate Lower and Upper bound of dataset
train.LB=data.frame(train0.9=0.9*train_data$windspd)
train.UB=data.frame(train1.1=1.1*train_data$windspd)



trainingData <- data.frame(train_data$windspd, x1=Lag(train_data$windspd,1), x2=Lag(train_data$windspd,2))
names(trainingData) <- c("windspd", "windspd1", "windspd2")
model <- train(windspd ~ x1 + x2, traindata, method='nnet', linout=TRUE, trace = FALSE)
#Grid of tuning parameters to try:
#tuneGrid=expand.grid(.size=c(1,5,10),.decay=c(0,0.001,0.1))) 

testdata <- data.frame(test_data$windspd, x1=Lag(test_data$windspd), x2=Lag(test_data$windspd,2))
names(testdata) <- c("windspd", "x1", "x2")
ps <- predict(model, testdata)
#a=cbind(test,ps)
#ps=ts(ps,start=c(1),frequency = 24)
#plot(ps,type="l",xlim=c(0,50),ylim=c(0,50),col="red",xlab#plot(testdata$windspd,type="l",xlim=c(0,50),ylim=c(0,50),xlab = "day",ylim="speed")
#par(new=T)


plot(test_data$windspd,type="l",xlim=c(0,50),ylim=c(0,50),xlab = "day",ylab="speed")
par(new=T)
plot(ps,type="l",xlim=c(0,50),ylim=c(0,50),col="red",xlab = "day",ylab="speed")

