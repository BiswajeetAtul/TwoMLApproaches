#rm(list=ls)
library(elmNN)
#library(ELMR)
library(quantmod) #for Lag
#library(confReg)
#library(e1071)
#set.seed(1234)
data.1.a<- read.csv("D:/BTECHPROJECT/projectdataset/testing.csv",header=T);
#####################PreProcessing the data##############################
#(processed_data<-preProcess(data))
#class(data)
#preprocessing the data:
#data.1.b<-data.frame(data.1.a$windspd)
#data.1.c<-preProcess(data.1.b)

#data.1.d<-data.frame(data.1.c, x1=Lag(data.1.c,1), x2=Lag(data.1.c,2))
#(data.1.d)
####################################################
#data.2.a<-list()
#---------------normalizing the data-----------------
max=max(data.1.a$windspd)
min=min(data.1.a$windspd)
#--------------------normalization-------------------------
data.2.a<-(data.1.a$windspd-min)/(max-min)
normalized_data<-data.frame(now=data.2.a, x1=Lag(data.2.a,1), x2=Lag(data.2.a,2))
#--------------------applying the ELM----------------------
#--------------------creating the elm model----------------
Elm_MOdel<-elmtrain(normalized_data$now~normalized_data$Lag.1+normalized_data$Lag.2,data=normalized_data, nhid = 10, actfun = "sig")
(Elm_MOdel)
#---------------------Predicting the values----------------
Elm_Pred<-predict(Elm_MOdel,newdata =normalized_data)



#----------
plot( data.2.a, type="l", col="yellow",ylim = c(0,1),xlim=c(0,800),xlab = "hours",ylab="normalized Windspeed" )
par(new=TRUE)
plot(Elm_Pred, type="l", col="blue",ylim = c(0,1),xlim = c(0,800),xlab = "hours",ylab="normalized Windspeed"  )
legend(600,0.9,legend=c("target","actual"),col=c("blue","red"),lwd=2,lty = 1:1,cex=0.8)
#------------

#--------------Statistics Calculation-----------
Prediction_Mean<-mean(Elm_Pred)
Prediction_STD.Deviation<-sd(Elm_Pred)
Prediction_Size<-length(Elm_Pred)


Prediction_Error<-qt(0.95,df=Prediction_Size-1)*Prediction_STD.Deviation/sqrt(Prediction_Size)
left<-Prediction_Mean- Prediction_Error
right<-Prediction_Mean+ Prediction_Error
#-----------------------------------------

prediction_intervals<-data.frame(upper=Elm_Pred+Prediction_Error,val=Elm_Pred,lower=Elm_Pred-Prediction_Error)
(prediction_intervals)



#----------------plotting the PI-----------------------
plot(prediction_intervals$upper, type="l", col="red",ylim=c(0,1),xlab = "hours",ylab="normalised wind speed",main="ELM-INTERVAL")
par(new=TRUE)
plot(prediction_intervals$lower,type="l", col="blue",ylim=c(0,1),xlab = "hours",ylab="normalised wind speed",main="ELM")

