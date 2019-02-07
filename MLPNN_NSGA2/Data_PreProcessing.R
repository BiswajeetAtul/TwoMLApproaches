library(quantmod) #for Lag

data.1.a<- read.csv("D:/BTECHPROJECT/projectdataset/testing.csv",header=T);
#---------------normalizing the data-----------------
max=max(data.1.a$windspd)
min=min(data.1.a$windspd)
#--------------------normalization-------------------------
data.2.a<-(data.1.a$windspd-min)/(max-min)
normalized_data<-data.frame(x1=Lag(data.2.a,1), x2=Lag(data.2.a,2), x3=Lag(data.2.a,3))

#normalized_data<-normalized_data[-c(1,2,3),]
normalized_data<-normalized_data[4:744,]
(tail(normalized_data))
data.3.a<-data.2.a[4:744]
(data.3.a)
