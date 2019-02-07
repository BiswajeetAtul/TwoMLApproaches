#rm(list=ls())
strt=Sys.time()
require(caret)
require(quantmod) #for Lag

#Read the dataset into the workspace
train_data=read.csv("/media/lonewolf/Documents/BTECHPROJECT/projectdataset/testing.csv",header=T)

#Normalisation
traindata=(train_data$windspd-min(train_data$windspd))/(max(train_data$windspd)-min(train_data$windspd))

dataset <- data.frame(traindata, x1=Lag(traindata), x2=Lag(traindata,2),x3=Lag(traindata,3),x4=Lag(traindata,4))
names(dataset) <- c("windspd", "x1", "x2","x3","x4")
dataset=dataset[5:nrow(dataset),]

#Spliting the dataset into training and testing dataset
dt=seq(1,round(nrow(dataset)*0.8),1)
traindata<-dataset[dt,]
testdata<-dataset[-dt,]

##################################################################
#Training the NN using NSGA
library(nsga2R)
results <- nsga2R(fn=PICP, varNo=60, objDim=2, lowerBounds=rep(-1,60), upperBounds=rep(1,60),
                  popSize=50, tourSize=2, generations=300, cprob=0.9, XoverDistIdx=20, mprob=0.2,MuDistIdx=3)

#Paerto front
plot(results$objectives,xlim=c(0,1),ylim=c(0,1),xlab="1-PICP",ylab="NMPIW",lwd=2)

################################################################
#choose the best weights
results$objectives
k=results$objectives[,1]-results$objectives[,2]
k=abs(k)
k=cbind(results$objectives,k,seq(1,50,1))
i=sort(k[,3],index.return=T)
i$ix[1]
results$objectives[i$ix[1],]

################################################################
#Testing the NN
weight=results$parameters[i$ix[1],]
w1=matrix(weight[1:(4*10)],nrow = 4,ncol = 10, byrow = T )
w2=matrix(weight[((4*10)+1):length(weight)],nrow = 10,ncol = 2, byrow = T )

X.test <- testdata[,2:5]
Y.test <- testdata[,1]

X.test=as.matrix(X.test)
Z.out=X.test%*%w1
Z.out=1/(1+exp(-Z.out))

output.t=Z.out%*%w2

#plot
plot(output.t[2:nrow(output.t),1],type="l",col="blue",xlim=c(0,nrow(output.t)),ylim=c(0,1),lwd=2,xlab="Hours",ylab="Normalised Windspeed",main="NN")
par(new=T)
plot(output.t[2:nrow(output.t),2],type="l",col="blue",xlim=c(0,nrow(output.t)),ylim=c(0,1),lwd=2,xlab="Hours",ylab="Normalised Windspeed",main="NN")
par(new=T)
plot(Y.test,type="l",col="red",xlim=c(0,nrow(testdata)),ylim=c(0,1),lwd=2,xlab="Hours",ylab="Normalised Windspeed",main="NN")


###############################################################
#denormalisation

datatest1=(Y.test*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)
datatest.l=(output.t[,1]*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)
datatest.u=(output.t[,2]*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)

#Plot
plot(datatest.l[2:length(datatest.l)],type="l",xlim= c(0,length(datatest1)),ylim=c(0,40),lwd=2,col="green",xlab="Time(Hours)",ylab="Windspeed(Km/h)",main="MOGA-NN")
par(new=TRUE)
plot(datatest.u[2:length(datatest.u)],type="l",xlim= c(0,length(datatest1)),ylim=c(0,40),lwd=2,col="blue",xlab="Time(Hours)",ylab="Windspeed(Km/h)",main="MOGA-NN")
par(new=TRUE)
plot(datatest1,type="l",xlim= c(0,length(datatest1)),ylim=c(0,40),lwd=2,col="red",xlab="Time(Hours)",ylab="Windspeed(Km/h)",main="MOGA-NN")
v=seq(0,271,24)
abline(v=v,lty=3)
legend(196,40,legend = c("Target","Lower Bound","Upper Bound"),col=c("red","blue","green"),lwd=2,lty=1:1,cex=0.8)

################################################################
(picp=1-results$objectives[i$ix[1],1])
(nmpiw=results$objectives[i$ix[1],2])
#x=as.ts(train_data,frequency(24))
#acf(x)

end=Sys.time()
end-strt
