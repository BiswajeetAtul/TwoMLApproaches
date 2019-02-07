rm(list = ls())
strt=Sys.time()
require(caret)
require(MASS)
require(quantmod) #for Lag
require(class)
library(FNN)
#Read the dataset into the workspace
train_data=read.csv("/media/lonewolf/Documents/BTECHPROJECT/projectdataset/testing.csv",header=T)

#Normalisation
traindata=(train_data$windspd-min(train_data$windspd))/(max(train_data$windspd)-min(train_data$windspd))

dataset <- data.frame(traindata, x1=Lag(traindata), x2=Lag(traindata,2),x3=Lag(traindata,3),x4=Lag(traindata,4))
names(traindata) <- c("windspd", "x1", "x2","x3","x4")
dataset=dataset[5:nrow(dataset),]

#Spliting the dataset into training and testing dataset
dt=seq(1,round(nrow(dataset)*0.8),1)
traindata<-dataset[dt,]
testdata<-dataset[-dt,]


hidden<-10

x<-2:5
y<-1
N <- nrow(traindata)
X <- unname(data.matrix(traindata[,x]))
Y <- traindata[,y]

# number of input features
D <- ncol(X)
K <- 1
H <-  hidden

#####################################################
# create the weight matrix frim input to hidden
W1 <- 0.01*matrix(runif(D*H), nrow=D, ncol=H)

#b1 <- matrix(rnorm(H), nrow=1, ncol=H)

###########################################################

A=X%*%W1
A=1/(1+exp(-A))

Y=as.matrix(Y)
W2=ginv(A)%*%Y
Y.out=A%*%W2

###########################################################
#Plot
plot(Y,type="l",xlim= c(0,nrow(traindata)),ylim=c(0,1),col="black",lwd=1,xlab="Hours",ylab="Normalised Windspeed",main="ELM")
par(new=TRUE)
plot(Y.out,type="l",xlim= c(0,nrow(traindata)),ylim=c(0,1),lwd=1,col="red",xlab="Hours",ylab="Normalised Windspeed",main="ELM")
legend(500,0.9,legend = c("Target","Actual"),col=c("black","red"),lwd=2,lty=1:1,cex=0.8)


##########################################################
#Error calculation
err=Y-Y.out
err=err*err
mse=mean(err)

##########################################################
#Testing
M <- nrow(testdata)
X.test <- unname(data.matrix(testdata[,x]))
Y.test <- testdata[,y]
Y.test <- as.matrix(Y.test)

Z.out=X.test%*%W1
Z.out=1/(1+exp(-Z.out))

Y.test.out=Z.out%*%W2


##########################################################
#plot
plot(Y.test[1:nrow(Y.test)],type="l",xlim= c(0,M),ylim=c(0,1),col="black",lwd=2,xlab="Time(Hours)",ylab="Normalised Windspeed",main="ELM")
par(new=TRUE)
plot(Y.test.out[2:length(Y.test.out)],type="l",xlim= c(0,M),ylim=c(0,1),lwd=2,col="red",xlab="Time(Hours)",ylab="Normalised Windspeed",main="ELM")
legend(196,1,legend = c("Target","Predicted"),col=c("black","red"),lwd=2,lty=1:1,cex=0.8)


err=Y.test-Y.test.out
err1=err*err
sqrt(mean(err1))


###################################################################
#Nearest Neighbor
d=as.numeric()
s=as.numeric()
testdata1=data.frame(Y.test.out,testdata[1:nrow(testdata),2:5])

knn=get.knnx(testdata1, traindata, k=1, algo="kd_tree")
sort2=sort(knn$nn.index,index.return=T)
sort2$ix[1]
dist=knn$nn.dist[sort2$ix[1]]

#Prediction Interval
Y.t.lower=Y.test.out-dist
Y.t.upper=Y.test.out+dist


###############################################################
#PICP
N=nrow(Y.test)
a=0
for (i in 1:(N-1)) 
{
  if(Y.test[i]>=Y.t.lower[i+1] && Y.test[i]<=Y.t.upper[i+1])
    c=1
  else
    c=0
  
  a=a+c
}
#PICP
picp=a/(N-1)


#NMPIW
a=0
for(i in 1:N-1)
{
  a=a+(Y.t.upper[i+1] - Y.t.lower[i+1])
}
nmpiw=a/(N-1)

###############################################################
#Plot
plot(Y.t.lower[2:length(Y.t.lower)],type="l",xlim= c(0,M),ylim=c(0,1),col="blue",lwd=2,xlab="Hours",ylab="Normalised Windspeed",main="ELM")
par(new=TRUE)
plot(Y.t.upper[2:length(Y.t.lower)],type="l",xlim= c(0,M),ylim=c(0,1),lwd=2,col="blue",xlab="Hours",ylab="Normalised Windspeed",main="ELM")
par(new=TRUE)
plot(Y.test,type="l",xlim= c(0,M),ylim=c(0,1),lwd=2,col="red",xlab="Hours",ylab="Normalised Windspeed",main="ELM")

###############################################################
#denormalisation

datatest1=(Y.test*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)
datatest.l=(Y.t.lower*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)
datatest.u=(Y.t.upper*((max(train_data$windspd)-min(train_data$windspd))))+min(train_data$windspd)

#Plot
plot(datatest.l[2:length(datatest.l)],type="l",xlim= c(0,M),ylim=c(0,40),lwd=2,lty=1,col="green",xlab="Time(Hours)",ylab="Windspeed(Km/h)",main="ELM-Nearest Neighbour")
par(new=TRUE)
plot(datatest.u[2:length(datatest.u)],type="l",xlim= c(0,M),ylim=c(0,40),lwd=2,lty=1,col="blue",xlab="Time(Hours)",ylab="Windspeed(Km/h)")
par(new=TRUE)
plot(datatest1,type="l",xlim= c(0,M),ylim=c(0,40),lwd=2,col="red",xlab="Time(Hours)",ylab="Windspeed(Km/h)")
v=seq(0,271,24)
h=seq(0,60,10)
abline(v=v,lty=3)
legend(196,40,legend = c("Target","ELM-UB","ELM-LB"),col=c("red","blue","green"),lwd=2,lty=1:1,cex=0.8)


end=Sys.time()
end-strt
