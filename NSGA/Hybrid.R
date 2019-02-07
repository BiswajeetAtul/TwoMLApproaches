require(caret)
require(quantmod) #for Lag

#Read the dataset into the workspace
train_data=read.csv("C:/Users/USER.000/Desktop/B Tech project/new_testdata.csv",header=T)

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
plot(results$objectives,ylim=c(0,1),xlab="1-PICP",ylab="NMPIW",main="Pareto Front")
#plot(results$objectives)

################################################################
#choose the best weights
results$objectives
k=results$objectives[,1]-results$objectives[,2]
k=abs(k)
k=cbind(results$objectives,k,seq(1,50,1))
i=sort(k[,3],index.return=T)
i$ix[1]

################################################################
#verification
weight=results$parameters[1]
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
plot(datatest.l[2:length(datatest.l)],type="l",xlim= c(0,length(datatest1)),ylim=c(0,max(datatest.u)),lwd=2,col="blue",xlab="Hours",ylab="Windspeed",main="MOGA-NN")
par(new=TRUE)
plot(datatest.u[2:length(datatest.u)],type="l",xlim= c(0,length(datatest1)),ylim=c(0,max(datatest.u)),lwd=2,col="blue",xlab="Hours",ylab="Windspeed",main="MOGA-NN")
par(new=TRUE)
plot(datatest1,type="l",xlim= c(0,length(datatest1)),ylim=c(0,max(datatest.u)),lwd=2,col="red",xlab="Hours",ylab="Windspeed",main="MOGA-NN")
legend(200,37,legend = c("Target","Lower Bound","Upper Bound"),col=c("red","blue","blue"),lwd=2,lty=1:1,cex=0.8)

######################################################################
#########################finding optimal using FNDS:
#Optimalchromosome<-as.matrix(results$objectives)
#rankedOptimalChromosomes<-fastNonDominatedSorting(Optimalchromosome)
