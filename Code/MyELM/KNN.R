library(class)

(cl <- kmeans(traindata, 50))

knn1=knn(traindata, testdata, cl$cluster, k = 5)
knn1
#########################################################################

data(BloodBrain)

inTrain <- createDataPartition(logBBB, p = .8)[[1]]

trainX <- bbbDescr[inTrain,]
trainY <- logBBB[inTrain]

testX <- bbbDescr[-inTrain,]
testY <- logBBB[-inTrain]

fit <- knnreg(trainX, trainY, k = 5)

plot(testY, predict(fit, testX)) 

