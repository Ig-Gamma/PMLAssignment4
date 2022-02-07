#r script file for PML week 4 assignment, Igor Gamayun
#loading libraries
library('knitr')
library('caret')


# getting data into variables
TrainData<-read.csv("pml-training.csv",sep = ',')
TestData<-read.csv("pml-testing.csv",sep = ',')
# working on training data

data<-data.frame(TrainData)
str(TrainData)
colMeans(is.na(TrainData))
# choose columns without na
data1<-data[,colSums(is.na(data))==0]
# choose columns without empty cells
data1<-data1[,colSums(data1=='')==0]

# make character variable "name" as factor variable
data1$classe=as.factor(data1$classe)

#extra variable we can compare two conditions: with the name as a variable and without
data1$user_name=as.factor(data1$user_name)
dataTrain1=data1[,-c(1:7)];

dataTrain1[,-53]=sapply(dataTrain1[,-53],as.numeric)
#dataTrain1[,-c(1,54)]=sapply(dataTrain1[,-c(1,54)],as.numeric)

#create data partition
index<-createDataPartition(dataTrain1$classe,p=0.75, list=FALSE)
dataTrain=dataTrain1[index,];
dataTrainValidation=dataTrain1[-index,];

#training control parameters
tc=trainControl(method='repeatedcv', number = 2)




#training with different models

modfSVM<-train(classe~., data = dataTrain, method='svmLinear', trControl=tc, preProcess=c('center','scale'))
modfSVM
modfKNN<-train(classe~., data = dataTrain, method='knn', trControl=tc, preProcess=c('center','scale'))
modfKNN
modfRF<-train(classe~., data = dataTrain, method='rf', trControl=tc, preProcess=c('center','scale'))
modfRF
modfRPART<-train(classe~., data = dataTrain, method='rpart', trControl=tc, preProcess=c('center','scale'),tuneGrid=data.frame(cp=0.0001))
modfRPART
modfGBM<-train(classe~., data = dataTrain, method='gbm', trControl=tc, preProcess=c('center','scale'))
modfGBM

# comparing accuracy of each model
confusionMatrix(dataTrainValidation$classe,predict(modfRF,dataTrainValidation[,-53]))
confusionMatrix(dataTrainValidation$classe,predict(modfGBM,dataTrainValidation[,-53]))
confusionMatrix(dataTrainValidation$classe,predict(modfRPART,dataTrainValidation[,-53]))
confusionMatrix(dataTrainValidation$classe,predict(modfKNN,dataTrainValidation[,-53]))
confusionMatrix(dataTrainValidation$classe,predict(modfSVM,dataTrainValidation[,-53]))

#prediction on the test Data, just to see if some model fails 
predict(modfSVM,TestData)
predict(modfRF,TestData)
predict(modfRPART,TestData)

