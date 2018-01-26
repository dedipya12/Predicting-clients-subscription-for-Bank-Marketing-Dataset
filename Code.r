#install.packages('randomForest')
#install.packages("rpart", dependencies=TRUE)
#install.packages('ipred')
#install.packages('adabag')
#install.packages('ada')
#install.packages('class')
#install.packages('gbm')
#install.packages('ROCR')

#including the packages installed
library(e1071)
library(ROCR)
library(randomForest)
library(rpart)
library(ipred)
library(adabag)
library(caret)
library(ada)
library(gbm)
library(class)
library("neuralnet")

#function to create a model based on the method
create_model <- function(method,train_data,test_data, Class, ClassIndex)
{
     
  if(method == "naive")
  {
    fmla <- as.formula(paste("as.factor(",Class,") ~","." ))
    model <- naiveBayes(fmla, data = train_data)
    
    prediction <- predict(model, 
                          testData)
    err <-  sum((prediction == testData[,ClassIndex])/length(testData[,ClassIndex]))*100
    return(err)
  }
  
  
  if(method == "randomForest")
  {
    cl<-train_data[,ClassIndex]
    set.seed(415)
    rf <- randomForest(as.factor(cl) ~ .,data=train_data,importance=TRUE, ntree=2000)
    predictedClass<-predict(rf,testData)
    err<- (sum(predictedClass==testData[,ClassIndex]))/length(testData[,ClassIndex])*100.0
    return(err)
  }
  
  if(method == "logistic")
  {
    fmla <- as.formula(paste("as.factor(",Class,") ~","." ))
    mylogit <- glm(fmla, data = train_data, family = "binomial")
    anova(mylogit)
    prediction <- predict(mylogit,testData)
    df = data.frame()
    df = ifelse(prediction>=0.5, 1 , 0)
    err <-  sum((df == testData[,ClassIndex])/length(testData[,ClassIndex]))*100
    return(err)
  }
  
  if(method == "bagging")
  {
    
    fmla <- as.formula(paste("as.factor(",Class,") ~","." ))
    bag <- ipred::bagging(fmla, data=train_data, boos = TRUE,mfinal=10,
                          control = rpart.control(cp = 0)) 
    predictedClass<-predict(bag,testData)
    err<- (sum(predictedClass==testData[,ClassIndex]))/length(testData[,ClassIndex])*100.0
    return(err)
    
  }
  
  if(method == "SVM")
  {
    fmla <- as.formula(paste("as.factor(",Class,") ~","." ))
    model <-svm(fmla, data = train_data)
    prediction <- predict(model,testData,type="class")
    err <-  sum((prediction == testData[,ClassIndex])/length(testData[,ClassIndex]))*100
    return(err)
  }
  
  
  if(method == "adaboosting")
  {
    n <- names(train_data)
    fmla <- as.formula(paste(Class," ~", paste(n[!n %in% Class], collapse = " + ")))
    adaboost <- ada(fmla, data = train_data, iter=20, nu=1, type="discrete")
    predictedClass<-predict(adaboost,testData)
    err<- (sum(predictedClass==testData[,ClassIndex]))/length(testData[,ClassIndex])*100.0
    return(err)
  }
  
  if(method == "knn")
  { 
    model.knn <- knn(train_data[,1:(ClassIndex-1)], testData[,1:(ClassIndex-1),drop=FALSE],train_data[,ClassIndex])
    cluster <- table("Predictions" = model.knn, Actual = testData[,ClassIndex])
    err <- (sum(diag(cluster)) / sum(cluster))*100.0
    return(err)
  }
  
}

#importing dataset
dataURL <- c("D:/2 semester/Machine learning/Project/Final_Project/PreProcessed_Dataset.csv")
x=1
for(p in dataURL)
{
  print(paste("-------------------------------dataset-", x ,"----------------------------------"))
  d<-read.csv(p,header = T)
  print(paste("no of instances",nrow(d)))
  print(paste("no of attributes",ncol(d)-1))
  Class <- colnames(d)[length(d)]
  ClassIndex <- length(d)
  n=nrow(d)
  # 10 fold Cross Valdation
  k_fold=10 
  sum = 0
  id <- sample(1:k_fold,nrow(d),replace=TRUE)
  list <- 1:k_fold
  #Array of methods
  A=c("knn","naive","adaboosting","bagging","randomForest","SVM","logistic")
  for(a in A)
  {
	print(paste("Building Model using ", a))
    for (i in 1:k_fold) 
    {
      train_data <- subset(d, id %in% list[-i])
      test_data <- subset(d, id %in% c(i))
      err <- create_model(a, train_data = train_data,test_data = train_data, Class, ClassIndex)
      sum = sum + err
    }
    acc <- sum/k_fold #Accuracy
    print(paste("Accuracy : ", acc))
    sum = 0
  }
  x=x+1
}