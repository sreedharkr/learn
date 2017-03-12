# rpart_iris.R
dtree <- function() {
library("rpart")
library("rpart.plot")
data("iris")

tree <- rpart(Species ~ ., data = iris, method = "class")
#summary(tree)
#rpart.plot(tree)
rpart.plot(tree, uniform=TRUE, main="Classification Tree for Iris")
#text(tree, use.n=TRUE, all=TRUE, cex=.8)
printcp(tree)
plotcp(tree)

}
dtree3 <- function() {
  library("rpart")
  library("rpart.plot")
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  tree <- rpart(diagnosis ~ ., data = bcancer3, method = "class")
  summary(tree)
  rpart.plot(tree)
}
# incomplete
rf_cancer <- function() {
  library("rpart")
  library("rpart.plot")
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  train.data <-
  #tree <- rpart(diagnosis ~ ., data = bcancer3, method = "class")
  tree <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
  summary(tree)
  rpart.plot(tree)
}
rforest <- function(){
  ind <- sample(2,nrow(iris),replace=TRUE,prob=c(0.7,0.3))
  trainData <- iris[ind==1,]
  testData <- iris[ind==2,]
  library(randomForest)
  iris_rf <- randomForest(Species~.,data=trainData,ntree=100,proximity=TRUE)
  table(predict(iris_rf),trainData$Species)
  print(iris_rf)
  plot(iris_rf)
  importance(iris_rf)
  varImpPlot(iris_rf)
  irisPred<-predict(iris_rf,newdata=testData)
  table(irisPred, testData$Species)
  plot(margin(iris_rf,testData$Species))
  tune.rf <- tuneRF(iris[,-5],iris[,5], stepFactor=0.5)
  print(tune.rf)
}