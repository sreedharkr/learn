# projectm_cvnet.R glm, glm+pca, rpart, fcorr, varimp

library(glmnet)
# 70:30 split and cv
#radius_se  4.0912751, radius_worst  0.5657320, texture_worst   0.1211370, smoothness_worst  14.7915504,
# concavity_worst   0.8301832, concave.points_worst   19.5613866, symmetry_worst            4.5592741
proj_cvglm2 <- function(){
  library(glmnet)
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  df <- df[ c(-1) ]
  set.seed(222)
  indices <- sample( 2, nrow(df), replace = T, prob = c(0.7, 0.3)  )
  df.train <- df[ indices == 1, ]  
  df.train1 <- df.train[, 2:31]
  df.train.results <- df.train[,1]
  
  df.test <- df[  indices == 2, ]
  df.test1 <- df.test[, 2:31 ]
  df.test.results <- df.test[,1]
  print( paste( c("number of rows in df.train"), nrow(df.train1) )  )
  print( paste( c("number of rows in df.test"), nrow(df.test1) )  )
  
  # apply regularization and cv
  cvglm <- cv.glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results), family = "binomial",alpha = 1)
  print(names(cvglm))
  #print( summary(cvglm) )
  plot(cvglm)
  #print(coef(glm.wis3)[, 30])
  print(coef(cvglm))
  #0.01 accuracy 1
  results.wis <- predict(object = cvglm, s = 0.01, as.matrix(df.test1),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != df.test.results)
  print(paste('Accuracy', 1 - misClasificError))
  
}

#PCA 
pca.classify <- function(){
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  df <- df[c(-1)]
  set.seed(222)
  indices <- sample(2, nrow(df), replace = T, prob = c(70, 30))
  #train data
  df.train <- df[indices == 1,] 
  df.test <- df[indices == 2,]
  df.train1 <- df.train[,2:31]
  df.train.results <- df.train[,1]
  df.test1 <- df.test[,2:31]
  df.test.results <- df.test[,1]
  
  #apply pca
  pca.df <- prcomp(df.train1, center = T, scale = T)
  pca.df.train <- pca.df$x[,1:6]
  #dataframe for training data
  pca.df1 <- data.frame(diagnosis = df.train.results, pca.df$x)
  #train.data <- data.frame(diagnosis=bcancer3_data_results, pca.bcancer$x)
  # train.data2 <- train.data[1:7]
  train.data2 <- pca.df1[1:7]
  # glm.model <- glm(diagnosis ~ .,family = binomial(link="logit"), data = train.data2, maxit = 100)
  glm.model <- cv.glmnet(x = as.matrix(pca.df.train), y = as.matrix(df.train.results),family = "binomial",alpha = 1)
  print (summary(glm.model))
  print(coef(glm.model))
  #preparing the test data
  test.data <- predict(pca.df, newdata = df.test1)
  test.data <- as.data.frame(test.data)
  test.data2 <- test.data[1:6]
  
  # results.wis <- predict(glm.model,test.data2)
  results.wis <- predict(object = glm.model, s = 0.01, as.matrix(test.data2),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != df.test.results)
  print(paste('Accuracy',1-misClasificError))
}

proj_glm <- function(){
  
  #load the dataset
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  set.seed(222)
  indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3 <- bcancer[indexes,]
  bcancer3 <- bcancer3[c(-1)] #remove serial number
  #training data
  bcancer3_data <- bcancer3[1:500,] 
  # training data without predicted variable
  bcancer3_data2 <- bcancer3_data[,2:31]
  # training data predicted variable
  bcancer3_data2_results <- bcancer3_data[,1]
  # test data
  bcancer3_test <- bcancer3[501:569,2:31]
  bcancer3_test_results <- bcancer3[501:569,1]
  
  # variable selection using lasso
  # alpha=1 lasso
  #glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
  glm.wis3 <- glmnet(x = as.matrix(bcancer3_data2),y = as.matrix(bcancer3_data2_results),family = "binomial",alpha = 1)
  print(names(glm.wis3))
  print( summary(glm.wis3) )
  #plot(glm.wis3)
  print(coef(glm.wis3))
  #print(coef(glm.wis3)[, 10])
  #0.01 accuracy 1
  results.wis <- predict(object=glm.wis3,s= 0.01,as.matrix(bcancer3_test),type = "response")
  #print(results.wis)
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy',1-misClasificError))

}
proj_cvglm <- function(){
  library(glmnet)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  set.seed(222)
  indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3 <- bcancer[indexes,]
  bcancer3 <- bcancer3[c(-1)] #remove serial number
  bcancer3_data <- bcancer3[1:500,] #
  
  bcancer3_data2 <- bcancer3_data[,2:31]
  bcancer3_data2_results <- bcancer3_data[,1]
  
  bcancer3_test <- bcancer3[501:569,2:31]
  bcancer3_test_results <- bcancer3[501:569,1]
  
  #glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
  cvglm <- cv.glmnet(x = as.matrix(bcancer3_data2), y = as.matrix(bcancer3_data2_results),family = "binomial",alpha = 1)
  print(names(cvglm))
  #print( summary(cvglm) )
  plot(cvglm)
  #print(coef(glm.wis3)[, 30])
  print(coef(cvglm))
  #0.01 accuracy 1
  results.wis <- predict(object = cvglm, s = 0.01, as.matrix(bcancer3_test),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy', 1 - misClasificError))
  
}

#PCA 
pca1 <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1,-2)]
  #train data
  bcancer3_data <- bcancer3[1:500,] 
  bcancer3_data_results <- bcancer[1:500,2]
  #test data
  bcancer3_test <- bcancer3[501:569,1:30]
  bcancer3_test_results <- bcancer[501:569,2]
  source('exploratory.R')
  #apply pca
  pca.bcancer <- prcomp(bcancer3_data,center = TRUE,scale. = TRUE)
  #dataframe for training data
  train.data <- data.frame(diagnosis=bcancer3_data_results, pca.bcancer$x)
  train.data2 <- train.data[1:7]
  glm.model <- glm(diagnosis ~ .,family=binomial(link="logit"),data=train.data2,maxit=100)
  #print (summary(glm.model))
  test.data <- predict(pca.bcancer, newdata = bcancer3_test)
  test.data <- as.data.frame(test.data)
  test.data2 <- test.data[1:7]
  
  results.wis <- predict(glm.model,test.data2)
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy',1-misClasificError))
}
#decision tree
pca.tree <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1,-2)]
  #train data
  bcancer3_data <- bcancer3[1:500,] 
  bcancer3_data_results <- bcancer[1:500,2]
  #test data
  bcancer3_test <- bcancer3[501:569,1:30]
  bcancer3_test_results <- bcancer[501:569,2]
  source('exploratory.R')
  #apply pca
  pca.bcancer <- prcomp(bcancer3_data,center = TRUE,scale. = TRUE)
  train.data <- data.frame(diagnosis=bcancer3_data_results, pca.bcancer$x)
  #train.data <- data.frame(diagnosis=bcancer3_data_results, bcancer3_data)
  train.data2 <- train.data[1:7]
  library(rpart)
  rpart.model <- rpart(formula = diagnosis ~ .,data=train.data2)
  summary(rpart.model)
  test.data <- predict(pca.bcancer, newdata = bcancer3_test)
  test.data <- as.data.frame(test.data)
  test.data2 <- test.data[1:7]
  #'arg' should be one of “vector”, “prob”, “class”, “matrix” 
  results.wis <- predict(rpart.model,newdata = test.data2,type="class")
  #results.wis <- predict(rpart.model,newdata = test.data2,type="prob")
  print(results.wis)
  print(dim(results.wis))
  # library(rpart.plot)
  # rpart.plot(results.wis)
  #results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  #print(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy',1-misClasificError))
  library(caret)
  xtab <- table(results.wis, bcancer3_test_results)
  print(xtab)
  result <- confusionMatrix(results.wis, bcancer3_test_results)
  precision <- result$byClass['Pos Pred Value']    
  recall <- result$byClass['Sensitivity']
  print(precision)
  print(recall)
}

tenfold <- function(){
  bcancer <- read.table("datasets/breast-cancer-wisconsin.txt",sep = ",")
  #Randomly shuffle the data
  bcancer<-bcancer[sample(nrow(bcancer)),]
  yourData <- bcancer[1:100,]
  
  #Create 10 equally size folds
  folds <- cut( seq(1,nrow(yourData)),breaks=10,labels=FALSE )
  class(folds)
  #Perform 10 fold cross validation
  for(i in 1:1){
    #Segement your data by fold using the which() function 
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- yourData[testIndexes, ]
    print("testdata >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    print(testData)
    
    trainData <- yourData[-testIndexes, ]
    print("traindata >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    print(trainData)
    #Use the test and train data partitions however you desire...
  }
}#tenfold

fselect <- function() {
  # ensure the results are repeatable
  set.seed(7)
  # load the library
  library(mlbench)
  library(caret)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1,-2)]
  # calculate correlation matrix
  cmatrix <- cor(bcancer3)
  print("correlationMatrix::")
  # find attributes that are highly corrected (ideally >0.75)
  correlated <- findCorrelation(cmatrix, cutoff=0.7)
  print(correlated)
  # library(corrplot)
  # corrplot(cmatrix,order = hclust)
  highlyCorrelated <- findCorrelation(cmatrix, cutoff=0.7,verbose=TRUE,names=TRUE)
  print(highlyCorrelated)
}

fimportance <- function() {
  set.seed(7)
  library(mlbench)
  library(caret)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  model <- train(diagnosis ~., data=bcancer, method="lvq", preProcess="scale", trControl=control)
  #model <- train(diabetes~., data=PimaIndiansDiabetes, method="lm", preProcess="scale", trControl=control)
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  print(importance)
  plot(importance)
}

roc_curve <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  bcancer3_data <- bcancer3[1:500,] 
  bcancer3_test <- bcancer3[501:569,2:31]
  bcancer3_test_results <- bcancer[501:569,2]
  glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
  summary(glm.wis3)
  results.wis <- predict.glm(glm.wis3,bcancer3_test,type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy',1-misClasificError))
  library(ROCR)
  # library(gplots)
  results.pred <- predict.glm(glm.wis3,bcancer3_test,type = "response")
  pred <- prediction(results.pred, bcancer3_test_results)
  # plot(performance(m1.scores, "tpr", "fpr"), col = "red")
  perfspec <- performance(prediction.obj = pred, measure="spec", x.measure="cutoff")
  plot(perfspec)
  par(new=TRUE)
  perfsens <- performance(prediction.obj = pred, measure="sens", x.measure="cutoff")
  plot(perfsens)
}
k_means <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  c2 <- c("perimeter_worst","radius_worst","area_worst","concave.points_worst",
          "concave.points_mean","perimeter_mean")
 # summary(bcancer[,c2])
  bcancer3 <- bcancer[c(-1,-2)][c2]
  bcancer3 <- scale(bcancer3, scale = T, center = T)
  #bcancer3 <- bcancer[c(-1,-2)]
  cancer.cluster <- kmeans(bcancer3, 2, nstart = 1,iter.max = 150)
  #print(cancer.cluster)
  print( table(cancer.cluster$cluster)["2"] )
  print ( table(cancer.cluster$cluster)["1"] )
  cancer.cluster$cluster <- as.factor(cancer.cluster$cluster)
  results.wis <- ifelse(cancer.cluster$cluster == 2,'M','B')
  print( table(results.wis)["M"] )
  print ( table(results.wis)["B"] )
  print( table(results.wis, bcancer$diagnosis) )
  misClasificError <- mean(results.wis != bcancer$diagnosis)
  print(paste('Accuracy',1-misClasificError))
  #ggplot(bcancer3, aes(perimeter_worst, radius_worst, color = cancer.cluster$diagnosis)) + geom_point()
}
# radius_worst concave.points_worst texture_worst
dtree3 <- function() {
  library("rpart")
  library("rpart.plot")
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  bcancer.train <- scale(bcancer3[2:31], center = T, scale = T)
  bcancer3.train <- data.frame(diagnosis = bcancer3[,1], bcancer.train)
  # tree <- rpart(diagnosis ~ ., data = bcancer3, method = "class")
  tree <- rpart(diagnosis ~ ., data = as.data.frame( bcancer3.train), method = "class")
  #summary(tree)
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
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}