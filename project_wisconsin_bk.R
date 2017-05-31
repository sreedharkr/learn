# projectm_cvnet.R glm, glm+pca, rpart, fcorr, varimp
source("utility.R")
library(glmnet)
# 70:30 split and cv
#radius_se  4.0912751, radius_worst  0.5657320, texture_worst   0.1211370, smoothness_worst  14.7915504,
# concavity_worst   0.8301832, concave.points_worst   19.5613866, symmetry_worst            4.5592741
logistic <- function(){
  # regularization + cross-validation
  library(glmnet)
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  df <- df[ c(-1) ]
  set.seed(222)
  indices <- sample( 2, nrow(df), replace = T, prob = c(0.7, 0.3)  )
  df.train <- df[ indices == 1, ]  
  df.train1 <- df.train[, 2:31]
  #display.head(df.train1)
  #display.pairs(df.train1)
  df.train.results <- df.train[,1]
  
  df.test <- df[  indices == 2, ]
  df.test1 <- df.test[, 2:31 ]
  df.test.results <- df.test[,1]
  print( paste( c("number of rows in df.train"), nrow(df.train1) )  )
  print( paste( c("number of rows in df.test"), nrow(df.test1) )  )
  
  # apply regularization and cv
  cvglm <- cv.glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results), family = "binomial",alpha = 1)
  #print(names(cvglm))
  #print( summary(cvglm) )
  #plot(cvglm)
  #print(coef(glm.wis3)[, 30])
  #print(coef(cvglm))
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
  # pca + cv.glmnet
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
  # glmnet only - no regularization
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

#PCA 

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

rf_cancer <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  set.seed(299)
  indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  train.data <- bcancer3[indices == 1,]
  test.data <- bcancer3[indices == 2,]
  rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, 
                     mtry = 3, proximity=TRUE, cutoff = c(0.6,0.4) )
  print(attributes(rf))
  plot(rf)
  #print (importance(rf)) 
  predicted.train <- predict(rf, type="class")
  train.actual <- train.data[,1]
  bias.estimate <- mean(predicted.train != train.actual)
  print(paste('training Accuracy',1 - bias.estimate))
  print(  table(predicted.train, train.actual)   )
  predicted.test = predict(rf,test.data[2:31], type="class")
  test.actual <- test.data[,1]
  misClasificError <- mean(predicted.test != test.actual)
  print(paste('Test data Accuracy',1 - misClasificError))
  print(  table(predicted.test, test.actual)   )
  # print(rf)
  plot(margin(rf, test.actual))
}
wis_adaboost <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  set.seed(299)
  indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  train.data <- bcancer3[indices == 1,]
  test.data <- bcancer3[indices == 2,]
  control <- rpart.control(cp = -1, maxdepth = 14,maxcompete = 1,xval = 0)
  gen1 <- ada(diagnosis ~ ., data = train.data, test.x = train.data[,2:31], 
              test.y = train.data$diagnosis , 
              type = "gentle", control = control, iter = 70)
  gen1 <- addtest(gen1, train.data[,2:31], train.data$diagnosis)
  summary(gen1)
  gen2 <- ada(diagnosis ~ ., data = test.data, test.x = test.data[,2:31], 
              test.y = test.data$diagnosis , 
              type = "gentle", control = control, iter = 70)
  gen2 <- addtest(gen2, test.data[,2:31], test.data$diagnosis)
  summary(gen2)
  
}

bayes_proj <- function() {
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  #head(df)
  df <- df[c(-1)]
  #df.class <- df[,1]
  #df.train <- df[c(-1)]
  #set.seed(299)
  indices <- sample(2, nrow(df), replace = T, prob = c(0.7,0.3))
  df.train0 <- df[indices == 1,]
  df.train <- df.train0[c(-1)]
  df.class <- df.train0[,1]
  df.test0 <- df[indices == 2,]
  df.test <- df.test0[c(-1)]
  df.test.class <- df.test0[,1]
  print(dim(df.train))
  print(dim(df.test))
  model = train(df.train,df.class,'nb',trControl=trainControl(method='cv',number=10))
  predicted.values <- predict(model$finalModel, df.train)
  table(predicted.values$class, df.class)
  # naive_iris <- NaiveBayes(iris$Species ~ ., data = iris)
  # plot(naive_iris)
  result <- confusionMatrix(as.factor( predicted.values$class), as.factor(df.class) )
  precision <- result$byClass['Pos Pred Value']    
  recall <- result$byClass['Sensitivity']
  print(precision)
  print(recall)
  # test data
  predicted.test <- predict(model$finalModel, df.test)
  print( table(predicted.test$class, df.test.class) )
  result2 <- confusionMatrix(as.factor( predicted.test$class), as.factor(df.test.class))
  precision <- result2$byClass['Pos Pred Value']    
  recall <- result2$byClass['Sensitivity']
  print(precision)
  print(recall)
}
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}