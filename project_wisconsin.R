library(caret)
wlogistic <- function(){
  # regularization + cross-validation createDataPartition()
  library(glmnet)
  library(ROSE)
  #y = factor(y, levels=c(0,1), labels=c("No","Yes") 
  #auth$class <- relevel(auth$class, ref = "YES") 
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  #barplot(table(df$diagnosis))
  #
  #df <- ovun.sample(diagnosis ~ ., data = df, method = "over", p = 0.5)$data
  df$diagnosis <- relevel(df$diagnosis, ref = "B")
  df <- df[ c(-1) ]
  set.seed(299)
  indices <- sample( 2, nrow(df), replace = T, prob = c(0.7, 0.3)  )
  # train data
  df.train <- df[ indices == 1, ]  
  df.test <- df[  indices == 2, ]
  
  df.train1 <- df.train[c(-1)]
  df.train.results <- df.train[,1]
  # test data
  df.test1 <- df.test[c(-1)]
  df.test.results <- df.test[,1]
  
  # apply regularization and cv
  grid=10^seq(10,-2,length=100)
  cvglm <- cv.glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results), 
                     family = "binomial",alpha = 1,
                     nfolds = 10,standardize=FALSE)
  #print(names(cvglm))
  print( summary(cvglm) )
  #plot(cvglm)
  plot(cvglm$glmnet.fit,"norm", label = TRUE)
  print(coef(cvglm))
  # glm2 <- glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results), 
  #                 family = "binomial",alpha = 1,
  #                  lambda = cvglm$lambda.min)
 
  predicted <- predict(object = cvglm, s = cvglm$lambda.min, as.matrix(df.test1),type = "class")
  #predicted <- ifelse(predicted >= 0.5,'M','B')
  #print(predicted)
  predicted <- as.factor(predicted)
  cf <- confusionMatrix(predicted,df.test.results,positive = 'M' )
  # names(cf) "positive" "table"    "overall"  "byClass"  "mode"     "dots" 
  #print(names(cf$byClass))
  print(cf$table)

}

wrandom <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  #bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer <- read.csv("wdbc.csv")
  bcancer <- bcancer[, 1:31]
  bcancer3 <- bcancer
  print(table(bcancer$diagnosis))
  bcancer3 <- bcancer[c(-1)]
  set.seed(299)
  indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  train.data <- bcancer3[indices == 1,]
  test.data <- bcancer3[indices == 2,]
  rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, 
                     mtry = 3, proximity=TRUE, cutoff = c(0.6,0.4) )
  #print(attributes(rf))
  plot(rf)
  #print (importance(rf)) 
  predicted.train <- predict(rf, type="class")
  train.actual <- train.data[,1]
  bias.estimate <- mean(predicted.train != train.actual)
  print(paste('training Accuracy',1 - bias.estimate))
  print(  table(predicted.train, train.actual)   )
  predicted.test = predict(rf,test.data[,2:30], type="class")
  test.actual <- test.data[,1]
  cf <- confusionMatrix(predicted.test, test.actual)
  print(cf$table)
  print(paste("Accuracy",cf$overall['Accuracy'], sep = "::"))
  print(paste("Precision",cf$byClass['Precision'], sep = "::"))
  print(paste("Recall",cf$byClass['Recall'], sep = "::"))
  print(paste("Sensitivity",cf$byClass['Sensitivity'], sep = "::"))
  print(paste("Specificity",cf$byClass['Specificity'], sep = "::"))
  # print(rf)
  #plot(margin(rf, test.actual))
}


wbayes <- function() {
  library(ROSE)
  df <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  #head(df)
  df <- df[c(-1)]
  #df.class <- df[,1]
  #df.train <- df[c(-1)]
  set.seed(299)
  
  # df <- ovun.sample(diagnosis ~ ., data = df, method = "over",
  #                         seed = 299, N = 714)$data
  
  cmatrix <- cor(df[,c(-1)])
  indices <- findCorrelation(cmatrix,cutoff = 0.8,names = TRUE)
  #df <- df[, -which(names(df) %in% indices   ) ]
  
  inTrain <- createDataPartition(df$diagnosis, p = 0.7, list = FALSE)
  df.train0 <- df[inTrain,]
  df.test0 <- df[-inTrain,]
  
  #indices <- sample(2, nrow(df), replace = T, prob = c(0.7,0.3))
  #df.train0 <- df[indices == 1,]
  df.train <- df.train0[c(-1)]
  df.class <- df.train0[,1]
  #df.test0 <- df[indices == 2,]
  df.test <- df.test0[c(-1)]
  df.test.class <- df.test0[,1]

  model = train(df.train,df.class,'nb',trControl=trainControl(method='cv',number=10))
  predicted.values <- predict(model$finalModel, df.train)
  table(predicted.values$class, df.class)
  # naive_iris <- NaiveBayes(iris$Species ~ ., data = iris)
  # plot(naive_iris)
  cf_train <- confusionMatrix(as.factor( predicted.values$class), as.factor(df.class) )
  print(cf_train$table)
  # test data
  predicted.test <- predict(model$finalModel, df.test)
  cf <- confusionMatrix(as.factor( predicted.test$class), as.factor(df.test.class),positive = "M")
  print(cf)
}

wadaboost <- function() {
  library(ada)
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  set.seed(299)
  # indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  # train.data <- bcancer3[indices == 1,]
  # test.data <- bcancer3[indices == 2,]
  inTrain <- createDataPartition(bcancer3$diagnosis, p = 0.7, list = FALSE)
  train.data <- bcancer3[inTrain,]
  test.data <- bcancer3[-inTrain,]
  
  Grid <- expand.grid(maxdepth=25,nu=2,iter=100)
  control <- rpart.control(cp = 0.01, maxdepth = 9,xval = 10)
  gen1 <- ada(diagnosis ~ ., data = train.data, test.x = train.data[,2:31], 
              test.y = train.data$diagnosis , nu = 0.1,
              type = "real", control = control, iter = 200)
  #gen1 <- addtest(gen1, train.data[,2:31], train.data$diagnosis)
  #summary(gen1)
  # print(names(gen1))
  cf <- gen1$confusion
  #print(cf)
  # gen2 <- ada(diagnosis ~ ., data = test.data, test.x = test.data[,2:31], 
  #             test.y = test.data$diagnosis , 
  #             type = "gentle", control = control, iter = 70)
  # gen2 <- addtest(gen2, test.data[,2:31], test.data$diagnosis)
  # summary(gen2)
  predicted <- predict(gen1,test.data[,2:31] )
  cf <- confusionMatrix(predicted, test.data$diagnosis)
  print(cf)
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
  #spec sens tpr fpr
  perfspec <- performance(prediction.obj = pred, measure="tpr", x.measure="fpr")
  plot(perfspec)
  abline(a=0, b= 1)
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
  library(caret)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  inTrain <- createDataPartition(bcancer3$diagnosis,p = 0.7, list =FALSE)
  # bcancer.train <- scale(bcancer3[2:31], center = T, scale = T)
  # bcancer3.train <- data.frame(diagnosis = bcancer3[,1], bcancer.train)
  bcancer.train <- bcancer3[inTrain,]
  bcancer.test <- bcancer3[-inTrain,]
  rpart.model <- rpart(diagnosis ~ ., data = as.data.frame( bcancer.train), method = "class")
  #summary(tree)
  rpart.plot(rpart.model)
  predicted <- predict(rpart.model, bcancer.test[,c(-1)], type = "class")
  cm <- confusionMatrix(predicted, bcancer.test[,1],positive = "M")
  print(cm)
}




unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}