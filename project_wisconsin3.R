library(caret)
library(AUC)
wglmnet <- function() {
  library(glmnet)
  library(caret)
  df <- read.csv("breast-cancer-wisconsin-data.csv",sep = ",")
  #df <- read.csv("wdbc.txt",sep = ",")
  ggplot(df, aes(x = diagnosis, y = radius_se)) +
    geom_boxplot(fill = "grey80", colour = "blue") +
    scale_x_discrete() + xlab("diagnosis") +
    ylab("radius")
 # df$diagnosis <- relevel(df$diagnosis, ref = "M")
  df <- df[ c(-1) ]
  set.seed(299)
  cmatrix <- cor(df[,c(-1)])
  indices <- findCorrelation(cmatrix,cutoff = 0.9,names = TRUE, verbose = T)
  df <- df[, -which(names(df) %in% indices   ) ]
  print(dim(df))
  df.scale <- scale(df[,c(-1)])
  df <- data.frame(diagnosis = df[,1],df.scale)
  inTrain <- createDataPartition(df$diagnosis, p = 0.7, list = FALSE)
  df.train <- df[inTrain,]
  df.test <- df[-inTrain,]
  print(table(df.train$diagnosis))
  print(table(df.test$diagnosis))
  df.train1 <- df.train[c(-1)]
  df.train.results <- df.train[,1]
  # test data
  df.test1 <- df.test[c(-1)]
  df.test.results <- df.test[,1]
  f <- as.formula(diagnosis ~ .*.)
  x1 <- model.matrix(f,df.train)
  cvglm <- glmnet::cv.glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results),
                     family = "binomial",alpha = 1,type.measure = "class",
                     nfolds = 10,standardize = FALSE)
  # cvglm <- cv.glmnet(x = x1, y = as.matrix(df.train.results), 
  #                    family = "binomial",alpha = 1,type.measure = "class",
  #                    nfolds = 10,standardize = FALSE)
  #plot(cvglm$glmnet.fit,"norm", label = TRUE)
  #print(cvglm)
 
  cv.glmnet_prediction_v <- predict(object = cvglm, s = cvglm$lambda.min, as.matrix(df.test1),type = "response")
  false_positive_cost <- 5
  false_negative_cost <- 100
  #for 25 100 % sensitivity

result_df <- data.frame(
  threshold = seq(from = 0.00, to = 1.0, by = 0.01),
  expected_cost = rep(0, 101)
)
actual_v <- as.numeric(df.test.results)
i <- 0
for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)){
  i <- i + 1
  prediction_v <- 1 + as.numeric(cv.glmnet_prediction_v >= threshold)
  match_count <- sum(prediction_v == actual_v)
  # B = 1, M = 2 positive is M
  true_positive_count <- sum(
    prediction_v * actual_v == 4
  )
  true_negative_count <- sum(
    prediction_v * actual_v == 1
  )
  # false_positive_count <- sum(prediction_v < actual_v)
  # false_negative_count <- sum(prediction_v > actual_v)
  false_negative_count <- sum(prediction_v < actual_v)
  false_positive_count <- sum(prediction_v > actual_v)
  total_cost <-
    false_positive_cost * false_positive_count +
    false_negative_cost * false_negative_count 
  expected_cost <- total_cost / nrow(df.test1) 
  result_df$expected_cost[i] <- expected_cost
}
print ( result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ] )
r1 <- result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ]
print(r1[1,1])
# threshold expected_cost
# 91 0.9 3.025
#print ( result_df[which(result_df$threshold == 0.65), ] )

predicted <- ifelse(cv.glmnet_prediction_v >= r1[1,1],"M","B")
cf <- confusionMatrix(predicted,df.test.results,positive = "M")
print(cf)
}

# tuneRF(x = train.data[,2:31], y = train.data[,1], mtryStart = 3, ntreeTry = 300, stepFactor = 2)
wrandom3 <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  library(AUC)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  set.seed(299)
  # Need to specify either N or p.
  # bcancer3 <- ovun.sample(diagnosis ~ ., data = bcancer, method = "over",
  #                         seed = 299, N = 714)$data
  print(table(bcancer3$diagnosis))
  bcancer3 <- bcancer[c(-1)]
  df <- bcancer3
  inTrain <- createDataPartition(df$diagnosis, p = 0.7, list = FALSE)
  train.data <- df[inTrain,]
  test.data <- df[-inTrain,]
  #, cutoff = c(0.6,0.4)
  rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, 
                     mtry = 3,type = "class")
  plot(roc(rf$votes[,2],rf$y),main="chose a threshold from")
  #print(attributes(rf))
  plot(rf)
  #print (importance(rf)) 
  test.actual <- test.data[,1]
  #plot(margin(rf, test.actual))
  predicted.test2 = predict( rf, test.data[, c(-1)], type="prob")
  predicted.test <- predicted.test2[,2]
  
  false_positive_cost <- 5
  false_negative_cost <- 30
  #for 25 100 % sensitivity
  
  result_df <- data.frame(
    threshold = seq(from = 0.00, to = 1.0, by = 0.01),
    expected_cost = rep(0, 101)
  )
  actual_v <- as.numeric(test.actual)
  
  i <- 0
  for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)){
    i <- i + 1
    prediction_v <- 1 + as.numeric(predicted.test >= threshold)
    match_count <- sum(prediction_v == actual_v)
    # B = 1, M = 2 positive is M
    true_positive_count <- sum(
      prediction_v * actual_v == 4
    )
    true_negative_count <- sum(
      prediction_v * actual_v == 1
    )
    # false_positive_count <- sum(prediction_v < actual_v)
    # false_negative_count <- sum(prediction_v > actual_v)
    false_negative_count <- sum(prediction_v < actual_v)
    false_positive_count <- sum(prediction_v > actual_v)
    total_cost <-
      false_positive_cost * false_positive_count +
      false_negative_cost * false_negative_count 
    expected_cost <- total_cost / nrow(GermanCredit) 
    result_df$expected_cost[i] <- expected_cost
  }
  print ( result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ] )
  r1 <- result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ]
  print(r1[1,1])
  predicted <- ifelse(predicted.test >= r1[1,1],"M","B")
  cf <- confusionMatrix(predicted,test.actual,positive = "M")
  print(cf)
}
#model=randomForest(x,y,xtest=x,ytest=y,keep.forest=TRUE)
wrandom31 <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  library(caret)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  set.seed(299)
  bcancer3 <- bcancer3[c(-1)]
  
  inTrain <- createDataPartition(df$diagnosis, p = 0.7, list = FALSE)
  train.data <- df[inTrain,]
  test.data <- df[-inTrain,]
  test.actual <- test.data[,1]
  #, cutoff = c(0.6,0.4)
  rf <- randomForest( x = train.data[2:31],y = train.data[,1],
                     xtest = test.data[2:31], ytest = test.actual, 
                     ntree = 300, 
                     mtry = 3,type = "class", keep.forest = FALSE)
  predicted.test2 = predict(rf,test.data[2:31], type="prob")
  predicted.test <- predicted.test2[,2]
  
  false_positive_cost <- 5
  false_negative_cost <- 100
  #for 25 100 % sensitivity
  
  result_df <- data.frame(
    threshold = seq(from = 0.00, to = 1.0, by = 0.01),
    expected_cost = rep(0, 101)
  )
  actual_v <- as.numeric(test.actual)
  
  i <- 0
  for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)){
    i <- i + 1
    prediction_v <- 1 + as.numeric(predicted.test >= threshold)
    match_count <- sum(prediction_v == actual_v)
    # B = 1, M = 2 positive is M
    true_positive_count <- sum(
      prediction_v * actual_v == 4
    )
    true_negative_count <- sum(
      prediction_v * actual_v == 1
    )
    # false_positive_count <- sum(prediction_v < actual_v)
    # false_negative_count <- sum(prediction_v > actual_v)
    false_negative_count <- sum(prediction_v < actual_v)
    false_positive_count <- sum(prediction_v > actual_v)
    total_cost <-
      false_positive_cost * false_positive_count +
      false_negative_cost * false_negative_count 
    expected_cost <- total_cost / nrow(GermanCredit) 
    result_df$expected_cost[i] <- expected_cost
  }
  print ( result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ] )
  r1 <- result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ]
  print(r1[1,1])
  predicted <- ifelse(predicted.test >= r1[1,1],"M","B")
  cf <- confusionMatrix(predicted,df.test.results,positive = "M")
  print(cf)
}

wglm <- function() {
  library(glmnet)
  library(caret)
  df <- read.csv("breast-cancer-wisconsin-data.csv",sep = ",")
  # df$diagnosis <- relevel(df$diagnosis, ref = "M")
  df <- df[ c(-1) ]
  set.seed(299)
  cmatrix <- cor(df[,c(-1,-2)]) 
  indices <- findCorrelation(cmatrix,cutoff = 0.9,names = TRUE)
  df <- df[, -which(names(df) %in% indices   ) ]
  print(dim(df))
  df.scale <- scale(df[,c(-1)])
  df <- data.frame(diagnosis = df[,1],df.scale)
  inTrain <- createDataPartition(df$diagnosis, p = 0.7, list = FALSE)
  df.train <- df[inTrain,]
  df.test <- df[-inTrain,]
  print(table(df.train$diagnosis))
  print(table(df.test$diagnosis))
  df.train1 <- df.train[c(-1)]
  df.train.results <- df.train[,1]
  # test data
  df.test1 <- df.test[c(-1)]
  df.test.results <- df.test[,1]
  # lasso alpha = 1, Ridge alpha = 0
  lambdas <- 10^seq(3, -2, by = -.1)
  cvglm <- cv.glmnet(x = as.matrix(df.train1), y = as.matrix(df.train.results), 
                     family = "binomial",alpha = 0,type.measure = "class",
                     nfolds = 10,standardize = FALSE, lambda = lambdas)
  plot(cvglm$glmnet.fit,"norm", label = TRUE)
  print(cvglm)
  
  cv.glmnet_prediction_v <- predict(object = cvglm, s = cvglm$lambda.min, as.matrix(df.test1),type = "response")
  false_positive_cost <- 5
  false_negative_cost <- 5
  #for 25 100 % sensitivity
  
  result_df <- data.frame(
    threshold = seq(from = 0.00, to = 1.0, by = 0.01),
    expected_cost = rep(0, 101)
  )
  actual_v <- as.numeric(df.test.results)
  i <- 0
  for(threshold in seq(from = 0.00, to = 1.0, by = 0.01)){
    i <- i + 1
    prediction_v <- 1 + as.numeric(cv.glmnet_prediction_v >= threshold)
    match_count <- sum(prediction_v == actual_v)
    # B = 1, M = 2 positive is M
    true_positive_count <- sum(
      prediction_v * actual_v == 4
    )
    true_negative_count <- sum(
      prediction_v * actual_v == 1
    )
    # false_positive_count <- sum(prediction_v < actual_v)
    # false_negative_count <- sum(prediction_v > actual_v)
    false_negative_count <- sum(prediction_v < actual_v)
    false_positive_count <- sum(prediction_v > actual_v)
    total_cost <-
      false_positive_cost * false_positive_count +
      false_negative_cost * false_negative_count 
    expected_cost <- total_cost / nrow(df.test1) 
    result_df$expected_cost[i] <- expected_cost
  }
  print ( result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ] )
  r1 <- result_df[which(result_df$expected_cost == min(result_df$expected_cost)), ]
  print(r1[1,1])
  # threshold expected_cost
  # 91 0.9 3.025
  #print ( result_df[which(result_df$threshold == 0.65), ] )
  
  predicted <- ifelse(cv.glmnet_prediction_v >= r1[1,1],"M","B")
  #cf <- confusionMatrix(predicted,df.test.results,positive = "M")
  #print(cf)
}

wadaboost3 <- function() {
  #library(ada)
  library(caret)
  # library("rpart")
  # library("rpart.plot")
  # library(randomForest)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  set.seed(299)
  inTrain <- createDataPartition(bcancer3$diagnosis, p = 0.7, list = FALSE)
  train.data <- bcancer3[inTrain,]
  test.data <- bcancer3[-inTrain,]
  
  objControl <- trainControl(method='cv', number=10, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)
  objModel <- train(train.data[,2:31], train.data[,1], 
                    method='gbm', 
                    trControl=objControl,  
                    metric = "ROC",
                    preProc = c("center", "scale"))
  
  predicted <- predict(object=objModel, test.data[,2:31], type='raw')
  cf <- confusionMatrix(predicted, test.data$diagnosis,positive = "M")
  print(cf)
}
