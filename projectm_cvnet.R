# projectm_cvnet.R glm, glm+pca, rpart, fcorr, varimp
# 30 attributes
proj_glm <- function(){
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
source('exploratory.R')
# alpha=1 lasso
#glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
glm.wis3 <- glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family="binomial",alpha=0)
print(names(glm.wis3))
plot(glm.wis3)
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
  source('exploratory.R')
  
  #glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
  cvglm <- cv.glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family="binomial",alpha=1)
  print(names(cvglm))
  plot(cvglm)
  #print(coef(glm.wis3)[, 30])
  #print(coef(glm.wis3)[, 10])
  #0.01 accuracy 1
  results.wis <- predict(object=cvglm,s= 0.01,as.matrix(bcancer3_test),type = "response")
  #print(results.wis)
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  #print(results.wis)
  results.wis <- as.factor(results.wis)
  misClasificError <- mean(results.wis != bcancer3_test_results)
  print(paste('Accuracy',1-misClasificError))
  
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
proj3 <- function(){
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
basicdata <- function() {
  bcancer <- read.table("datasets/breast-cancer-wisconsin.txt",sep = ",")
  # colnames(bcancer) <- c("Sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitoses","Class")
  colnames(bcancer) <- c("code","Thickness","Cell Size","Cell Shape","Marginal Adhesion","SE Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitoses","Class")
  #print(paste0("number of rows: ", bcancer))
  cat("number of rows",nrow(bcancer),"\n")
  sum(bcancer$Class)
  str(bcancer)
  # breaks = seq(1.5, 5.5, by=0.5)
  # t1 <- cut(bcancer$Thickness,breaks,right = FALSE)
  # table(t1)
  print ( cor(bcancer[c(-1,-7,-11) ]) )
  bcancer2 <- bcancer[c(-1,-7,-11) ]
  pairs(bcancer2)
  d <- density(bcancer2$Thickness)
  #ggplot(bcancer) + geom_density(aes(x='Cell Size')) + scale_x_continuous(labels='Cell Size')
  plot(d)
  for(i in names(bcancer)){
    #mcol <- cat("bcancer",i,sep = "$")
    #print(class(mcol))
    #print(bcancer[i])
    #print( is.numeric(bcancer[[i]]) )
    #df[[paste(i, 'length', sep="_")]] <- str_length(df[[i]])
    if( is.numeric(bcancer[[i]]) ) {
    t1 <- cut(bcancer[[i]],breaks,right = FALSE)
    #cat("distribution of column",i,t1)
    }
  }#for
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
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}