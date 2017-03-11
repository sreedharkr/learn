library(mlbench)
library(caret)
DataProfiling <- function(df = null) {
  # performs the basic profiling of the data
  #
  #Args:
  #   dataset: dataset for which the profiling is done
  n = 1;
  dataset <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  dim(dataset)
  dataset <- dataset[c(-1)]
  set.seed(666)
  indices <- sample(2, nrow(dataset), replace=TRUE, prob = c(0.7,0.3) )
  train.data <- dataset[indices == 1,]
  print( paste( c("number of rows train.data",nrow(train.data))))
  print( paste( c("number of rows test.data",nrow(test.data))))
  test.data <- dataset[indices == 2,]
  test.data.class <- test.data[, n]
  test.data <- test.data[c(-1)]
  glm.model <- glm (diagnosis ~ ., family = binomial(link = "logit"), data = train.data, maxit = 100)
  # displays the p-values
  #print( summary(glm.model)$coefficients[,4])
  #print ( summary(glm.wis3)$coefficients[,4] < 0.05 )
  data.coeff <- summary(glm.model)$coefficients[,4]
  data.pval.logical <- (data.coeff < 0.05 )
  count.pval <- length (data.pval.logical[data.pval.logical == FALSE])
  print(paste(c("count of pcal",count.pval)))
  if(count.pval > 0) {
    print("Some variables are statistically insignificant")
    print( names(train.data)[data.pval.logical == FALSE][c(-n)] )
  }
  
  #invisible(readline(prompt="Press [enter] to continue"))
  cat ("Press [Y] for summary or N for no summary")
  input <- readline()
  if ( identical(input, "Y") ) {
    print (summary(glm.model) )
  }
  cat ("Press [Y] for correlation matrix")
  input <- readline()
  if ( identical(input, "Y") ) {
    cmatrix <- cor(train.data[c(-n)]) 
    correlated <- findCorrelation(cmatrix, cutoff=0.7)
    print( paste( cat("these variables have corelation > 0.7 ::::",correlated)  ) )
  }
  invisible(readline(prompt="Press [enter] to see number of rows and columns"))
  no.rows <- nrow(dataset)
  no.cols = ncol(dataset)
  print( paste( c("number of rows",no.rows) ) )
  print( paste( c("number of columns",no.cols)))
  invisible(readline(prompt="Press [enter] to see names of columns"))
   print( paste( c("names of columns",names(dataset))))
  predicted.results <- predict.glm(glm.model, test.data, type = "response")
  predicted.results <- ifelse (predicted.results >= 0.5, 'M', 'B')
  predicted.results <- as.factor(predicted.results)
  #print(predicted.results)
  #print(test.data.class)
  
  misClasificError <- mean(predicted.results != test.data.class)
  print(paste('Accuracy', 1 - misClasificError))
}
n = 1;
dataset <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
DataProfiling()
#PCA 
proj2 <- function(){
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
  train.data2 <- train.data[1:7]
  glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=train.data2,maxit=100)
  summary(glm.wis3)
  test.data <- predict(pca.bcancer, newdata = bcancer3_test)
  test.data <- as.data.frame(test.data)
  test.data2 <- test.data[1:7]
  
  results.wis <- predict(glm.wis3,test.data2)
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  print(results.wis)
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
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  # train the model
  model <- train(diagnosis ~., data=bcancer, method="lvq", preProcess="scale", trControl=control)
  #model <- train(diabetes~., data=PimaIndiansDiabetes, method="lm", preProcess="scale", trControl=control)
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  # summarize importance
  print(importance)
  # plot importance
  plot(importance)
}

dtree2 <- function() {
  library("rpart")
  library("rpart.plot")
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  tree2 <- rpart(diagnosis ~ ., data = bcancer3, method = "class")
  summary(tree2)
  rpart.plot(tree2)
}