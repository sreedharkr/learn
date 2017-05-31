# genetic-ga.R
bcancer <- read.csv("./../class/datasets/breast-cancer-wisconsin-data.csv",sep = ",")
c1 <- c(3:32)
accuracy <- 0.0
solution <- c(3:32)
count1 <<- 0
library(GA)
library(class)
library(glmnet)

 #gaControl("binary" = list(selection = "gabin_tourSelection"))
#gaControl("binary" = list(selection = "ga_rwSelection",crossover = "gabin_uCrossover"))
ga2 <- function() {
  count1 <<- 0
  accuracy <- 0.0
  
  # cross = .9 muta = .25 elit = 3
  #gaControl("binary" = list(selection = "gabin_rwSelection"))
  # GA <- ga(type = "binary", fitness = proj_glm, nBits = 30, popSize = 50,maxiter = 50,
  #        parallel = F, pcrossover = 0.8,pmutation = 0.1, selection = gabin_rwSelection )
  GA <- ga(type = "binary", fitness = proj_glm, nBits = 30, popSize = 50, maxiter = 50,
           parallel = T, pcrossover = 0.9, pmutation = 0.25, elitism = 2,run = 40,
           selection = gabin_tourSelection)
  plot(GA)
  #print(summary(GA))
  print(">>>>>>>>>>>>> GS@solution and dim")
  m1 <- GA@solution
  print(dim(m1))
  print(GA@solution)
  print("best fitness value >>>>>>>>>>>>>")
  print(GA@fitnessValue)
  print("accuracy manual >>>>>>>>>>>>>")
  cat("accuracy \n ",accuracy)
  print("\n")
  cat("solution::::: \n ",solution)
  #print(GA@solution == solution)
  cat("count value is:::",count1)
  if(dim(m1)[1] > 1) {
    glm.verify(m1[1,])
  }
}

proj_glm <- function(x){
  
  indices2 <- x * c1
  #print(indices2)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  #print(dim(bcancer))
  #print(head(bcancer[,1:10]))
  set.seed(222)
  #indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3_data2_results <- bcancer[1:500,2]
  #print(bcancer3_data2_results)
  bcancer3_test_results <- bcancer[501:569,2]
  bcancer3 <- bcancer[,indices2]
  #print(dim(bcancer3))
  bcancer3_data2 <- bcancer3[1:500,]
  #print(dim(bcancer3_data2))
  bcancer3_test <- bcancer3[501:569,]
  #print(dim(bcancer3_test))
  # alpha=1 lasso
  glm.wis3 <- glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family="binomial",alpha=0)
  #0.01 accuracy 1
  # results.wis <- predict(object=glm.wis3,s= 0.01,as.matrix(bcancer3_test),type = "response")
  results.wis <- predict(object=glm.wis3, s= 0.02, as.matrix(bcancer3_data2),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  results.wis <- as.factor(results.wis)
  # misClasificError <- mean(results.wis != bcancer3_test_results)

  # mn <- mean(results.wis == bcancer3_test_results)
  count1 <<- count1 + 1
  mn <- mean(results.wis == bcancer3_data2_results)
    if(accuracy < mn) {
      #Sys.sleep(1)
      accuracy <<- mn
      #solution <<- x
    }else {
    #print(rep("NA",6))
    #return(mn) 
  }
  print(paste('Accuracy',mn))
  return(mn)
}

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

glm.verify <- function(x){
  indices2 <- x * c1
  #print(indices2)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  set.seed(222)
  #indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3_data2_results <- bcancer[1:500,2]
  #print(bcancer3_data2_results)
  bcancer3_test_results <- bcancer[501:569,2]
  bcancer3 <- bcancer[,indices2]
  bcancer3_data2 <- bcancer3[1:500,]
  bcancer3_test <- bcancer3[501:569,]
  # alpha=1 lasso
  glm.wis3 <- glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family = "binomial",alpha = 0)
  #0.01 accuracy 1
  results.wis <- predict(object=glm.wis3,s= 0.02,as.matrix(bcancer3_data2),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  results.wis <- as.factor(results.wis)
  mn <- mean(results.wis == bcancer3_data2_results)
  print(paste('Accuracy',mn))
}
knn2 <- function(x) {
  #ws_train1 <- bcancer[1:565,3:32]
  indices2 <- x * c1
  print(indices2)
  #Sys.sleep(1)
  ws_train <- bcancer[1:565,indices2]
  ws_c_data <- bcancer[1:565,2]
  #ws_test   <- bcancer[566:569,3:32]
  ws_test   <- bcancer[566:569,indices2]
  pred.results <- knn.cv(train = ws_train,cl = ws_c_data,k=10,l=4, prob = FALSE,use.all = TRUE)
  mn <- mean(pred.results == ws_c_data)
  if(!is.na(mn)) {
    if(accuracy < mn) {
      #Sys.sleep(1)
      accuracy <<- mn
      solution <<-x
    }
  }else {
    #print(rep("NA",6))
    return(0) 
  }
}

highdata <- function(){
  # data(package = "spls")
  #names(prostate)
  library("spls")
  data("prostate")
  prs.data <- prostate$x
  prs.class <- prostate$y
  dataset2 <- cbind(prs.data, prs.class)
  dim(dataset2)
}
# detach("package:vegan", unload=TRUE)
highdata2 <- function(){
  # data(package = "spls")
  #names(prostate)
  data("prostate")
  prs.data <- prostate$x
  prs.class <- prostate$y
  dataset2 <- cbind(prs.data, prs.class)
  dim(dataset2)
}
# Matrix x is gene expression data and arrays were normalized, 
# log transformed, and standardized to zero mean and 
# unit variance across genes
rf_cancer <- function() {
  library("spls")
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  data("prostate")
  prs.data <- prostate$x
  prs.class <- as.factor(prostate$y)
  dataset2 <- data.frame(y = prs.class,prs.data)
  bcancer3 <- dataset2
  set.seed(299)
  indices <- sample(2,nrow(bcancer3),replace = T,prob = c(70,30))
  train.data <- bcancer3[indices == 1,]
  test.data <- bcancer3[indices == 2,]
  rf <- randomForest(y ~ ., data = train.data, ntree = 400, 
                     mtry = 2000, proximity=TRUE , cutoff = c(0.5,0.5))
  # , cutoff = c(0.5,0.5)
  #print(attributes(rf))
  #plot(rf)
  #print (importance(rf)) 
  predicted.train <- predict(rf, type="class")
  train.actual <- train.data[,1]
  bias.estimate <- mean(predicted.train != train.actual)
  print(paste('training Accuracy',1 - bias.estimate))
  print(  table(predicted.train, train.actual)   )
  predicted.test = predict(rf,test.data, type = "class")
  test.actual <- test.data[,1]
  misClasificError <- mean(predicted.test != test.actual)
  print(paste('Test data Accuracy',1 - misClasificError))
  print(  table(predicted.test, test.actual)   )
  # print(rf)
  plot(margin(rf, test.actual))
}