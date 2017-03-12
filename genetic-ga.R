# genetic-ga.R
bcancer <- read.csv("./../class/datasets/breast-cancer-wisconsin-data.csv",sep = ",")
c1 <- c(3:32)
accuracy <- 0.0
solution <- c(3:32)
library(GA)
library(class)


ga2 <- function() {
GA <- ga(type = "binary", fitness = proj_glm, nBits = 30, popSize = 50,maxiter = 20,parallel = T,
         keepBest = T, monitor = monitor.ga)
plot(GA)
# print(summary(GA))
print(summary(GA))
print(summary(GA)$bestSol)
cat("accuracy \n ",accuracy)
print("\n")
c2 <- solution * c1
cat("solution::::: \n ",solution)
indices2 <- x * c1
#cat("solution variables::::: \n ",indices2)
#print(indices2)
#knn_cancer(solution)
# glm.verify(solution)
}

f <- function(x) {
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
  
  #print(length(pred.results))
  print(mn)
}
proj_glm <- function(x){
  library(glmnet)
  indices2 <- x * c1
  print(indices2)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  print(dim(bcancer))
  #print(head(bcancer[,1:10]))
  set.seed(222)
  #indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3_data2_results <- bcancer[1:500,2]
  #print(bcancer3_data2_results)
  bcancer3_test_results <- bcancer[501:569,2]
  bcancer3 <- bcancer[,indices2]
  print(dim(bcancer3))
  bcancer3_data2 <- bcancer3[1:500,]
  print(dim(bcancer3_data2))
  bcancer3_test <- bcancer3[501:569,]
  print(dim(bcancer3_test))
  # alpha=1 lasso
  glm.wis3 <- glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family="binomial",alpha=1)
  #0.01 accuracy 1
  results.wis <- predict(object=glm.wis3,s= 0.01,as.matrix(bcancer3_test),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  results.wis <- as.factor(results.wis)
  # misClasificError <- mean(results.wis != bcancer3_test_results)

  mn <- mean(results.wis == bcancer3_test_results)
  print(paste('Accuracy',mn))
  solution <<- x
  return(mn)
}

knn_cancer <- function(y) {
  x <- y * c1
  cat(" \n indices of knn::::",x)
  ws_train <- bcancer[1:565,x]
  ws_c_data <- bcancer[1:565,2]
  ws_test   <- bcancer[566:569,x]
  pred.results <- knn.cv(train = ws_train,cl = ws_c_data,k=10,l=4, prob = FALSE,use.all = TRUE)
  mn <- mean(pred.results == ws_c_data)
  cat(" \n length of the predicted:::",length(pred.results))
  print(mn)
  
}

glm.verify <- function(x){
  indices2 <- x * c1
  print(indices2)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  print(dim(bcancer))
  #print(head(bcancer[,1:10]))
  set.seed(222)
  #indexes <- sample(1:nrow(bcancer),size = nrow(bcancer)) #random shuffle
  bcancer3_data2_results <- bcancer[1:500,2]
  #print(bcancer3_data2_results)
  bcancer3_test_results <- bcancer[501:569,2]
  bcancer3 <- bcancer[,indices2]
  print(dim(bcancer3))
  bcancer3_data2 <- bcancer3[1:500,]
  print(dim(bcancer3_data2))
  bcancer3_test <- bcancer3[501:569,]
  print(dim(bcancer3_test))
  # alpha=1 lasso
  glm.wis3 <- glmnet(x= as.matrix(bcancer3_data2),y=as.matrix(bcancer3_data2_results),family="binomial",alpha=1)
  #0.01 accuracy 1
  results.wis <- predict(object=glm.wis3,s= 0.01,as.matrix(bcancer3_test),type = "response")
  results.wis <- ifelse(results.wis >= 0.5,'M','B')
  results.wis <- as.factor(results.wis)
  mn <- mean(results.wis == bcancer3_test_results)
  print(paste('Accuracy',mn))
}

monitor.ga <- function(obj) 
{ 
  # curve(f, -10, 10, main = paste("iteration =", obj@iter))
  # points(obj@population, obj@fitness, pch = 20, col = 2)
  # rug(obj@population, col = 2)
  # Sys.sleep(0.2)
  #print(obj$fitnessValue)
  print(obj@solution)
}