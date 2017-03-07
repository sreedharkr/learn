bcancer <- read.csv("./../class/datasets/breast-cancer-wisconsin-data.csv",sep = ",")
weightlimit <- 20
c1 <- c(3:32)
accuracy <- 0.0
solution <- c(3:32)
library(GA)
library(class)
f2 <- function(x)  {
  print(x)
}
ga2 <- function() {
min <- -10; max <- 10
GA <- ga(type = "binary", fitness = f, nBits = 30, popSize = 50,maxiter = 20, monitor = FALSE)
summary(GA)
cat("accuracy \n ",accuracy)
print("\n")
c2 <- solution * c1
cat("solution::::: \n ",solution)
indices2 <- x * c1
#cat("solution variables::::: \n ",indices2)
#print(indices2)
knn_cancer(solution)
}

f <- function(x) {
  #ws_train1 <- bcancer[1:565,3:32]
  indices2 <- x * c1
  print(indices2)
  #Sys.sleep(1)
  ws_train <- bcancer[1:565,indices2]
  ws_c_data <- bcancer[1:565,2]
  ws_test   <- bcancer[566:569,3:32]
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