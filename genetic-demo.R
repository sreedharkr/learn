# genetic-ga.R
bcancer <- read.csv("./../class/datasets/breast-cancer-wisconsin-data.csv",sep = ",")
#c1 <- c(2:6034)
c1 <- c(1:746)
accuracy <- 0.0
#solution <- c(2:6034)
count1 <<- 0
library(GA)
library(class)
library(glmnet)

 #gaControl("binary" = list(selection = "gabin_tourSelection"))
#gaControl("binary" = list(selection = "ga_rwSelection",crossover = "gabin_uCrossover"))
ga3 <- function() {
  count1 <<- 0
  accuracy <- 0.0
  
  # cross = .9 muta = .25 elit = 3
  #gaControl("binary" = list(selection = "gabin_rwSelection"))
  # GA <- ga(type = "binary", fitness = proj_glm, nBits = 6033, popSize = 50,maxiter = 50,
  #        parallel = F, pcrossover = 0.8,pmutation = 0.1, selection = gabin_rwSelection )
  GA <- ga(type = "binary", fitness = knn3, nBits = 746, popSize = 10, maxiter = 10,
           parallel = T, pcrossover = 0.9, pmutation = 0.1, elitism = 2,run = 100,
           selection = gabin_tourSelection)
  plot(GA)
  #print(summary(GA))
  print(">>>>>>>>>>>>> GS@solution and dim")
  m1 <- GA@solution
  print(dim(m1))
  print(GA@solution)
  print("selected columns")
  if(dim(m1)[1] > 1) {
    cols <- m1[1,]
  }
  else
    cols <- m1
  print(table(cols))
  print("selected columns number")
  b <- table(cols)
  print( b[names(b) == 1] )
  print("best fitness value >>>>>>>>>>>>>")
  print(GA@fitnessValue)
  print("accuracy manual >>>>>>>>>>>>>")
  cat("accuracy \n ",accuracy)
  print("\n")
  #cat("solution::::: \n ",solution)
  #print(GA@solution == solution)
  cat("count value is:::",count1)
  
}

knn4 <- function() {
  load(file="./genedf.RData")
  print(dim(dt)  )
  print(dt[, ncol(dt) ])
  print(table(dt[, ncol(dt) ]))
}
knn3 <- function(x) {
   load(file="./genedf.RData")
   indices2 <- x * c1
   #print(indices2)
   
   indices <- sample(1:nrow(dt), size = nrow(dt))
   df <- dt[indices,]
   ws_c_data <- df[, ncol(df) ]
   num <- -ncol(dt)
   ws_train <- df[,num ]
   
   ws_train <- ws_train[,indices2]
  
   pred.results <- knn.cv(train = ws_train, cl = ws_c_data,k = 5, prob = FALSE, use.all = TRUE)
  # print(pred.results)
   mn <- mean(pred.results == ws_c_data)
   print(mn)
  # #print ( table(pred.results, ws_c_data) )
  return(mn)
}
# 87 accuracy
knn4 <- function() {
  load(file="./genedf.RData")
  #set.seed(222)
  indices <- sample(1:nrow(dt), size = nrow(dt))
  df <- dt[indices,]
  print(dim(df))
  ws_train <- df[,1:ncol(dt)-1]
  ws_c_data <- df[, ncol(df) ]
  pred.results <- knn.cv(train = ws_train, cl = ws_c_data,k = 5, prob = FALSE, use.all = TRUE)
  print(pred.results)
  mn <- mean(pred.results == ws_c_data)
  print(mn)
  print ( table(pred.results, ws_c_data) )
  return(mn)
}

knn2 <- function(x) {
  library("spls")
  data("prostate")
  prs.data <- prostate$x
  prs.class <- prostate$y
  dataset2 <- data.frame(prs.class, prs.data)
  #bcancer <- dataset2
  #indices2 <- x * c1
  #dataset.selected <- bcancer[,indices2]
  dataset.selected <- dataset2
  set.seed(222)
  indices <- sample(1:nrow(dataset.selected), size = nrow(dataset.selected))
  ws_train2 <- dataset.selected[indices,]
  print(dim(ws_train2))
  ws_train <- ws_train2[c(-1)]
  ws_c_data <- ws_train2[,1]
  #ws_test   <- bcancer[566:569,3:32]
  #ws_test   <- bcancer[566:569,indices2]
  #pred.results <- knn.cv(train = ws_train, cl = ws_c_data, k = 10,l = 2, prob = FALSE,use.all = TRUE)
  pred.results <- knn.cv(train = ws_train, cl = ws_c_data,k = 8, prob = FALSE, use.all = TRUE)
  print(pred.results)
  mn <- mean(pred.results == ws_c_data)
  print(mn)
  print ( table(pred.results, ws_c_data) )
  return(mn)
  # if(!is.na(mn)) {
  #   if(accuracy < mn) {
  #     #Sys.sleep(1)
  #     accuracy <<- mn
  #     solution <<-x
  #   }
  # }else {
  #   #print(rep("NA",6))
  #   return(0) 
  # }
}

highdata <- function(){
  # data(package = "spls")
  #names(prostate)
  library("spls")
  data("prostate")
  prs.data <- prostate$x
  prs.class <- prostate$y
  dataset2 <- cbind(prs.class, prs.data)
  dim(dataset2)
}
# detach("package:vegan", unload=TRUE)
#prostate gene expression data
highdata2 <- function(){
  library("spls")
  data("prostate")
  prs.data <- prostate$x
  prs.class <- prostate$y
  dataset2 <- cbind(prs.data, prs.class)
  dim(dataset2)
  f <- spls(prs.data, prs.class, K = 20, eta = 0.7)
  pred <- predict(f)
}

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
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}