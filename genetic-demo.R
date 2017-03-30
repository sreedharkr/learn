# genetic-ga.R
bcancer <- read.csv("./../class/datasets/breast-cancer-wisconsin-data.csv",sep = ",")
#c1 <- c(2:6034)
c1 <- c(1:746)

library(GA)
library(class)
#gaControl("binary" = list(selection = "gabin_tourSelection"))
#gaControl("binary" = list(selection = "ga_rwSelection",crossover = "gabin_uCrossover"))
demo <- function() {
  # cross = .9 muta = .25 elit = 3
  #gaControl("binary" = list(selection = "gabin_rwSelection"))
  # GA <- ga(type = "binary", fitness = proj_glm, nBits = 6033, popSize = 50,maxiter = 50,
  #        parallel = F, pcrossover = 0.8,pmutation = 0.1, selection = gabin_rwSelection )
  GA <- ga(type = "binary", fitness = knn3, nBits = 746, popSize = 100, maxiter = 70,
           parallel = T, pcrossover = 0.5, pmutation = 0.1, elitism = 2,run = 100,
           selection = ga_tourSelection, crossover = "gabin_spCrossover")
  plot(GA)
  #print(summary(GA))
  print(">>>>>>>>>>>>> GS@solution and dim")
  m1 <- GA@solution
  print(dim(m1))
  #print(GA@solution)
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
test <- function() {
  load(file="./genedf.RData")
  print(dim(dt)  )
  print(dt[, ncol(dt) ])
  print(table(dt[, ncol(dt) ]))
}

rf_cancer <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  load(file="./genedf.RData")
  ws_c_data <- dt[, ncol(dt) ]
  num <- -ncol(dt)
  ws_train <- dt[,num ]
  
  df <- data.frame(Mut = ws_c_data, ws_train)
  set.seed(299)
  rf <- randomForest(Mut ~ ., data = df, ntree = 400, 
                     mtry = 300, proximity=TRUE)
  #print(attributes(rf))
  #plot(rf)
  #print (importance(rf)) 
  predicted.train <- predict(rf, type="class")
  bias.estimate <- mean(predicted.train != ws_c_data)
  print(paste('training Accuracy',1 - bias.estimate))
  print(  table(predicted.train, ws_c_data)   )
  plot(margin(rf, test.actual))
}

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}