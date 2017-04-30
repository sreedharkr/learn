# genetic-ga.R
c1 <- c(1:746)
library(GA)
library(caret)
library(class)

# gabin_tourSelection, ga_rwSelection, 
# gabin_uCrossover, gabin_spCrossover
demo <- function() {
 
  # GA <- ga(type = "binary", fitness = knnga, nBits = 746, popSize = 100, maxiter = 10,
  #          parallel = T, pcrossover = 0.90, pmutation = 0.008, elitism = 2,run = 200,
  #          selection = ga_rwSelection, crossover = "gabin_uCrossover", keepBest = F)
  
  GA <- ga(type = "binary", fitness = knnga, nBits = 746, popSize = 10, maxiter = 10,
           parallel = F, pcrossover = 0.9, pmutation = 0.1, elitism = 2,run = 100,
           selection = gabin_tourSelection)
  plot(GA)
  #print(summary(GA))
  print(GA)
  print(">>>>>>>>>>>>> GS@solution and dim")
  gmat <- GA@solution
  cols <- gmat[1,1:746]
  #tt <- as.numeric(unlist(m1[1]))
  print(table(cols))
  print("selected columns number")
  b <- table(cols)
  print( b[names(b) == 1] )
  print("best fitness value >>>>>>>>>>>>>")
  print(GA@fitnessValue)
  knn5(cols)
}


knnga <- function(x) {
  # print("entered")
    load(file="./genedf.RData")
    indices2 <- x * c1
    df <- dt
    ws_c_data <- df[, ncol(df) ]
    num <- -ncol(dt)
    ws_train <- df[,num ]
    ws_train <- ws_train[,indices2]
    pred.results <- knn.cv(train = ws_train, cl = ws_c_data,k = 5, prob = FALSE, use.all = TRUE)
    #pred.results <- knn(train = ws_train, cl = ws_c_data,test = ws_train,k = 5, prob = FALSE, use.all = TRUE)
    cm <- confusionMatrix(pred.results, ws_c_data)
    f1 <- cm$byClass[4,11]
  # print(f1)
   return(f1)
}
knn5 <- function(x) {
  load(file="./genedf.RData")
  indices2 <- x * c1
  df <- dt
  print(table(df$Mut))
  ws_c_data <- df[, ncol(df) ]
  num <- -ncol(dt)
  ws_train <- df[,num ]
  ws_train <- ws_train[,indices2]
  pred.results <- knn(train = ws_train, cl = ws_c_data,test = ws_train,k = 5, prob = FALSE, use.all = TRUE)
  #pred.results <- knn.cv(train = ws_train, cl = ws_c_data,k = 5, prob = FALSE, use.all = TRUE)
  # print(pred.results)
  library(caret)
  cf <- confusionMatrix(pred.results,ws_c_data)
  print(cf)
}
# 87 accuracy
knn4 <- function() {
  load(file="./genedf.RData")
  #set.seed(222)
  indices <- sample(1:nrow(dt), size = nrow(dt))
  df <- dt[indices,]
  print(dim(df))
  ws_train <- df[,1:ncol(dt)-1]
  #ws_train <- scale(ws_train, center = T, scale = T)
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
  cm <- confusionMatrix(predicted.train,ws_c_data)
  cm$byClass
  plot(margin(rf, test.actual))
}

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# "binary"
# – population = "gabin_Population"
# – selection = "gabin_lrSelection"
# – crossover = "gabin_spCrossover"
# – mutation = "gabin_raMutation"