explore <- function(){
  df.ha <- read.table("dataset-har.txt", header = T, sep = ";")
  #filter numeric data
  ha.num.cols <- sapply(df.ha, is.numeric) 
  which(ha.num.cols)
  df.ha.numeric <- df.ha[, ha.num.cols] 
  
  #select factor columns
  ha.fact.cols  <- sapply(ha,is.factor)
  which(ha.fact.cols)
  df.ha.factors <- ha[, ha.fact.cols]
}


profiling <- function(){
  library(ggplot2)
  library(grid)
  library(gridExtra)
  #ha <- read.csv("./../kaggle-non/wcomputing/dataset-har-PUC-Rio-ugulino.csv", header = T, sep = ";")
  ha <- read.table("dataset-har.txt", header = T, sep = ";")
  
  #ha.class <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3")
  ha.class <- c("x1","y1","z1","x2","y2","z2")
  plots <- list()
  i <- 1
  for (a in ha.class) {
    y_string <- a
    plt <- ggplot(data = ha, aes_string(x = 'class', y = y_string)) + geom_boxplot()
    plots[[i]] <- plt
    i <- i + 1
    #print(plt)
    #Sys.sleep(2)
  }
  multiplot(plotlist = plots, cols = 3)
}

myplot <- function(df,x_string,y_string) {
  ggplot(data = df, aes_string(x = x_string, y = y_string)) + geom_boxplot()
}

multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

haboost <- function() {
  library(adabag)
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  ha <- read.csv("./../kaggle-non/wcomputing/dataset-har-PUC-Rio-ugulino.csv", header = T, sep = ";")
  ha.select <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4","class")
  ha2 <- ha[, ha.select]
  set.seed(299)
  inTrain <- createDataPartition(ha2$class, p = 0.1, list = FALSE)
  train.data <- ha2[inTrain,]
  test.data <- ha2[-inTrain,]
  
  ha.adaboost <- boosting(class~., data=train.data, boos=TRUE, mfinal=5)
  ha.boost.prediction <- predict.boosting(ha.adaboost, newdata = test.data[,-ncol(ha2)])
  cf <- confusionMatrix(ha.boost.prediction, test.data$class)
  print(cf)
}

harandomf <- function() {
  library("rpart")
  library("rpart.plot")
  library(randomForest)
  library(caret)
  ha <- read.csv("./dataset-har.txt", header = T, sep = ";")
  #ha$how_tall_in_meters <- as.numeric(gsub(",", ".", gsub("\\.", "", ha$how_tall_in_meters)))
  ha$how_tall_in_meters  <- as.numeric(gsub(x = ha$how_tall_in_meters, pattern = ",", replacement = "."))
  ha$body_mass_index  <- as.factor(gsub(x = ha$body_mass_index, pattern = ",", replacement = "."))
  
  ha.select <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4",
                 "how_tall_in_meters","weight","class")
  ha2 <- ha[, ha.select]
  ha2$z4 <- as.numeric(ha2$z4)
  set.seed(299)
  inTrain <- createDataPartition(ha2$class, p = 0.7, list = FALSE)
  train.data <- ha2[inTrain,]
  test.data <- ha2[-inTrain,]
  
  # sampsize = c(25000,8000,25000,8000,25000) strata
  rf <- randomForest(class ~ ., data = train.data, ntree = 400, mtry = 3,
                     type = "class", keep.forest = T,replace = T, 
                     strata= train.data$class,
                     maxnodes = 2500 )
  #plot(roc(rf$votes[,2],rf$y),main="chose a threshold from")
  #print(attributes(rf))
  plot(rf)
  print (sort(importance(rf)) )
  test.actual <- test.data[,ncol(test.data)]
  #plot(margin(rf, test.actual))
  predicted.test2 = predict( rf, test.data[, c(-ncol(test.data))], type="class")
  #predicted.test <- predicted.test2[,2]
  
  actual_v <- as.numeric(test.actual)
  cf <- confusionMatrix(predicted.test2,test.actual)
  print(cf)
}
# mtry    OOBError
# 2.OOB     2 0.005338784
# 3.OOB     3 0.005269785
# 5.OOB     5 0.005899400
# 10.OOB   10 0.007977989

knnha <- function(){
  library(caret)
  ha <- read.table("./dataset-har.txt", header = T, sep = ";")
  ha$z4 <- as.numeric(ha$z4) 
  fcol <- c("user","gender","age","how_tall_in_meters","weight","body_mass_index")
  indices <- - which(names(ha) %in% fcol)
  ha2 <- ha[, indices]
  inTrain <- createDataPartition(ha2$class, p = 0.4, list = FALSE)
  train.data <- ha2[inTrain,]
  test.data <- ha2[-inTrain,]
  
  train1.indices <- createDataPartition(train.data$class, p = 0.5, list = FALSE)
  train1.data <- train.data[train1.indices,] 
  test1.data <- train.data[-train1.indices,]
    
  #knnm <- knn3(x = train.data[-c(15)],y = train.data$class,k = 5, test.data[ -c(15)] )
  knnm <- knn3(x = train.data[-c(15)],y = train.data$class,k = 5)
  predicted <- predict(knnm, newdata = test.data[ -c(15)], type = "class" )
  cmatrix <- confusionMatrix(data = predicted, reference = test.data$class)
  print( cmatrix$table )
  
  #ctrl <- trainControl(method="repeatedcv",repeats = 3)
  ctrl <- trainControl(method="repeatedcv", repeats = 1)
  knnFit <- train(class ~ ., data = train.data, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
  print(knnFit)
}

effects <- function(){
  ha <- read.csv("./../kaggle-non/wcomputing/dataset-har-PUC-Rio-ugulino.csv", header = T, sep = ";")
  with(ha,interaction.plot(x1,y1,class))
  
}
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

kmhar <- function(){
  df2 <- read.table("dataset-har.txt", header = T, sep = ";") 
  df2$how_tall_in_meters  <- as.numeric(gsub(x = df2$how_tall_in_meters, pattern = ",", replacement = "."))
  c1 <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4")
  c2 <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4","how_tall_in_meters")
  df3 <- df2[c2]
  print(dim(df3))
  #set.seed(333)
  #km11 <- kmeans(scale(df3,center = T, scale = T), centers = 5,iter.max = 25)
  km11 <- kmeans(df3, centers = 5,iter.max = 50, nstart = 50)
  print(summary(km11))
  df33 <- cbind(df3, class = df2$class, km11$cluster)
  print(dim(df33))
  print(table(df33$class))
  t = table(df33$class, df33$`km11$cluster`)
  print(t)
  #5
  sitting <- df33[df33$class == 'sitting',]
  t1 = table(sitting$class, sitting$`km11$cluster`)
  #print("sitting >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>..")
  #print(t1)
  #3
  standing <- df33[df33$class == 'standing',]
  t2 = table(standing$class, standing$`km11$cluster`)
  #print("standing >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>..")
  #print(t2)
  #1  walking are clustered as standing
  walking <- df33[df33$class == 'walking',]
  t3 = table(walking$class, walking$`km11$cluster`)
  #print("walking >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>..")
  #print(t3)
  sittingdown <- df33[df33$class == 'sittingdown',]
  t4 = table(sittingdown$class, sittingdown$`km11$cluster`)
  #print("sittingdown >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>..")
  #print(t4)
  standingup <- df33[df33$class == 'standingup',]
  t5 = table(standingup$class, standingup$`km11$cluster`)
  #print("standingup >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>..")
  #print(t5)
  
  # calculates the KM centers, obtained also by km$centers
  aggregate(df3, by=list(cluster=km1$cluster), mean)
  table(df2$class, km11$cluster)
  fitted1 <- fitted(km11)
  dim(fitted1)
  head(fitted1)
  fitteddf <- as.data.frame(cbind(fitted1, 'km11$cluster' = as.integer(km11$cluster)) )
  resid.df3 <- df3 - fitted(km11)
  cc <- cbind(km11[c("betweenss", "tot.withinss", "totss")], # the same two columns
        c(ss(fitted1), ss(resid.df3),    ss(df3)))
  print(cc)
  
  
  df33_1 <- df33[df33$`km11$cluster` == 1,]
  ss(df33_1[-c(14,15)])
  sscalc(df33)
  sscalc(fitteddf, type = 2)
}

ss <- function(x) sum(scale(x, scale = FALSE)^2)

sscalc <- function(df33,type = 1){
  for(a in 1:5){
    df <- df33[ df33$`km11$cluster` == a,]
    if(type == 1) {
      print(ss(df[-c(14,15)]))
    }
    if(type == 2) {
      print(ss(df))
    }
  }
}

harBayes <- function() {
  library(e1071)
  library(caret)
  ha <- read.csv("./dataset-har.txt", header = T, sep = ";")
  #ha$how_tall_in_meters <- as.numeric(gsub(",", ".", gsub("\\.", "", ha$how_tall_in_meters)))
  ha$how_tall_in_meters  <- as.numeric(gsub(x = ha$how_tall_in_meters, pattern = ",", replacement = "."))
  ha$body_mass_index  <- as.factor(gsub(x = ha$body_mass_index, pattern = ",", replacement = "."))
  
  ha.select <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4",
                 "how_tall_in_meters","weight","class")
  ha2 <- ha[, ha.select]
  cor.ha2 <- cor(ha2[-c(13:15)])
  # names = T returns names; F returns indices
  indices <- findCorrelation(cor.ha2, cutoff = 0.7, verbose = T, names = F) 
  ha2 <- ha2[-c(indices)]
  ha2$z4 <- as.numeric(ha2$z4)
  set.seed(299)
  inTrain <- createDataPartition(ha2$class, p = 0.7, list = FALSE)
  train.data <- ha2[inTrain,]
  test.data <- ha2[-inTrain,]
  nb <- naiveBayes(x = train.data[c(-15)], y = train.data$class)
  summary(nb)
  test.actual <- test.data[,ncol(test.data)]
  #plot(margin(rf, test.actual))
  predicted.test2 = predict( nb, test.data[, c(-ncol(test.data))], type="class")
  #predicted.test <- predicted.test2[,2]
  
  actual_v <- as.numeric(test.actual)
  cf <- confusionMatrix(predicted.test2,test.actual)
  print(cf)
}