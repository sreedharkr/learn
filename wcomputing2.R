# setwd("/Users/sreedhar/Documents/working/R lang/class")
profiling <- function(){
  library(ggplot2)
  library(grid)
  library(gridExtra)
  ha <- read.csv("./../kaggle-non/wcomputing/dataset-har-PUC-Rio-ugulino.csv", header = T, sep = ";")
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
  ha <- read.csv("./../kaggle-non/wcomputing/dataset-har-PUC-Rio-ugulino.csv", header = T, sep = ";")
  ha$how_tall_in_meters <- as.numeric(gsub(",", ".", gsub("\\.", "", ha$how_tall_in_meters)))
  ha.select <- c("x1","y1","z1","x2","y2","z2","x3","y3","z3","x4","y4","z4","how_tall_in_meters","class")
  ha2 <- ha[, ha.select]
  ha2$z4 <- as.numeric(ha2$z4)
  set.seed(299)
  inTrain <- createDataPartition(ha2$class, p = 0.7, list = FALSE)
  train.data <- ha2[inTrain,]
  test.data <- ha2[-inTrain,]
  
  rf <- randomForest(class ~ ., data = train.data, ntree = 400, 
                     mtry = 3,type = "class")
  #plot(roc(rf$votes[,2],rf$y),main="chose a threshold from")
  #print(attributes(rf))
  plot(rf)
  #print (importance(rf)) 
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