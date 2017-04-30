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