# biocLite("hgu95av2.db")
# genetic-gahigh.R
# detach("package:Biobase", unload=TRUE)
# library(help="Biobase")
micro <- function() {
 library(Biobase)
 library(ALL)
 data(ALL)
 dim(exprs(ALL))
 tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
 ALLb <- ALL[,tgt.cases]
 ALLb$BT <- factor(ALLb$BT)
 ALLb$mol.bio <- factor(ALLb$mol.bio)
 # save(ALLb, file = "myALL.Rdata")
 es <- exprs(ALLb)
 print( dim(es) )
 # source("http://bioconductor.org/biocLite.R") 
 # biocLite("genefilter")
 library(genefilter)
 #explore dataset
 hist(as.vector(es),breaks=80,prob=T,
     xlab='Expression Levels',
     main='Histogram of Overall Expression Levels')
 abline(v=c(median(as.vector(es)),
           shorth(as.vector(es)),
           quantile(as.vector(es),c(0.25,0.75))),
       lty=2,col=c(2,3,4,4))
 legend('topright',c('Median','Shorth','1stQ','3rdQ'),
       lty=2,col=c(2,3,4,4))

 sapply(levels(ALLb$mol.bio),function(x) summary(as.vector(es[,which(ALLb$mol.bio == x)])))
 
 plot(rowMedians(es),rowIQRs(es),
      xlab='Median expression level',
      ylab='IQR expression level',
      main='Main Characteristics of Genes Expression Levels')
 library(genefilter)
 # filtering based on IQR
 ALLb <- nsFilter(ALLb,
                  var.func=IQR,
                  var.cutoff=IQR(as.vector(es))/5, 
                  feature.exclude="^AFFX")
  ALLb <- ALLb$eset
  es <- exprs(ALLb)
  print("dim after nsFilter")
  print(dim(es))
 #Anova filtering data
 f <- Anova(ALLb$mol.bio, p=0.01)
 ff <- filterfun(f)
 selGenes <- genefilter(exprs(ALLb),ff)
 print("sum sel of genes")
 print (sum(selGenes))
 
 ALLb <- ALLb[selGenes,]
 es <- exprs(ALLb)
 print("dim after Anova")
 print(dim(es))
 plot(rowMedians(es),rowIQRs(es),
      xlab='Median expression level',
      ylab='IQR expression level',
      main='Distribution Properties of the Selected Genes')
 
 
 featureNames(ALLb) <- make.names(featureNames(ALLb))
 es <- exprs(ALLb)
 Mut = ALLb$mol.bio

 library(randomForest)
 dt <- data.frame( t(es), Mut = ALLb$mol.bio )
 print(dim(dt))
 # load(file="./genedf.RData")
 #save(dt, file = "genedf.Rdata")
 #print(head(dt[1,]))
  rf <- randomForest(Mut ~  ., dt, importance=T )
 imp <- importance(rf)
 imp <- imp[,ncol(imp)-1]
 rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]
 print(rf.genes)
}
rowIQRs <- function(em) 
  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))

# library(genefilter)
# detach_package("vegan", TRUE)

rf <- function(){
  library(caret)
  load(file="./genedf.RData")
  print(table(dt$Mut))
  # prop.table(table(dt$Mut))
  # rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, mtry = 3, proximity=TRUE, cutoff = c(0.6,0.4) )
  v1 <- as.vector(prop.table(table(dt$Mut)))
  rf <- randomForest(Mut ~  ., dt, importance=T,ntree = 500 , mtry = 45,cutoff = v1)
                     
  predicted.train <- predict(rf, type="class")
  train.actual <- dt[, ncol(dt)]
  cm <- confusionMatrix(predicted.train, train.actual)
  print(cm$table)
  
}
rf2 <- function(){
  library(caret)
  library(mlbench)
  library(mlr)
  load(file="./genedf.RData")
  print(table(dt$Mut))
  inTrain <- createDataPartition(dt$Mut, p = 0.7, list = FALSE)
  df = dt
  df.train <- df[inTrain,]
  df.test <- df[-inTrain,]
  df.test1 <- df.test[c(-ncol(df.test))]
  df.test.results <- df.test[,ncol(df.test)]
  #classif.randomForest
 
  gn.task = makeClassifTask(id = "genes", data = df.train, target = "Mut")
  costs = matrix(c(0, 5, 10, 30, 0, 8, 80, 4, 0), 3)
  wf.costs = makeCostMeasure(id = "gene.costs", name = "test.costs", costs = costs,
                             best = 0, worst = 10)
  #getParamSet("classif.randomForest")
  rf.lrn = makeLearner("classif.randomForest", predict.type = "response")
  #classif.lrn = setPredictType(rf.lrn, "response")
  mod = train(rf.lrn, gn.task)
  pred = predict(mod, newdata = df.test1)
  cm <- confusionMatrix(pred$data$response,df.test.results)
  print(table(df.test.results))
  print(cm$table)
}
bayes <- function(){
  load(file="./genedf.RData")
  train.actual <- dt[, ncol(dt)]
  df.train <- dt[,1:ncol(dt)-1]
  print(dim(dt))
  library(caret)
  train_control <- trainControl(method="cv")
  # train the model
  model <- train(Mut ~., data = dt, trControl=train_control, method="nb")
  predicted.train <- predict(model$finalModel,df.train)
  bias.estimate <- mean(predicted.train != train.actual)
  print(paste('training Accuracy',1 - bias.estimate))
  #print(  table(predicted.train, train.actual)   )
  
}
