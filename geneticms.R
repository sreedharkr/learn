# biocLite("hgu95av2.db")
# genetic-gahigh.R
# detach("package:Biobase", unload=TRUE)
# library(help="Biobase")
micro <- function() {
 library(Biobase)
 library(ALL)
 data(ALL)
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
 #Anova filtering data
 f <- Anova(ALLb$mol.bio, p=0.01)
 ff <- filterfun(f)
 selGenes <- genefilter(exprs(ALLb),ff)
 
 print("sum sel of genes")
 print (sum(selGenes)) 
 ALLb <- ALLb[selGenes,]
 es <- exprs(ALLb)
 
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
 save(dt, file = "genedf.Rdata")
 #print(head(dt[1,]))
 rf <- randomForest(Mut ~  ., dt, importance=T )
 imp <- importance(rf)
 imp <- imp[,ncol(imp)-1]
 rf.genes <- names(imp)[order(imp,decreasing=T)[1:100]]
 print(rf.genes)
 
 dt2 <- dt[, rf.genes]
 dt3 <- cbind(dt2, Mut = ALLb$mol.bio)
 rf100 <- randomForest(Mut ~  ., dt3, importance=T )
}
rowIQRs <- function(em) 
  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))

# library(genefilter)
# detach_package("vegan", TRUE)

rf <- function(){
  library(randomForest)
  library(caret)
  load(file="./genedf.RData")
  set.seed(444)
  rfimp <- randomForest(Mut ~  ., dt, importance=T )
  imp <- importance(rfimp)
  imp <- imp[,ncol(imp)-1]
  rf.genes <- names(imp)[order(imp,decreasing=T)[1:50]]
  print(rf.genes[1:10])
  dt2 <- dt[, rf.genes]
  dt3 <- cbind(dt2, Mut = ALLb$mol.bio)
  print(dim(dt3))
  print(table(dt3$Mut))
  rf <- randomForest(Mut ~  ., dt3,ntree = 300 , mtry = 10 )
  # rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, mtry = 3, proximity=TRUE, cutoff = c(0.6,0.4) )
  #rf <- randomForest(Mut ~  ., dt, importance=T,ntree = 500 , mtry = 400)
  #predicted.train <- predict(rf, type="class")
  #train.actual <- dt3[, ncol(dt3)]
  print(names(rf))
  print(rf$confusion)
  table(dt3$Mut, rf$predicted)
  
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

logistic <- function(dt3){
  # nnet::multinomial
  library(glmnet)
  df1 <- dt3[,1: (ncol(dt3) - 1) ]
  df2 <- dt3[, ncol(dt3)]
  cvglm <- cv.glmnet(x = as.matrix(df1), y = as.matrix(df2), 
                     family = "binomial",alpha = 1,
                     nfolds = 10,standardize=FALSE)
  print( summary(cvglm) )
  predicted <- predict(object = cvglm, s = cvglm$lambda.min, as.matrix(df.test1),type = "class")
  predicted <- as.factor(predicted)
  cf <- confusionMatrix(predicted,df.test.results,positive = 'M' )
  print(cf$table)
  
  
}

neural <- function(){
  library(caret)
  library(nnet)
  net22 <- multinom(data = dt3, formula = Mut ~ .)
  pd2 <- predict(net22, newdata = dt3)
  cf1 <- confusionMatrix(pd2, dt3[,ncol(dt3)])
  cf1$table
}


knnha <- function(){
  load(file="./genedf.RData")
  library(caret)
  set.seed(222)
  inTrain <- createDataPartition(dt$Mut, p = 0.3, list = FALSE)
  train.data <- dt[inTrain,]
  test.data <- dt[-inTrain,]
  
  #knnm <- knn3(x = train.data[-c(15)],y = train.data$class,k = 5, test.data[ -c(15)] )
  v1 <- c(2,3,4,5)
  for(a in v1) {
  knnm <- knn3(x = train.data[-ncol(dt)],y = train.data$Mut,k = a)
  predicted <- predict(knnm, newdata = train.data[ -ncol(test.data)], type = "class" )
  cmatrix <- confusionMatrix(data = predicted, reference = train.data$Mut)
  print( cmatrix )
  }
  library(class)
  set.seed(222)
  knn2 <- knn.cv(train = train.data[-ncol(dt)], cl = train.data$Mut, k = 4, prob = F)
  cmatrix2 <- confusionMatrix(data = knn2, reference = train.data$Mut)
  print( cmatrix2 )
  
  set.seed(222)
  #ctrl <- trainControl(method="repeatedcv",repeats = 3)
  ctrl <- trainControl(method="repeatedcv", number =3, repeats = 3)
  #knnFit <- train(Mut ~ ., data = train.data, method = "knn", trControl = ctrl, preProcess = c("center","scale"))
  knnFit <- train(Mut ~ ., data = train.data, method = "knn", trControl = ctrl,tuneLength = 5)
  print(knnFit)
}

bootstrap2 <- function(){
  iris.boot <- iris[sample.int(nrow(iris),size=(nrow(iris)*1),replace=TRUE),]
  boot.ind <- !( rownames(iris) %in% row.names(iris.boot) )
  iris22 <- iris[boot.ind,]
  print(dim(iris22))
  
  library(caret)
  library(klaR)
  # load the iris dataset
  data(iris)
  # define training control
  train_control <- trainControl(method="boot", number=100)
  # train the model
  model <- train(Species~., data=iris, trControl=train_control, method="nb")
  # summarize results
  print(model)
  #very imp 
  samp <- createResample(1:nrow(iris), 3)
  samp1 <- unique(unlist(samp[1]))
  iris11 <- iris[samp1,]
  iris1 <- iris[-samp1,]
  
  samp2 <- unique(unlist(samp[2]))
  iris21 <- iris[samp2,]
  iris2 <- iris[-samp2,]
  
}