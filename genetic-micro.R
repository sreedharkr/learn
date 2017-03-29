# biocLite("hgu95av2.db")
# genetic-gahigh.R
# detach("package:Biobase", unload=TRUE)
# library(help="Biobase")
micro <- function() {
 library(Biobase)
 library(ALL)
 data(ALL)
 # ALL
 # pD <- phenoData(ALL)
 # varMetadata(pD)
 # table(ALL$BT)
 # table(ALL$mol.biol)
 # table(ALL$BT, ALL$mol.bio)
 # featureNames(ALL)[1:10]
 # sampleNames(ALL)[1:5]
 tgt.cases <- which(ALL$BT %in% levels(ALL$BT)[1:5] & ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
 ALLb <- ALL[,tgt.cases]
 ALLb$BT <- factor(ALLb$BT)
 ALLb$mol.bio <- factor(ALLb$mol.bio)
 # save(ALLb, file = "myALL.Rdata")
 es <- exprs(ALLb)
 #class(es)
 print( dim(es) )

 #summary(as.vector(es))
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
 ALLb <- nsFilter(ALLb,
                  var.func=IQR,
                  var.cutoff=IQR(as.vector(es))/5, 
                  feature.exclude="^AFFX")
 #print( ALLb) 
  ALLb <- ALLb$eset
  es <- exprs(ALLb)
 # print( dim(es1) )
 
 f <- Anova(ALLb$mol.bio, p=0.01)
 ff <- filterfun(f)
 selGenes <- genefilter(exprs(ALLb),ff)
 
 print("sum sel of genes")
 print (sum(selGenes)) 
 ALLb <- ALLb[selGenes,]
 #print( ALLb )
 es <- exprs(ALLb)
 #print(dim(es2))
 
 #es3 <- exprs(ALLb)
 plot(rowMedians(es),rowIQRs(es),
      xlab='Median expression level',
      ylab='IQR expression level',
      main='Distribution Properties of the Selected Genes')
 
 
 featureNames(ALLb) <- make.names(featureNames(ALLb))
 es <- exprs(ALLb)
 # print("dim es")
 # print(dim(es))
 
 # dt <- data.frame(es3)
 # print( dim(t(dt)) )
 Mut = ALLb$mol.bio
 #print(Mut)
 #head(dt)
 
 library(randomForest)
 dt <- data.frame( t(es), Mut = ALLb$mol.bio )
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
  load(file="./genedf.RData")
  # rf <- randomForest(diagnosis ~ ., data = train.data, ntree = 300, mtry = 3, proximity=TRUE, cutoff = c(0.6,0.4) )
  rf <- randomForest(Mut ~  ., dt, importance=T,ntree = 500 , mtry = 400)
  predicted.train <- predict(rf, type="class")
  train.actual <- dt[, ncol(dt)]
  bias.estimate <- mean(predicted.train != train.actual)
  print(paste('training Accuracy',1 - bias.estimate))
  print(  table(predicted.train, train.actual)   )
  
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
