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
 # ALLb$BT <- factor(ALLb$BT)
 # ALLb$mol.bio <- factor(ALLb$mol.bio)
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
 es1 <- exprs(ALLb)
 print( dim(es1) )
 
 f <- Anova(ALLb$mol.bio,p=0.01)
 ff <- filterfun(f)
 selGenes <- genefilter(exprs(ALLb),ff)
 
 
 sum(selGenes) 
 ALLb <- ALLb[selGenes,]
 #print( ALLb )
 es2 <- exprs(ALLb)
 print(dim(es2))
}
rowIQRs <- function(em) 
  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))

  




# library(genefilter)
# detach_package("vegan", TRUE)
