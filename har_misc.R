ha <- read.csv("./dataset-har-PUC-Rio-ugulino.txt",
header = T, sep = ";")
lapply(ha,is.numeric)
class( lapply(ha,is.numeric) )
class( sapply(ha,is.numeric) )
ha$body_mass_index <- as.numeric(gsub(",",".",ha$body_mass_index))
unique(ha$body_mass_index)
dim(ha)
ha.num.list <- sapply(ha, is.numeric)
ha.num.list
ha.numeric <- ha[ha.num.list]
dim(ha.numeric)
str(ha.numeric)
cor(ha.numeric)
cor(ha.numeric)
cmatrix <- cor(ha.numeric)
library(caret)
??findCorrelation
findCorrelation(cmatrix,cutoff = 0.3)
findCorrelation(cmatrix,cutoff = 0.4)
findCorrelation(cmatrix,cutoff = 0.7)
findCorrelation(cmatrix,cutoff = 0.7,names = T)
cmatrix
findCorrelation(cmatrix,cutoff = 0.7,names = T)
table(ha[,x1])
table(ha[,c(x1)])
table(ha[,c("x1")])
summary(ha[,"x1"])
summary(ha[,"y1"])
ha.walking <- ha[, class="walking"]
str(ha)
unique(ha$class)
ha.walk <- subset(ha, subset = "class = 'walking' ")
ha.walk <- subset(ha, subset = "class == 'walking' ")
ha.walk <- subset(ha,class == 'walking')
dim(ha.walk)
table(ha$class)
summary(ha.walk)
summary(ha.walk$x1)
summary(ha.walk$y1)
summary(ha.walk$z1)
str(ha)
unique(ha$user)
ha.walk.debora <- subset(ha,class == 'walking' && user == 'debora')
dim(ha.walk.debora)
subset(ha, user == debora)
subset(ha, user == 'debora')
ab <- subset(ha, user == 'debora')
dim(ab)
ha.walk.debora <- subset(ha,class == 'walking' & user == 'debora')
dim(ha.walk.debora)
summary(ha.walk.debora$x1)
summary(ha.walk.debora$y1)
summary(ha.walk.debora$z1)
unique(ha$class)
ha.stand.debora <- subset(ha,class == 'standing' & user == 'debora')
summary(ha.stand.debora$x1)
summary(ha.stand.debora$y1)
summary(ha.stand.debora$z1)
summary(ha.stand.debora$y1)
summary(ha.stand.debora$z1)
summary(ha.stand.debora$x1)
unique(ha$class)
ha.sitdown.debora <- subset(ha,class == 'sittingdown' & user == 'debora')
summary(ha.sitdown.debora$x1)
summary(ha.sitdown.debora$y1)
summary(ha.sitdown.debora$z1)
summary(ha.sitdown.debora$z3)
summary(ha.sitdown.debora$x3)
summary(ha.sitdown.debora$y3)
str(ha)
