# calculate p-values, perform pca and calculate p-values
# bestsubset, forward-stepwise, backward-stepwise

learn_pvalue <- function(){
bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
bcancer3 <- bcancer[c(-1)]
bcancer3_data <- bcancer3[1:500,] 
bcancer3_test <- bcancer3[501:569,2:31]
bcancer3_test_results <- bcancer[501:569,2]
source('exploratory.R')
# Warning: glm.fit: fitted probabilities numerically 0 or 1 
# occurred means that the data is possibly linearely separable
#glm.wis3 <- glm(diagnosis ~ .,family=binomial(link="logit"),data=bcancer3_data,maxit=100)
#glm.wis3 <- glm(diagnosis ~ .,family=binomial,data=bcancer3_data,maxit=100)
#glm.wis3 <- glm(diagnosis ~ radius_mean + texture_mean + radius_mean*texture_mean, family=binomial,data=bcancer3_data,maxit=100)
glm.wis3 <- glm(diagnosis ~ radius_mean*texture_mean, family=binomial,data=bcancer3_data,maxit=100)
library(interplot)
interplot(m = glm.wis3, "radius_mean","texture_mean")
print(summary(glm.wis3))
summary(glm.wis3)$coefficients[,4]
summary(glm.wis3)$coefficients[,4] < 0.05
}

pca_pvalue <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1,-2)]
  #train data
  bcancer3_data <- bcancer3[1:500,] 
  bcancer3_data_results <- bcancer[1:500,2]
  #test data
  bcancer3_test <- bcancer3[501:569,1:30]
  bcancer3_test_results <- bcancer[501:569,2]
  source('exploratory.R')
  #apply pca
  pca.bcancer <- prcomp(bcancer3_data,center = TRUE,scale. = TRUE)
  train.data <- data.frame(diagnosis=bcancer3_data_results, pca.bcancer$x)
  train.data2 <- train.data[1:6]
  glm.wis3 <- glm(diagnosis ~ .,family=binomial,data=train.data2,maxit=100)
  # glm.wis3 <- glm(diagnosis ~ train.data2$PC1 * train.data2$PC2,family=binomial,data=train.data2,maxit=100
  # summary(glm.wis3)$coefficients[,4] < 0.05
  #summary(glm.wis3)
  cf <- summary(glm.wis3)$coefficients
  cf2 <- cf[,4]
  print(cf)
  print(cf2)
  count <- ifelse(cf2 < 0.05,1,0)
  print(count)
  tab <- table(count)
  print(tab[names(tab)==1])
  print(sum(count == 1))
  # test.data <- predict(pca.bcancer, newdata = bcancer3_test)
  # test.data <- as.data.frame(test.data)
  # test.data2 <- test.data[1:7]
  # 
  # results.wis <- predict(glm.wis3,test.data2)
  # results.wis <- ifelse(results.wis >= 0.5,'M','B')
  # print(results.wis)
  # results.wis <- as.factor(results.wis)
  # misClasificError <- mean(results.wis != bcancer3_test_results)
  # print(paste('Accuracy',1-misClasificError))
}

leaps_pack <- function(){
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  # step forward
  step_forward = regsubsets(diagnosis ~.  ,data = bcancer3, method = "forward",nvmax = 7)
  #summary(reg2)
  print("step forward::::  ")
  print(step_forward$vorder)
  print(step_forward$vorder[1:7])
  #step back ward
  step_backward <- regsubsets(diagnosis ~.  ,data = bcancer3, method = "backward",nvmax = 7)
  #summary(reg2)
  print("step backward::::  ")
  print(step_backward$vorder)
  print(step_backward$vorder[1:7])
  #best subset
  best_sub <- regsubsets(diagnosis ~.  ,data = bcancer3,nvmax = 7)
  print("best subset::::  ")
  print(best_sub$vorder[1:7])
}

fimportance <- function() {
  set.seed(7)
  library(mlbench)
  library(caret)
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  bcancer <- read.csv("datasets/breast-cancer-wisconsin-data.csv",sep = ",")
  bcancer3 <- bcancer[c(-1)]
  model <- train(diagnosis~., data=bcancer3, method="lvq", preProcess="scale", trControl=control)
  importance <- varImp(model, scale=FALSE)
  print(importance)
  ind_imp <- importance$importance
  print(ind_imp[1:4,1:2])
  # plot importance
  plot(importance)
  # perimeter_worst          0.9755
  # radius_worst             0.9704
  # area_worst               0.9698
  # concave.points_worst     0.9667
  # concave.points_mean      0.9644
  library(ggplot2)
  ggplot(bcancer3, aes(perimeter_worst, radius_worst, color = diagnosis)) + geom_point()
}

