# clustering and samples wrongly clustered
data(iris)
library(cluster)
library(robustbase)
library(fpc) # plotcluster function

set.seed(222)
km <- kmeans(iris[,1:4], centers = 3, iter.max = 300, nstart = 5)
km
iris_c <- cbind(iris, cluster= km$cluster)
table(iris$Species,  iris_c$cluster)
wrong <- subset(iris_c, subset = (Species == 'versicolor' & cluster != 3) )
wrong2 <- subset(iris_c, subset = (Species == 'virginica' & cluster != 1) )

#calculates the clutser centers
#aggregate(iris_c[1:4], by = list(cluster = km$cluster), FUN = mean)
aggregate(iris_c[1:4], by = list(km$cluster), FUN = mean)

plotcluster(iris[, 1:4], km$cluster)
plotcluster(iris[, 1:4], iris$Species)

clusplot(iris[,1:4], km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
with(iris, pairs(iris[, 1:4], col=c(1:3)[km$cluster]))
#
disse <- daisy(iris[, 1:4])
de2 <- disse^2
sk2 <- silhouette(km$cl, de2)
plot(sk2)

#iris_setosa1 <- iris_c[ iris_c$Species == 'setosa' & iris_c$cluster == '2',]
iris_setosa1 <- iris_c[iris_c$cluster == '2',]
#iris_versicolor1 <- iris_c[ iris_c$Species == 'versicolor' & iris_c$cluster == '3',]
iris_versicolor1 <- iris_c[iris_c$cluster == '3',]
head(iris_setosa1)
# calculates within cluster si=umofsquares
ss <- function(x) sum(scale(x, center = TRUE, scale = FALSE)^2)
ss(iris_setosa1[, 1:4])
ss(iris_versicolor1[, 1:4])
#
(cl <- kmeans(iris[,1:4], 3, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)

misc <- function() {
dist(iris,method = "euclidean", diag = FALSE,upper= FALSE,  p = 2)
dist(iris[,3:4])
}

iris_pca <- function() {
  data(iris)
  iris_pca <- prcomp(iris[, 1:4])
  iris_pca2 <- iris_pca$x[,1:2]
  iris_pca2 <- as.data.frame(iris_pca2)
  
  vars <- apply(iris_pca$x, MARGIN = 2, var)
  props <- vars/sum(vars)
  cumsum(props)
  
  df <- cbind(iris_pca2, Species = iris$Species)
  ggplot(df, aes(x = PC1, y = PC2, color = Species)) + geom_point()
  ggplot(iris, aes(x = iris$Sepal.Length, y = iris$Sepal.Width, color = Species)) + geom_point()
  ggplot(iris, aes(x = iris$Petal.Length, y = iris$Petal.Width, color = Species)) + geom_point()
}

iris_reorder <- function(x){
  #print('hello')
  if (x == 'setosa') { return(2)}
  if (x == 'virginica') { return(1)}
  if (x == 'versicolor') { return(3)}
}
train.indices <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
iris.train <- iris[train.indices,]
iris.test <- iris[-train.indices, ]
set.seed(333)
km <- kmeans(iris.train[, 1:4], 3)
t3 <- sapply(iris.train[, 5], FUN = iris_reorder)
cm1 <- confusionMatrix(data = km$cluster, reference = t3)
cm1$table
closest.cluster <- function(x) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
clusters2 <- apply(iris.test[, 1:4], 1, closest.cluster)
t4 <- sapply(iris.test[, 5], FUN = iris_reorder)
cm2 <- confusionMatrix(data = clusters2, reference = t4)
cm2$table
#correlation based distance matrix
factoextra::get_dist(USArrests, "pearson")