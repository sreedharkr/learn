# clustering and samples wrongly clustered
data(iris)
library(cluster)
library(robustbase)
library(fpc) # plotcluster function

set.seed(222)
km <- kmeans(iris[,1:4], centers = 3, iter.max = 300, nstart = 5)
km$cluster
iris_c <- cbind(iris, cluster= km$cluster)
table(iris$Species,  iris_c$cluster)
wrong <- subset(iris_c, subset = (Species == 'versicolor' & cluster != 3) )
wrong2 <- subset(iris_c, subset = (Species == 'virginica' & cluster != 1) )


plotcluster(iris[, 1:4], km$cluster)
plotcluster(iris[, 1:4], iris$Species)

clusplot(iris[,1:4], km$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)
with(iris, pairs(iris[, 1:4], col=c(1:3)[km$cluster]))
#
disse <- daisy(iris[, 1:4])
de2 <- disse^2
sk2 <- silhouette(km$cl, de2)
plot(sk2)
#

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