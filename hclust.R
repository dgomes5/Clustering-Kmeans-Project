# Daniel Gomes, John Gomes, Blake Simmons
# CIS 490: Sectional Project 3
# Hclust algorithm

# Load iris dataset
require("datasets")
data("iris") # load Iris Dataset

library(mclust)
library(ggplot2)

set.seed(77)

# Hierarchical clustering using Complete Linkage
iris.hclust <- hclust(dist(iris[,3:4]))
plot(iris.hclust) # Plot the obtained dendrogram

# determined the denogram, we want to use 3 clusters
iris.clusterCut <- cutree(iris.hclust, 3)
table(iris.clusterCut, iris$Species)
# 50 + 21 + 50 = 121
# 29
# accuracy = 121 / (121 + 29) = 0.806 or 80.6% accurate

# Hierarchical clustering using average linkage
iris.hclust <- hclust(dist(iris[, 3:4]), method = 'average')
plot(iris.hclust) # Plot the obtained dendrogram

# determined the denogram, we want to use 3 clusters
iris.clusterCut <- cutree(iris.hclust, 3)
table(iris.clusterCut, iris$Species)
# 50 + 45 + 49 = 144
# 5 + 1
# accuracy = 144 / (144 + 6) = 0.96 or 96% accurate

# plot petal.lentgh and petal.width
ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = iris.clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))

# plot sepal.length and petal.width
ggplot(iris, aes(Sepal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = iris.clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))
