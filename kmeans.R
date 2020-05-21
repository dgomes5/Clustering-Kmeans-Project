# Daniel Gomes, John Gomes, Blake Simmons
# CIS 490: Sectional Project 3
# Kmeans algorithm

# Load iris dataset
require("datasets")
data("iris") # load Iris Dataset
summary(iris)

set.seed(490)

# Plot elbow method to find best k
k.max <- 10
wss<- sapply(1:k.max,function(k){kmeans(iris[,3:4],k,nstart = 20,iter.max = 20)$tot.withinss})
wss
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")

# remove class label and store to new variables
iris.new<- iris[,c(1,2,3,4)]
iris.class<- iris[,"Species"]

# do k means algorithm
iris.kmeans <- kmeans(iris.new,3) #apply k-means algorithm with no. of centroids(k)=3

par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(iris.new[c(1,2)], col=iris.kmeans$cluster)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed in clusters
plot(iris.new[c(1,2)], col=iris.class)# Plot to see how Sepal.Length and Sepal.Width data points have been distributed originally as per "class" attribute in dataset
plot(iris.new[c(3,4)], col=iris.kmeans$cluster)# Plot to see how Petal.Length and Petal.Width data points have been distributed in clusters
plot(iris.new[c(3,4)], col=iris.class)

table(iris.kmeans$cluster,iris.class)
# 50 + 48 + 36 = 134
# 2 + 14 = 16
# accuracy = 134 / (134 + 16) = 0.8874 or 88.74% accurate
