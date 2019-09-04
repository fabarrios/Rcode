# Example from HSAUR2, cluster analysis, Chapter 18
# Romano-British example
# 
# Create a distance for the pottery data
library(lattice)
library(scatterplot3d)
data("pottery", package = "HSAUR02")
#
pottery_dist <- dist(pottery[, colnames(pottery) != "kiln"])
levelplot(as.matrix(pottery_dist), xlab = "Pot Number", ylab = "Pot Number")

# Hierarchical clustering with inter-cluster dissimilarity
# Inter-group dissimilarity measure -> single linkage clustering
pottery_single <- hclust(pottery_dist, method = "single")
# Inter-group max -> complete linkage clustering
pottery_complete <- hclust(pottery_dist, method = "complete")
# Average linkage clustering
pottery_average <- hclust(pottery_dist, method = "average")

# The dendrogram plots with the regular plot and ggplot
layout(matrix(1:3, ncol = 3))
plot(pottery_single, main = "Single Linkage", sub = "", xlab = "")
plot(pottery_complete, main = "Complete Linkage", sub = "", xlab = "")
plot(pottery_average, main = "Average Linkage ", sub = "", xlab = "")
#
ggdendrogram(pottery_single, rotate=FALSE) + labs(title="Single Linkage")
ggdendrogram(pottery_complete, rotate=FALSE) + labs(title="Complete Linkage")
ggdendrogram(pottery_average, rotate=FALSE) + labs(title="Average Linkage")

pottery_cluster <- cutree(pottery_average, h = 4)
xtabs(~ pottery_cluster + kiln, data = pottery)

# Example of exoplanets
data("planets", package = "HSAUR2")
scatterplot3d(log(planets$mass), log(planets$period), log(planets$eccen), type = "h", angle = 55, pch = 16, y.ticklabs = seq(0, 10, by=2), y.margin.add = 0.1, scale.y = 0.7)
# To estimate the number of groups to apply a k-means clustering; a solution is plotting the within-group 
# sum of squares for partitions given by applying the kmeans procedure and looking for an 'elbows' in the
# resulting curve
rge <- apply(planets, 2, max) - apply(planets, 2, min)
planet.dat <- sweep(planets, 2, rge, FUN = "/")
n <- nrow(planet.dat)
wss <- rep(0, 10)
wss[1] <- (n-1) * sum(apply(planet.dat, 2, var))
for (i in 2:10)
  wss[i] <- sum(kmeans(planet.dat, centers = i)$withinss)
plot(1:10, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares")
# With some imagination we can see 'elboes' at 3, 5, 7 in the last plot
# to find the number of planets in each group, for 3, 5, 7 we can use 'centers'
planet_kmeans3 <- kmeans(planet.dat, centers = 3)
table(planet_kmeans3$cluster)
# To estimate the values for the k  clasification
# now to estimate the centers we can use a formula:
ccent <- function(cl) {
  f <- function(i) colMeans(planets[cl ==i,])
  x <- sapply(sort(unique(cl)), f)
  colnames(x) <- sort(unique(cl))
  return(x)
}
# Appliying the ccent formula to the kmeans for the 3 classification
ccent(planet_kmeans3$cluster)

# For te five cluster solution we can estimate the k-means
planet_kmeans5 <- kmeans(planet.dat, centers = 5)
table(planet_kmeans5$cluster)
ccent(planet_kmeans5)

