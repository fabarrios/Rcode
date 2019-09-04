#  # # Clustering
# Example coppied from k-means example at R-Blogers
# 
# Initialiatrion and definition of helper plotting function
#
n <- 3 # no. of centroids
set.seed(1415) # set seed for reproducibility

M1 <- matrix(round(runif(100, 1, 5), 1), ncol = 2)
M2 <- matrix(round(runif(100, 7, 12), 1), ncol = 2)
M3 <- matrix(round(runif(100, 20, 25), 1), ncol = 2)
M <- rbind(M1, M2, M3)

C <- M[1:n, ] # define centroids as first n objects
obs <- length(M) / 2
A <- sample(1:n, obs, replace = TRUE) # assign objects to centroids at random
colors <- seq(10, 200, 25) 

clusterplot <- function(M, C, txt) {
  plot(M, main = txt, xlab = "", ylab = "")
  for(i in 1:n) {
    points(C[i, , drop = FALSE], pch = 23, lwd = 3, col = colors[i])
    points(M[A == i, , drop = FALSE], col = colors[i])    
  }
}
clusterplot(M, C, "Initialization")
# 
#k-means algorithm

repeat {
  # calculate Euclidean distance between objects and centroids
  D <- matrix(data = NA, nrow = n, ncol = obs)
  for(i in 1:n) {
    for(j in 1:obs) {
      D[i, j] <- sqrt((M[j, 1] - C[i, 1])^2 + (M[j, 2] - C[i, 2])^2)
    }
  }
  O <- A
  
  ## E-step: parameters are fixed, distributions are optimized
  A <- max.col(t(-D)) # assign objects to centroids
  if(all(O == A)) break # if no change stop
  clusterplot(M, C, "E-step")
  
  ## M-step: distributions are fixed, parameters are optimized
  # determine new centroids based on mean of assigned objects
  for(i in 1:n) {
    C[i, ] <- apply(M[A == i, , drop = FALSE], 2, mean)
  }
  clusterplot(M, C, "M-step")
}

# #
# Now using the k-means in Base-R
cl <- kmeans(M, n)
clusterplot(M, cl$centers, "Base R")
# 
#
(custom <- C[order(C[ , 1]), ])
(base <- cl$centers[order(cl$centers[ , 1]), ])

round(base - custom, 13)
# same results
