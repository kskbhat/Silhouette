# Load the iris dataset
data(iris)

# Run k-means clustering (e.g., 3 clusters)
kmeans_result <- kmeans(iris[, 1:4], centers = 3)

# Compute distance matrix (obs x clusters)
# distance_matrix <- as.matrix(dist(rbind(kmeans_result$centers, iris[, 1:4])))[-(1:3), 1:3]

# install.packages("proxy") # if not already installed
library(proxy)
distance_matrix <- proxy::dist(iris[, 1:4], kmeans_result$centers)
# Convert to matrix
distance_matrix <- as.matrix(distance_matrix)


out = silhobj(pm = distance_matrix,pmtype = "dissim")
plot(out)

