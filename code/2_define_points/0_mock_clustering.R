# ------------------------------------------------------------------------------
#
# This script makes a mock grid-cell clustering for explanation purpose
#
# ------------------------------------------------------------------------------

set.seed(1234)
rm(list=ls())

pacman::p_load(tidyverse,ggplot2)

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"


# Generate a 10x10 grid of population
set.seed(123)
grid_data <- expand.grid(x = 1:10, y = 1:10) %>%
  mutate(population = sample(1:100, size = 100, replace = TRUE))

# K-means clustering with 3 clusters
set.seed(123)
kmeans_result <- kmeans(grid_data[, c("x", "y")], centers = 3)

# Add cluster and centroid information to the data
grid_data <- grid_data %>%
  mutate(cluster = as.factor(kmeans_result$cluster))

centroids <- as.data.frame(kmeans_result$centers) 

# Plot the grid without anything
ggplot(grid_data, aes(x = x, y = y)) +
  geom_point(aes(size=population),color="black") +
  labs(title = "Population gird cell", 
       x = "X Coordinate", 
       y = "Y Coordinate", 
       size = "Population") +
  theme_minimal()
ggsave(paste0(path_folder,"/output/graph/mock_clustering_before_kmean.pdf"))

# Plot the grid with clusters
ggplot(grid_data, aes(x = x, y = y)) +
  geom_point(aes(color = cluster, size = population)) +
  labs(title = "K-Means Clustering", 
       x = "X Coordinate", 
       y = "Y Coordinate", 
       color = "Cluster", 
       size = "Population") +
  theme_minimal()
ggsave(paste0(path_folder,"/output/graph/mock_clustering_after_kmean.pdf"))

# Plot the grid with clusters and centroids
ggplot(grid_data, aes(x = x, y = y)) +
  geom_point(aes(color = cluster, size = population)) +
  geom_point(data = centroids, aes(x = x, y = y), color = "black", size = 4, shape = 4) +
  labs(title = "K-Means Clustering with Centroids", 
       x = "X Coordinate", 
       y = "Y Coordinate", 
       color = "Cluster", 
       size = "Population") +
  theme_minimal()
ggsave(paste0(path_folder,"/output/graph/mock_clustering_after_kmean_with_centroids.pdf"))
