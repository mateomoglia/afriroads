# Load necessary libraries
library(ggplot2)
library(dplyr)

# Step 1: Generate a random dataset on a 9x9 grid
set.seed(123)  # Ensure reproducibility
n <- 81  # Number of points
data <- data.frame(
  x = rep(1:9,times=9),
  y = rep(1:9,each=9),
  weight = sample(0:5000,size=n,replace=T)
)

# Step 2: Visualize the initial data on a grid
ggplot(data, aes(x = factor(x), y = factor(y), size = weight)) +
  geom_point(alpha = 0.6) +
  labs(title = "Random Dataset on a 9x9 Grid",
       x = "Grid X",
       y = "Grid Y",
       size = "Weight") +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Step 3: K-means clustering process
k <- 3  # Number of clusters
set.seed(456)
initial_centers <- data %>% 
  sample_n(k) %>% 
  select(x, y)

# Initialize cluster centers
centers <- as.matrix(initial_centers)

# Step-by-step K-means
for (i in 1:1) {
  # Assign points to the nearest cluster
  data <- data %>% mutate(
    cluster = apply(., 1, function(row) {
      dists <- apply(centers, 1, function(center) sqrt((row["x"] - center[1])^2 + (row["y"] - center[2])^2))
      which.min(dists)
    })
  )
  
  # Visualize current clustering on the grid
  ggplot(data, aes(x = factor(x), y = factor(y), color = as.factor(cluster), size = weight)) +
    geom_point(alpha = 0.6) +
    geom_point(data = as_tibble(centers), aes(x = factor(x), y = factor(y)), 
               color = "black", size = 5, shape = 4) +
    labs(title = paste("K-means Iteration", i, "on a 9x9 Grid"),
         x = "Grid X",
         y = "Grid Y",
         color = "Cluster",
         size = "Weight") +
    scale_size_area(max_size = 10) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  # Recalculate cluster centers
  centers <- data %>% 
    group_by(cluster) %>% 
    summarise(
      x = round(weighted.mean(x, weight)),
      y = round(weighted.mean(y, weight))
    ) %>% 
    select(x, y) %>% 
    as.matrix()
}

# Final clustering
final_kmeans <- kmeans(data %>% select(x, y), centers = k, iter.max = 10)
data$cluster <- as.factor(final_kmeans$cluster)
data = data %>%
  group_by(cluster) %>%
  mutate(largest = ifelse(weight == max(weight),1,0))

# Final plot on the grid
ggplot(data, aes(x = factor(x), y = factor(y), color = cluster, size = weight)) +
  geom_point(alpha = 0.6) +
  geom_point(data = as_tibble(final_kmeans$centers), aes(x = (x), y = (y)), 
             color = "black", size = 5, shape = 4) +
  geom_point(data = data %>% filter(largest==1), aes(x=x,y=y), color="black",shape=1) +
  labs(title = "Final K-means Clustering on a 9x9 Grid",
       x = "Grid X",
       y = "Grid Y",
       color = "Cluster",
       size = "Weight") +
  scale_size_area(max_size = 10) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
