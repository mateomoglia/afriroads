# ------------------------------------------------------------------------------
#
# This script takes all centroids and compute the distance to the network
#
# ------------------------------------------------------------------------------

rm(list=ls())

pacman::p_load(tidyverse,ggplot2,sf)

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"

# Open the network and the centroids -------------------------------------------

network = read_sf(paste0(path_folder,"/temp/shapefiles/current_roads_network.shp"))
geographic_centroids = read_sf(paste0(path_folder,"/temp/centroids/geographic_centroids.shp"))
weighted_centroids = read_sf(paste0(path_folder,"/temp/centroids/weighted_centroids.shp"))
kmean_centroids = read_sf(paste0(path_folder,"/temp/centroids/k_centroids.shp"))

# Compute the distances --------------------------------------------------------

# Find the closest node of the network for each point

closest_geographic = st_nearest_feature(geographic_centroids ,network) %>% 
  data.frame(road_id = ., 
             dist_closest = as.numeric(st_distance(network[., ], geographic_centroids, by_element = TRUE))/1000) %>%
  mutate(ethnic_id = row_number()) %>%
  group_by(ethnic_id) %>%
  arrange(dist_closest) %>%
  slice(1) %>%
  dplyr::select(-road_id)

closest_weighted = st_nearest_feature(weighted_centroids, network) %>%
  data.frame(road_id = ., 
             dist_weighted = as.numeric(st_distance(network[., ], weighted_centroids, by_element = TRUE))/1000) %>%
  mutate(ethnic_id = row_number()) %>%
  group_by(ethnic_id) %>%
  arrange(dist_weighted) %>%
  slice(1) %>%
  dplyr::select(-road_id)

closest_kmean = st_nearest_feature(kmean_centroids, network) %>% 
  data.frame(road_id = ., 
             dist_kmean = as.numeric(st_distance(network[., ], kmean_centroids, by_element = TRUE))/1000) %>%
  mutate(ethnic_id = rep(1:726,each=3)) %>%
  group_by(ethnic_id) %>%
  arrange(dist_kmean) %>%
  slice(1) %>%
  dplyr::select(-road_id)

# Merge all datasets

closest_all = full_join(closest_geographic,closest_weighted) %>% full_join(closest_kmean)

ggplot(closest_all) +
  stat_density(aes(x=log(dist_closest), color = "Geographic", linetype = "Geographic"), geom = "line", lwd=0.75) +
  stat_density(aes(x=log(dist_weighted), color = "Weighted", linetype = "Weighted"), geom = "line", lwd=0.75) +
  stat_density(aes(x=log(dist_kmean), color = "Kmean", linetype = "Kmean"), geom = "line", lwd=0.75) +
  theme_bw() +
  ylab("Density") + xlab("Distance (log)") +
  scale_color_manual(name="Centroid",
                       values = c("Geographic" = "black", "Weighted" = "indianred", "Kmean" = "cadetblue")) +
  scale_linetype_discrete(name="Centroid") +
  theme(legend.position = "bottom")
ggsave(paste0(path_folder,"/output/graph/distance_road_points.pdf"))

closest_all %>% ungroup() %>%
  dplyr::select(-ethnic_id) %>%
  summarize_all(mean)
