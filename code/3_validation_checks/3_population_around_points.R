# ------------------------------------------------------------------------------
#
# This script takes all centroids and compute the population weighted distance
# between each ethnic-grid cell and the centroid
#
# ------------------------------------------------------------------------------

rm(list=ls())

pacman::p_load(tidyverse,ggplot2,sf,exactextractr,raster)

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"

# Open the pop and the centroids -----------------------------------------------

ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
geographic_centroids = read_sf(paste0(path_folder,"/temp/centroids/geographic_centroids.shp"))
weighted_centroids = read_sf(paste0(path_folder,"/temp/centroids/weighted_centroids.shp"))
kmean_centroids = read_sf(paste0(path_folder,"/temp/centroids/k_centroids.shp"))

pop = raster(paste0(path_folder,"/raw/WorldPop/pop_africa_2010.tif"))
pop = projectRaster(pop,crs = st_crs(ethnies)$proj4string)
raster_ethnies = exactextractr::exact_extract(pop,ethnies,include_xy=T)

# Compute population around buffers --------------------------------------------

# Geographic centroid 
dist_geographic = data.frame()
for(x in 1:726){
  
  # Ethnic group population
  temp_ethnic = raster_ethnies[[x]] %>% 
    mutate(pop = value * coverage_fraction) %>% 
    st_as_sf(coords=c("x","y"),crs=4326)
  
  # Keep the relevant centroids
  temp_centroid = geographic_centroids %>% 
    filter(ethnic_id == ethnies$ethnic_id[[x]])
  
  # Compute the distance between each point and the ethnic centroid + cumulative share
  temp = data.frame(temp_ethnic,dist = as.numeric(st_distance(temp_ethnic,temp_centroid))/1000) %>%
    as.data.frame() %>% 
    dplyr::select(-geometry,-coverage_fraction,-value) %>%
    mutate(dist_bin = floor(dist)) %>%
    group_by(dist_bin) %>% 
    summarize(pop = sum(pop,na.rm=T)) %>% 
    ungroup() %>%
    arrange(dist_bin) %>%
    mutate(popshare = cumsum(pop)/sum(pop)) %>% 
    mutate(ethnic_id = ethnies$ethnic_id[[x]]) %>%
    rename(cumpop_geographic = popshare)
  dist_geographic = rbind(dist_geographic,temp)
  cat("Ethnic group",x,"done!\r")
}

# Population weighted centroid
dist_weighted = data.frame()
for(x in 1:726){
  
  # Ethnic group population
  temp_ethnic = raster_ethnies[[x]] %>% 
    mutate(pop = value * coverage_fraction) %>% 
    st_as_sf(coords=c("x","y"),crs=4326)
  
  # Keep the relevant centroids
  temp_centroid = weighted_centroids %>% 
    filter(ethnic_id == ethnies$ethnic_id[[x]])

  # Compute the distance between each point and the ethnic centroid + cumulative share
  temp = data.frame(temp_ethnic,dist = as.numeric(st_distance(temp_ethnic,temp_centroid))/1000) %>%
    as.data.frame() %>% 
    dplyr::select(-geometry,-coverage_fraction,-value) %>%
    mutate(dist_bin = floor(dist)) %>%
    group_by(dist_bin) %>% 
    summarize(pop = sum(pop,na.rm=T)) %>% 
    ungroup() %>%
    arrange(dist_bin) %>%
    mutate(popshare = cumsum(pop)/sum(pop)) %>% 
    mutate(ethnic_id = ethnies$ethnic_id[[x]]) %>%
    rename(cumpop_weighted = popshare)
  dist_weighted = rbind(dist_weighted,temp)
  cat("Ethnic group",x,"done!\r")
}


# Kmean centroid
dist_kmean = data.frame()
for(x in 1:726){
  
  # Ethnic group population
  temp_ethnic = raster_ethnies[[x]] %>% 
    mutate(pop = value * coverage_fraction) %>% 
    st_as_sf(coords=c("x","y"),crs=4326)
  
  # Keep the relevant centroids
  temp_centroid = kmean_centroids %>% 
    filter(ethnic_id == ethnies$ethnic_id[[x]])
  
  temp = data.frame()
  for(point in 1:3){
  temp_point = data.frame(temp_ethnic,dist = as.numeric(st_distance(temp_ethnic,temp_centroid %>% filter(cluster==point)))/1000) %>%
    as.data.frame() %>% 
    dplyr::select(-geometry,-coverage_fraction,-value) %>%
    mutate(dist_bin = floor(dist)) %>%
    group_by(dist_bin) %>% 
    summarize(pop = sum(pop,na.rm=T)) %>% 
    ungroup() %>%
    arrange(dist_bin) %>%
    mutate(popshare = cumsum(pop)/sum(pop)) %>% 
    mutate(ethnic_id = ethnies$ethnic_id[[x]]) %>%
    rename(cumpop_kmean = popshare) %>%
    mutate(cluster = point)
  temp = rbind(temp,temp_point)
  }
  
  temp = temp %>% 
    group_by(dist_bin) %>% 
    summarize(pop = mean(pop,na.rm=T),
              cumpop_kmean = mean(cumpop_kmean,na.rm=T)) %>% 
    mutate(ethnic_id = ethnies$ethnic_id[[x]])
  
  dist_kmean = rbind(dist_kmean,temp)
  cat("Ethnic group",x,"done!\r")
}

write.csv2(dist_geographic,paste0(path_folder,"/temp/distances/dist_geographic_centroids.csv"),row.names=F)
write.csv2(dist_weighted,paste0(path_folder,"/temp/distances/dist_weighted_centroids.csv"),row.names=F)
write.csv2(dist_kmean,paste0(path_folder,"/temp/distances/dist_kmean_centroids.csv"),row.names=F)

dist_geographic = data.table::fread(paste0(path_folder,"/temp/distances/dist_geographic_centroids.csv"))
dist_weighted = data.table::fread(paste0(path_folder,"/temp/distances/dist_weighted_centroids.csv"))
dist_kmean = data.table::fread(paste0(path_folder,"/temp/distances/dist_kmean_centroids.csv"))

# Cumulative distribution
rbind(dist_geographic %>% ungroup() %>%
  arrange(dist_bin) %>%
  group_by(dist_bin) %>%
  summarize(cumpop = mean(cumpop_geographic)) %>%
  mutate(centroid = "geographic"),
  dist_weighted %>% ungroup() %>%
    arrange(dist_bin) %>%
    group_by(dist_bin) %>%
    summarize(cumpop = mean(cumpop_weighted)) %>%
    mutate(centroid="weighted")) %>%
  rbind(dist_kmean %>% ungroup() %>%
          arrange(dist_bin) %>% 
          group_by(dist_bin) %>%
          summarize(cumpop = mean(cumpop_kmean)) %>%
          mutate(centroid = "kmean")) %>%
  ggplot(aes(x=log(dist_bin),y=cumpop,color=centroid)) +
  geom_line() +
  theme_bw() +
  xlab("Distance from centroid (log)") + ylab("Cumulative share")
ggsave(paste0(path_folder,"/output/graph/average_cumulative_population_around_centroidss.pdf"),
       width=20,height=7,unit="cm")

# For how many ethnic groups the kmean is better than the geographic centroids?


# dta_to_plot = dist_all %>%
#   group_by()
#   pivot_longer(cols=starts_with("share"),names_to="method",values_to="share",names_prefix = "share_") %>%
#   mutate(method = case_when(method == "geographic" ~ "Geographic",
#                             method == "weighted" ~ "Weighted",
#                             method == "kmean" ~ "Kmean"),
#          dist_bin = case_when(dist_bin == 1 ~ "<1km",
#                               dist_bin == 2 ~ "<5km",
#                               dist_bin == 3 ~ "<10km",
#                               dist_bin == 4 ~ "<25km",
#                               dist_bin == 5 ~ "<50km",
#                               dist_bin == 6 ~ "<100km",
#                               dist_bin == 7 ~ ">100km")) %>%
#   mutate(dist_bin = factor(dist_bin,levels=c("<1km","<5km","<10km","<25km","<50km","<100km",">100km"),
#                            labels = c("<1km","<5km","<10km","<25km","<50km","<100km",">100km")))
# 
# ggplot(dta_to_plot,aes(x=dist_bin,y=share,shape=method,color=method)) +
#   geom_point(size=2) +
#   theme_bw() +
#   scale_color_manual(name="Centroid",
#                      values = c("Geographic" = "black", "Weighted" = "indianred", "Kmean" = "cadetblue")) +
#   scale_shape_discrete(name="Centroid") +
#   ylab("Share of population in buffer") +
#   theme(axis.title.x = element_blank(),
#         legend.position = "bottom") 
# 
# ggsave(paste0(path_folder,"/output/graph/population_buffer_around_points.pdf"))
