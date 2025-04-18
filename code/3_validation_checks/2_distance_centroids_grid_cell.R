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

# Compute, for each ethnic group, the distance between all cells and the centroid

# Geographic centroid and ethnic group 1
dist_geographic = data.frame()
for(x in 1:726){
temp_ethnic = raster_ethnies[[x]] %>% as.data.frame() %>% mutate(pop = value * coverage_fraction) 
temp_centroid = geographic_centroids %>%
  mutate(ethnic_id = row_number()) %>% filter(ethnic_id == x) %>%
  st_coordinates() %>% as.data.frame() 
temp = cbind(temp_ethnic,temp_centroid) %>%
  mutate(dist = pop*sqrt((X-x)^2 + (Y-y)^2)) %>%
  summarize(pwad_geographic = 1/sum(pop,na.rm=T) * sum(dist,na.rm=T)) %>% mutate(ethnic_id = x)
dist_geographic = rbind(dist_geographic,temp)
}

dist_weighted = data.frame()
for(x in 1:726){
  temp_ethnic = raster_ethnies[[x]] %>% as.data.frame() %>% mutate(pop = value * coverage_fraction) 
  temp_centroid = weighted_centroids %>%
    mutate(ethnic_id = row_number()) %>% filter(ethnic_id == x) %>%
    st_coordinates() %>% as.data.frame() 
  temp = cbind(temp_ethnic,temp_centroid) %>%
    mutate(dist = pop*sqrt((X-x)^2 + (Y-y)^2)) %>%
    summarize(pwad_weighted = 1/sum(pop,na.rm=T) * sum(dist,na.rm=T)) %>% mutate(ethnic_id = x)
  dist_weighted = rbind(dist_weighted,temp)
}

dist_kmean = data.frame()
for(x in 1:726){
  temp_ethnic = raster_ethnies[[x]] %>% as.data.frame() %>% mutate(pop = value * coverage_fraction) 
  temp_centroid = kmean_centroids %>%
    mutate(ethnic_id = rep(1:726,each=3)) %>% filter(ethnic_id == x) %>%
    st_coordinates() %>% as.data.frame() 
  temp=data.frame()
  for(point in 1:3){
  temp_point = cbind(temp_ethnic,temp_centroid[point,]) %>%
    mutate(dist = pop*sqrt((X-x)^2 + (Y-y)^2)) %>%
    summarize(pwad_kmean = 1/sum(pop,na.rm=T) * sum(dist,na.rm=T)) %>% mutate(ethnic_id = x)
  temp=rbind(temp,temp_point)
  }
  temp=temp %>% summarize(pwad_kmean=mean(pwad_kmean,na.rm=T)) %>% mutate(ethnic_id=x) 
  dist_kmean = rbind(dist_kmean,temp)
}

dist_all = full_join(dist_geographic,dist_kmean) %>% full_join(dist_weighted)

dist_all %>%
  pivot_longer(cols=c(starts_with("pwad_")),names_to = "method",values_to="pwad") %>%
  group_by(method) %>%
  summarise(mean = mean(pwad, na.rm = TRUE),
                sd = sd(pwad, na.rm = TRUE),
                n = n()) %>%
  mutate(se = sd / sqrt(n),
         lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
         upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
