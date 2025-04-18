# ------------------------------------------------------------------------------
#
# This script take the Murdock shapefiles and extract the population weighted
# centroid for each 3 kmean clusters for each ethnic group.
#
# ------------------------------------------------------------------------------
    
    set.seed(1234)
    
    # I. Open the shapefile for -covered- Murdock ethnic groups --------------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    
    # II. Open population data for Africa ------------------------------------------
    
    pop = raster(paste0(path_folder,"/raw/WorldPop/pop_africa_2010.tif"))
    pop = projectRaster(pop,crs = st_crs(ethnies)$proj4string)
    
    # III. Compute the population weighted centroids -------------------------------
    
    raster_ethnies = exactextractr::exact_extract(pop,ethnies,include_xy=T)
    # This is a list of 726 elements, containing all cells covered by the ethnic group
    # the population, the xy, and the share of cell covered. 
    
    # Loop over all ethnic groups to compute the kmean cluster and its weighted centroid
    
    pb <- txtProgressBar(min = 0, max = length(raster_ethnies), style = 3)
    
    k_centroids = do.call(rbind, lapply(seq_along(raster_ethnies), function(group) {
      temp = do.call(cbind, raster_ethnies[[group]]) %>% 
        as.data.frame() %>%
        mutate(weighted_pop = value * coverage_fraction,
               weighted_x = x * weighted_pop,
               weighted_y = y * weighted_pop) %>%
        filter(!is.na(value))
      
      # Compute the kmean on weighted_pop
      kmean_results = kcca(temp[, c("x", "y")], k = 3, weights = temp$weighted_pop, family = kccaFamily("kmeans"))
      temp = temp %>%
        mutate(cluster = clusters(kmean_results))
      
      suppressMessages({
      result = temp %>%
        group_by(cluster) %>% # Compute the weighted centroid
        summarize(x = ifelse(sum(weighted_pop, na.rm = TRUE) > 0, sum(x * weighted_pop, na.rm = TRUE) / sum(weighted_pop, na.rm = TRUE), mean(x,na.rm=T)),
                  y = ifelse(sum(weighted_pop, na.rm = TRUE) > 0, sum(y * weighted_pop, na.rm = TRUE) / sum(weighted_pop, na.rm = TRUE), mean(y,na.rm=T)),
                  pop = sum(weighted_pop)) %>%
        mutate(ethnic_id = as.numeric(ethnies$ethnic_id[[group]])) %>%
        full_join(
          temp %>% 
            st_as_sf(coords=c("x","y"),crs=4326) %>% 
            group_by(cluster) %>% 
            summarize(geometry_cluster = st_convex_hull(st_union(geometry))) # Extract the geom of the cluster
        ) %>%
        st_as_sf() %>%
        mutate(area = as.numeric(st_area(geometry_cluster))/1000000) # Compute area in sqkm
      })
      
      setTxtProgressBar(pb, group)
      result
    }))
    
    close(pb)
    
    k_centroids = left_join(k_centroids,ethnies %>% as.data.frame() %>% dplyr::select(ethnic_id,NAME)) 
    
    k_clusters_shapefile = k_centroids %>% dplyr::select(-x,-y) %>%
      rename(geometry=geometry_cluster)
    write_sf(k_clusters_shapefile,paste0(path_folder,"/temp/centroids/k_clusters_shapefile.shp"))
    
    k_centroids_shapefile = k_centroids %>%
      st_drop_geometry() %>%
      st_as_sf(coords = c("x","y"), crs = 4326)
    
    write_sf(k_centroids_shapefile,paste0(path_folder,"/temp/centroids/k_centroids.shp"))
    
    rm(list = setdiff(ls(), "path_folder"))
