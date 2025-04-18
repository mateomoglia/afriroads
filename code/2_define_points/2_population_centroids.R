# ------------------------------------------------------------------------------
#
# This script take the Murdock shapefiles and extract the population weighted
# centroid for each ethnic group.
#
# ------------------------------------------------------------------------------

    # I. Open the shapefile for -covered- Murdock ethnic groups ----------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    
    # II. Open population data for Africa --------------------------------------
    
    pop = raster(paste0(path_folder,"/raw/WorldPop/pop_africa_2010.tif"))
    pop = projectRaster(pop,crs = st_crs(ethnies)$proj4string)
    
    # III. Compute the population weighted centroids ---------------------------
    
    raster_ethnies = exactextractr::exact_extract(pop,ethnies,include_xy=T)
    # This is a list of 726 elements, containing all cells covered by the ethnic group
    # the population, the xy, and the share of cell covered. 
    
    # Loop over all ethnic groups to compute the weighted centroids 
    
    w_centroids = data.frame()
    pb = txtProgressBar(min = 0, max = length(raster_ethnies), style = 3)
    for(group in 1:nrow(ethnies)){
      temp = do.call(cbind,raster_ethnies[[group]]) %>% 
        as.data.frame() %>%
        mutate(weighted_pop = value * coverage_fraction) %>%
        summarize(x = sum(x*weighted_pop,na.rm=T)/sum(weighted_pop,na.rm=T),
                  y = sum(y*weighted_pop,na.rm=T)/sum(weighted_pop,na.rm=T)) %>%
        mutate(ethnic_id = ethnies$ethnic_id[[group]])
      w_centroids = rbind(w_centroids,temp)
      setTxtProgressBar(pb, group)
    }
    rm(pb,temp)
    
    w_centroids = st_as_sf(w_centroids,coords = c("x","y"),crs=4326)
    w_centroids = left_join(w_centroids,ethnies %>% as.data.frame() %>% dplyr::select(ethnic_id,NAME))
    
    write_sf(w_centroids,paste0(path_folder,"/temp/centroids/weighted_centroids.shp"))
    
    rm(list = setdiff(ls(), "path_folder"))
    
