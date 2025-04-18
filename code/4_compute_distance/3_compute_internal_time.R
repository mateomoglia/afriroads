# ------------------------------------------------------------------------------
#
#   This script computes the internal distance/time for each ethnic cluster.
#   It is the average time to cross the cluster computed as the pop-weighted
#   avg. of all roads crossing the cluster.
#
# ------------------------------------------------------------------------------

    # Open the ethnic info + the road network --------------------------------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    nodes = read_sf(paste0(path_folder,"/output/data/graph_nodes_with_centroids.shp"))
    edges = read_sf(paste0(path_folder,"/output/data/graph_edges_with_centroids_and_links.shp"))
    nodes_id_centroids = data.table::fread(paste0(path_folder,"/output/data/nodes_id_centroids.csv"))
    clusters = read_sf(paste0(path_folder,"/temp/centroids/k_clusters_shapefile.shp"))
    centroids = read_sf(paste0(path_folder,"/temp/centroids/k_centroids.shp"))
    
    #-------------------------------------------------------------------------------
    # 0. Before, I benchmark using the Head-Mayer formula
    #-------------------------------------------------------------------------------
    
    hm = clusters %>%
      st_drop_geometry() %>%
      group_by(ethnic_id,cluster) %>%
      summarize(area = sum(area)) %>%
      ungroup() %>% 
      st_drop_geometry() %>%
      mutate(internal_hm = 2/3*sqrt(area/pi^2)) # Formula from Head-Mayer (2002)
    
    write.csv2(hm,paste0(path_folder,"/output/data/head-mayer_internal_distance.csv"),row.names=F)
    
    #-------------------------------------------------------------------------------
    # I. Extract all roads within each cluster
    #-------------------------------------------------------------------------------
    
    st_intersection_faster = function(x,y,...){
      
      y_subset <-
        st_intersects(x, y) %>%
        unlist() %>%
        unique() %>%
        sort() %>%
        {y[.,]}
      
      st_intersection(x, y_subset,...)
    }
    
    joined_edges = st_join(edges, clusters, join=st_intersects)
    
    ggplot() + 
      geom_sf(data = clusters %>% filter(NAME == "BUDUMA"),fill="white",color="black") + 
      geom_sf(data = joined_edges %>% filter(NAME == "BUDUMA"),color="blue") + 
      geom_sf(data = nodes %>% filter(node_id %in% c(53215,53216,53217))) +
      geom_sf(data = joined_edges %>% filter(NAME == "BUDUMA"),color="red") + 
      geom_sf(data = centroids %>% filter(NAME == "BUDUMA"), shape = 15) + 
      theme_bw() 
    
    pb = txtProgressBar(min = 0, max = nrow(clusters), style = 3)
    roads_in_cluster = do.call(rbind, lapply(seq(1:nrow(clusters)), function(group) { # 
      
      ethn = clusters %>% filter(row_number()==group) %>% pull(ethnic_id)
      clus = clusters %>% filter(row_number()==group) %>% pull(cluster)
      
      temp = st_intersection_faster(
        joined_edges %>% filter(ethnic_id == ethn & cluster == clus), 
        clusters %>% filter(row_number()==group)
      ) %>%
        mutate(length_in_cluster = as.numeric(st_length(geometry))/1000) 
      setTxtProgressBar(pb, group)
      temp
    }))
    close(pb)
    
    roads_in_cluster = roads_in_cluster %>% 
      mutate(temp_id = group_indices(., ethnic_id, cluster)) %>%
      dplyr::select(R_current,TAH,length,cluster,ethnic_id,NAME,temp_id,length_in_cluster)
    
    write_sf(roads_in_cluster,paste0(path_folder,"/temp/shapefiles/intersection_roads_k_clusters.shp"))
    
    roads_in_cluster = read_sf(paste0(path_folder,"/temp/shapefiles/intersection_roads_k_clusters.shp"))
    names(roads_in_cluster) = c("R_current","TAH","length","cluster","ethnic_id","NAME","temp_id","length_in_cluster","geometry")
    
    #-------------------------------------------------------------------------------
    # II. Keep the longest road and compute its length
    #-------------------------------------------------------------------------------
    
    longest = roads_in_cluster %>%
      mutate(longest_length = as.numeric(st_length(geometry))/1000) %>%
      group_by(ethnic_id,cluster) %>% 
      slice_max(order_by = longest_length, n = 1, with_ties = FALSE)  %>%
      mutate(speed = case_when(R_current == -Inf ~ 10,
                               R_current == 0 ~ 10,  # Dirt roads
                               R_current == 1 ~ 10,  # Dirt roads
                               R_current == 2 ~ 10,  # Dirt roads
                               R_current == 3 ~ 30,  # Improved roads
                               R_current == 5 ~ 40,  # Roads
                               R_current == 8 ~ 80,  # Highways
                               R_current == 9 ~ 10,  # Connecting (ethnic centroids <-> network)
                               R_current == 10 ~ 10) # Connecting (between subgraphs)
      ) %>%
      mutate(longest_time = longest_length/speed) %>%
      dplyr::select(c("ethnic_id","NAME","cluster","longest_length","longest_time")) %>%
      st_drop_geometry() %>%
      as.data.frame()
    
    write.csv2(longest,paste0(path_folder,"/output/data/longest_internal_distance.csv"),row.names=F)
    
    #-------------------------------------------------------------------------------
    # III. Compute the (population-weighted) average of road lengths and speeds in cluster 
    #-------------------------------------------------------------------------------
    
    pop = raster(paste0(path_folder,"/raw/WorldPop/pop_africa_2010.tif"))
    pop = projectRaster(pop,crs = st_crs(ethnies)$proj4string)
    raster_ethnies = exactextractr::exact_extract(pop,ethnies,include_xy=T)
    
    # Unweighted
    
    uw_avg = roads_in_cluster %>%
      mutate(road_length = as.numeric(st_length(geometry))/1000) %>%
      group_by(ethnic_id,cluster) %>%
      mutate(speed = case_when(R_current == -Inf ~ 10,
                               R_current == 0 ~ 10,  # Dirt roads
                               R_current == 1 ~ 10,  # Dirt roads
                               R_current == 2 ~ 10,  # Dirt roads
                               R_current == 3 ~ 30,  # Improved roads
                               R_current == 5 ~ 40,  # Roads
                               R_current == 8 ~ 80,  # Highways
                               R_current == 9 ~ 10,  # Connecting (ethnic centroids <-> network)
                               R_current == 10 ~ 10)) %>%
      summarize(uw_length = mean(road_length,na.rm=T),
                uw_time   = mean(road_length/speed,na.rm=T)) %>%
      st_drop_geometry() %>%
      as.data.frame()
    
    write.csv2(uw_avg,paste0(path_folder,"/output/data/uw_avg_internal_distance.csv"),row.names=F)
    
    # Weighted
    
    # Need to compute for road the population crossed by that road
    
    pop_rast = terra::rast(pop)
    
    pb = txtProgressBar(min = 0, max = nrow(roads_in_cluster), style = 3)
    pop_intersected = do.call(rbind, lapply(seq(1:nrow(roads_in_cluster)), function(group) { # 
      road_rast = roads_in_cluster %>%
        filter(row_number()==group)
      road_rast = terra::rasterize(terra::vect(road_rast), pop_rast, field=1, touches=TRUE)
      pop_masked = terra::mask(pop_rast, road_rast)
      pop_intersected = terra::global(pop_masked, sum, na.rm=TRUE)$sum
      output = cbind(roads_in_cluster %>% filter(row_number()==group),pop_intersected = pop_intersected)
      setTxtProgressBar(pb, group)
      output
    }))
    close(pb)
    gc()
    
    write_sf(pop_intersected,paste0(path_folder,"/temp/shapefiles/intersection_roads_k_clusters_with_population.shp"))
    
    w_avg = pop_intersected %>%
      st_drop_geometry() %>%
      group_by(ethnic_id,cluster) %>%
      mutate(speed = case_when(R_current == -Inf ~ 10,
                               R_current == 0 ~ 10,  # Dirt roads
                               R_current == 1 ~ 10,  # Dirt roads
                               R_current == 2 ~ 10,  # Dirt roads
                               R_current == 3 ~ 30,  # Improved roads
                               R_current == 5 ~ 40,  # Roads
                               R_current == 8 ~ 80,  # Highways
                               R_current == 9 ~ 10,  # Connecting (ethnic centroids <-> network)
                               R_current == 10 ~ 10)) %>%
      summarize(length = sum(length * pop_intersected, na.rm = T)/sum(pop_intersected, na.rm = T),
                time = sum((length/speed) * pop_intersected, na.rm = T)/sum(pop_intersected, na.rm =T)) %>%
      ungroup()
    
    write.csv2(w_avg,paste0(path_folder,"/output/data/w_avg_internal_distance.csv"),row.names=F)
    
