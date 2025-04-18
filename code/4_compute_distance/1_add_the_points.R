# ------------------------------------------------------------------------------
#
#   This script opens the network and:
#     - adds the kmean centroids 
#     - creates ad hoc road segment between the network and the centroids
#     - saves the updated network 
#
# ------------------------------------------------------------------------------

    # Open the network and the centroids -------------------------------------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    nodes = read_sf(paste0(path_folder,"/output/data/graph_nodes_current_roads.shp"))
    edges = read_sf(paste0(path_folder,"/output/data/graph_edges_current_roads.shp")) %>%
      group_by(from,to,length) %>%
      filter(row_number()==1)
    kmean_centroids = read_sf(paste0(path_folder,"/temp/centroids/k_centroids.shp"))
    
    # Add the nodes ----------------------------------------------------------------
    
    nodes_with_centroids = kmean_centroids %>%
      mutate(node_id = row_number()+nrow(nodes)) %>%
      dplyr::select(node_id) %>%
      rbind(nodes) %>%
      arrange(node_id)
    
      # Save the nodes ID with the associated ethnic groups for further use
    nodes_id_centroids = kmean_centroids %>%
      mutate(node_id = row_number()+nrow(nodes)) %>%
      as.data.frame() %>%
      dplyr::select(-geometry)
    write.csv2(nodes_id_centroids,paste0(path_folder,"/output/data/nodes_id_centroids.csv"),row.names = F)
    
      # Make a n*(n-1)/2 with all possible distances
    dyad = expand.grid(orig = min(nodes_id_centroids$node_id):max(nodes_id_centroids$node_id),
                       dest = min(nodes_id_centroids$node_id):max(nodes_id_centroids$node_id)) %>%
      filter(orig != dest) %>%
      rowwise() %>%
      mutate(pair = paste0(min(orig, dest), "-", max(orig, dest))) %>%
      distinct(pair, .keep_all = TRUE) %>%
      select(orig, dest)
    write.csv2(dyad,paste0(path_folder,"/temp/dyad.csv"),row.names=F)
    
    # Create the edges -------------------------------------------------------------
      # Two steps: identify the closest node from each kmean_centroids
      #            create a LINE geometry between these two
    
    closest = st_nearest_feature(kmean_centroids,nodes,longlat=T) %>% 
      as.data.frame() 
    
    closest = closest %>%
      mutate(node_id = row_number()+nrow(nodes)) %>%
      left_join(nodes_with_centroids) 
    names(closest) = c("node_id","to","geometry_kmean")
    closest = closest %>%
      left_join(nodes_with_centroids) %>%
      rename(geometry_network = geometry,
             from = node_id) %>%
      mutate(R_current = 9,
             R_future = 9,
             edge_id = max(edges$edge_id)+row_number())
    
    closest$geometry =
      Map(st_union, closest$geometry_kmean, closest$geometry_network) %>%
      st_as_sfc(crs = st_crs(nodes)) %>%
      st_cast("LINESTRING") 

    closest = closest %>%
      dplyr::select(-geometry_kmean,-geometry_network) %>%
      st_as_sf(sf_column_name = "geometry") %>%
      mutate(length = as.numeric(st_length(geometry))/1000)
    
    edges_with_centroids = rbind(closest,edges) %>% arrange(edge_id)
    
    write_sf(nodes_with_centroids, dsn = paste0(path_folder,"/output/data/graph_nodes_with_centroids.shp"))
    write_sf(edges_with_centroids, dsn = paste0(path_folder,"/output/data/graph_edges_with_centroids.shp"))
    
    rm(list = setdiff(ls(), "path_folder"))
    