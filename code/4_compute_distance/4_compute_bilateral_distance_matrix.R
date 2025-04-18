# ------------------------------------------------------------------------------
#
#   This script creates compute four objects:
#     - Unweighted shortest distance
#     - Unweighted shortest time
#     - weighted shortest distance
#     - weighted shortest time
#
# ------------------------------------------------------------------------------

    nodes = read_sf(paste0(path_folder,"/output/data/graph_nodes_with_centroids.shp"))
    edges = read_sf(paste0(path_folder,"/output/data/graph_edges_with_centroids_and_links.shp"))
    nodes_id_centroids = data.table::fread(paste0(path_folder,"/output/data/nodes_id_centroids.csv"))
    
    source(paste0(path_folder, "/code/4_compute_distance/0_function_shortest_path.R"))
    
    #---------------------------------------------------------------------------
    # Counterfactual 1: Compute the speed & distance after a uniform increase
    # in a given road type speed
    #---------------------------------------------------------------------------
    
    # Choose the speeds
    speeds = data.frame(
      "dirt" = 10, "improved" = 30, "roads" = 40, "highways" = 80, "connect" = 10
    )
    
    # Choose the ethnic groups to connect
    ethnies_to_connect = c("ABABDA","ZULU")
    # ethnies_to_connect = c(unique(nodes_id_centroids$NAME))
    
    # Run the cft
    cft = compute_shortest_path_general_increase(speeds,ethnies_to_connect)

    # Save the output
    build_the_output(cft,"cft0",path = paste0(path_folder,"/output/data"))
    
    #---------------------------------------------------------------------------
    # Counterfactual 2: Compute the speed & distance after the completion of the 
    # TAH
    #---------------------------------------------------------------------------
    
    # Choose the speeds
    speeds = data.frame(
      "dirt" = 10, "improved" = 30, "roads" = 40, "highways" = 80, "connect" = 10
    )
    
    # Choose the ethnic groups to connect
    # ethnies_to_connect = c("ABABDA","ZULU")
    ethnies_to_connect = c(unique(nodes_id_centroids$NAME))
    
    # Run the cft
    cft = compute_shortest_path_general_increase(speeds,ethnies_to_connect)
    
    # Save the output
    build_the_output(cft,"cft_tah",path = paste0(path_folder,"/output/data"))
    
    #---------------------------------------------------------------------------
    # Counterfactual 3: Improve the path between two given ethnic groups
    #---------------------------------------------------------------------------
    
    # Choose the speeds
    speeds = data.frame(
      "dirt" = 10, "improved" = 30, "roads" = 40, "highways" = 80, "connect" = 10, "tah" = 80
    )
    speeds_updated = data.frame(
      "dirt" = 10, "improved" = 30, "roads" = 40, "highways" = 100, "connect" = 10, "tah" = 80
    )
    
    # Choose the ethnic groups to connect
    ethnies_to_connect = c("ABABDA","ZULU")
    # ethnies_to_connect = c(unique(nodes_id_centroids$NAME))
    
    # Run the cft
    cft = compute_shortest_path_specific(speeds, ethnies_to_connect, 
                                         orig_to_impr = "ABABDA",dest_to_impr = "LUO",
                                         speeds_updated)
    
    # Save the output: a nx(n-1)/2 matrix, a nxn matrix, and a csv with stats
    build_the_output(cft,"cft_impr",path = paste0(path_folder,"/output/data"))
    
    rm(list = setdiff(ls(), "path_folder"))
