# ------------------------------------------------------------------------------
#
#   This script creates several functions to run the shortest path algorithms
#     - Unweighted shortest distance
#     - Unweighted shortest time
#     - weighted shortest distance
#     - weighted shortest time
#
# ------------------------------------------------------------------------------

    # Function 1: Increase the speed of a given road type ----------------------
    #---------------------------------------------------------------------------
    
    compute_shortest_path_general_increase = function(speeds, ethnies) {
      
      # Select the relevant dyads 
      dyad = data.table::fread(paste0(path_folder, "/temp/dyad.csv")) %>%
        filter(orig %in% c(nodes_id_centroids %>%
                 dplyr::filter(NAME %in% ethnies) %>% pull(node_id)) &
                dest %in% c(nodes_id_centroids %>%
                 dplyr::filter(NAME %in% ethnies) %>% pull(node_id))) %>%
        arrange(orig,dest)
      
      # Adapt the speed depending on the counterfactual
      edges_with_speed = edges %>%
        mutate(
          speed = case_when(
            R_current == -Inf ~ as.numeric(speeds["dirt"]),
            R_current == 0 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_current == 1 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_current == 2 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_current == 3 ~ as.numeric(speeds["improved"]),
            # Improved roads
            R_current == 5 ~ as.numeric(speeds["roads"]),
            # Roads
            R_current == 8 ~ as.numeric(speeds["highways"]),
            # Highways
            R_current == 9 ~ as.numeric(speeds["connect"]),
            # Set missing to 0
            is.na(R_current) ~ 0,
          ) 
        ) %>%
        mutate(time = length / speed)
      
      # Create the graph -------------------------------------------------------
      
      graph = tbl_graph(
        nodes = nodes,
        edges = as_tibble(edges_with_speed),
        directed = F
      )

      results <- do.call(rbind, lapply(1:nrow(dyad), function(x) {
        from_node = as.numeric(dyad[x, 1])
        to_node = as.numeric(dyad[x, 2])
        
        shortest_path <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(length)
        )
        
        quickest_time <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(time)
        )
        
        length <- graph %>%
          subgraph_from_edges(eids = shortest_path$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(length = sum(length, na.rm = TRUE))
        
        time <- graph %>%
          subgraph_from_edges(eids = quickest_time$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(time = sum(time, na.rm = TRUE))
        
        data.frame(
          orig = dyad[x, 1],
          dest = dyad[x, 2],
          length = length$length,
          time = time$time
        )
      }))
      
      return(results)
    }
    
    # Function 2: Use the Transafrican Highway speeds --------------------------
    #---------------------------------------------------------------------------
    
    compute_shortest_path_tah = function(speeds, ethnies) {
      
      # Select the relevant dyads 
      dyad = data.table::fread(paste0(path_folder, "/temp/dyad.csv")) %>%
        filter(orig %in% c(nodes_id_centroids %>%
                             dplyr::filter(NAME %in% ethnies) %>% pull(node_id)) &
                 dest %in% c(nodes_id_centroids %>%
                               dplyr::filter(NAME %in% ethnies) %>% pull(node_id))) %>%
        arrange(orig,dest)
      
      # Adapt the speed depending on the counterfactual
      edges_with_speed = edges %>%
        mutate(
          speed = case_when(
            R_future == -Inf ~ as.numeric(speeds["dirt"]),
            R_future == 0 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_future == 1 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_future == 2 ~ as.numeric(speeds["dirt"]),
            # Dirt roads
            R_future == 3 ~ as.numeric(speeds["improved"]),
            # Improved roads
            R_future == 5 ~ as.numeric(speeds["roads"]),
            # Roads
            R_future == 8 ~ as.numeric(speeds["highways"]),
            # Highways
            R_future == 9 ~ as.numeric(speeds["connect"]),
            # Connecting (ethnic centroids <-> network)
            R_future == 10 ~ as.numeric(speeds["tah"]),
            # Set missing to 0
            is.na(R_future) ~ 0,
          ) 
        ) %>%
        mutate(time = length / speed)
      
      # Create the graph -------------------------------------------------------
      
      graph = tbl_graph(
        nodes = nodes,
        edges = as_tibble(edges_with_speed),
        directed = F
      )
      
      results <- do.call(rbind, lapply(1:nrow(dyad), function(x) {
        from_node = as.numeric(dyad[x, 1])
        to_node = as.numeric(dyad[x, 2])
        
        shortest_path <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(length)
        )
        
        quickest_time <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(time)
        )
        
        length <- graph %>%
          subgraph_from_edges(eids = shortest_path$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(length = sum(length, na.rm = TRUE))
        
        time <- graph %>%
          subgraph_from_edges(eids = quickest_time$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(time = sum(time, na.rm = TRUE))
        
        data.frame(
          orig = dyad[x, 1],
          dest = dyad[x, 2],
          length = length$length,
          time = time$time
        )
      }))
      
      return(results)
    }
      
    
    # Function 3: Increase the speed of a given path ---------------------------
    #---------------------------------------------------------------------------
    
    compute_shortest_path_specific = function(speeds, ethnies, orig_to_impr, dest_to_impr, speeds_updated) {
      
      # Select the relevant dyads 
      dyads = data.table::fread(paste0(path_folder, "/temp/dyad.csv"))
      dyad = dyads %>%
        filter(orig %in% c(nodes_id_centroids %>%
                             dplyr::filter(NAME %in% ethnies) %>% pull(node_id)) &
                 dest %in% c(nodes_id_centroids %>%
                               dplyr::filter(NAME %in% ethnies) %>% pull(node_id))) %>%
        arrange(orig,dest)
      
      # Select the origins and destinations to improve
      orig_impr = c(nodes_id_centroids %>% dplyr::filter(NAME %in% orig_to_impr) %>% pull(node_id))
      dest_impr = c(nodes_id_centroids %>% dplyr::filter(NAME %in% dest_to_impr) %>% pull(node_id)) 
      
      # Adapt the speed depending on the counterfactual
      edges_with_speed = edges %>%
        mutate(
          speed = case_when(
            # Dirt roads
            R_current == -Inf ~ as.numeric(speeds["dirt"]),
            R_current == 0 ~ as.numeric(speeds["dirt"]),
            R_current == 1 ~ as.numeric(speeds["dirt"]),
            R_current == 2 ~ as.numeric(speeds["dirt"]),
            # Improved
            R_current == 3 ~ as.numeric(speeds["improved"]),
            # Roads
            R_current == 5 ~ as.numeric(speeds["roads"]),
            # Highways
            R_current == 8 ~ as.numeric(speeds["highways"]),
            # Connect
            R_current == 9 ~ as.numeric(speeds["connect"])
          )
        ) %>%
        mutate(time = length / speed)
      
      # Create the graph -------------------------------------------------------
      
      graph = tbl_graph(
        nodes = nodes,
        edges = as_tibble(edges_with_speed),
        directed = F
      )
      
      # Extract the shortest paths between the ethnies to improve --------------
      
      shortest_to_impr = do.call(rbind,lapply(1:length(orig_impr), function(x) {
        sp_to_impr = shortest_paths(
          graph   = graph,
          from    = orig_impr[x],
          to      = dest_impr,
          output  = "epath",
          weights = graph %>% activate(edges) %>% pull(length)
        ) 
        sp_to_impr = sp_to_impr$epath[[1]] %>% as.numeric()
        sp_to_impr = edges %>% slice(sp_to_impr) %>% pull(edge_id) %>% as.data.frame()
        return(sp_to_impr)
      }))
      
      # Update the paths 
      edges_with_speed = edges %>%
        mutate(R_toimpr = ifelse(edge_id %in% shortest_to_impr,1,0)) %>%
        mutate(
          speed = case_when(
            # Dirt roads
            R_current == -Inf ~ as.numeric(speeds["dirt"]),
            R_current == 0 ~ as.numeric(speeds["dirt"]),
            R_current == 1 ~ as.numeric(speeds["dirt"]),
            R_current == 2 ~ as.numeric(speeds["dirt"]),
            # Improved roads
            R_current == 3 ~ as.numeric(speeds["improved"]),
            # Roads
            R_current == 5 ~ as.numeric(speeds["roads"]),
            # Highways
            R_current == 8 ~ as.numeric(speeds["highways"]),
            # Connecting links
            R_current == 9 ~ as.numeric(speeds["connect"]),
            
            # Same but for the roads to improve
            # Dirt
            R_current == -Inf & R_toimpr == 1 ~ as.numeric(speeds_updated["dirt"]),
            R_current == 0 & R_toimpr == 1 ~ as.numeric(speeds_updated["dirt"]),
            R_current == 1 & R_toimpr == 1 ~ as.numeric(speeds_updated["dirt"]),
            R_current == 2 & R_toimpr == 1 ~ as.numeric(speeds_updated["dirt"]),
            # Improved
            R_current == 3 & R_toimpr == 1 ~ as.numeric(speeds_updated["improved"]),
            # Roads
            R_current == 5 & R_toimpr == 1 ~ as.numeric(speeds_updated["roads"]),
            # Highways
            R_current == 8 & R_toimpr == 1 ~ as.numeric(speeds_updated["highways"]),
            # Connecting links
            R_current == 9 & R_toimpr == 1 ~ as.numeric(speeds_updated["connect"])
          )
        ) %>%
        mutate(time = length / speed)
      
      # Create the graph -------------------------------------------------------
      
      graph = tbl_graph(
        nodes = nodes,
        edges = as_tibble(edges_with_speed),
        directed = F
      )
      
      results <- do.call(rbind, lapply(1:nrow(dyad), function(x) {
        from_node = as.numeric(dyad[x, 1])
        to_node = as.numeric(dyad[x, 2])
        
        shortest_path <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(length)
        )
        
        quickest_time <- shortest_paths(
          graph = graph,
          from = from_node,
          to = to_node,
          output = "both",
          weights = graph %>% activate(edges) %>% pull(time)
        )
        
        length <- graph %>%
          subgraph_from_edges(eids = shortest_path$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(length = sum(length, na.rm = TRUE))
        
        time <- graph %>%
          subgraph_from_edges(eids = quickest_time$epath %>% unlist()) %>%
          as_tbl_graph() %>%
          activate(edges) %>%
          as_tibble() %>%
          st_drop_geometry() %>%
          summarize(time = sum(time, na.rm = TRUE))
        
        data.frame(
          orig = dyad[x, 1],
          dest = dyad[x, 2],
          length = length$length,
          time = time$time
        )
      }))
      
      return(results)
      
    }
    
    
    # Annex function: build the output matrices and the descriptive stats ------
    #---------------------------------------------------------------------------
    
    build_the_output = function(cft,file_name,path){
      
      pop_data = data.table::fread(paste0(path_folder,"/output/data/nodes_id_centroids.csv")) %>%
        group_by(ethnic_id) %>%
        mutate(share_pop = pop/sum(pop))
      
      data_to_output = cft %>%
        rename(node_id=orig) %>%
        left_join(pop_data) %>%
        rename(orig=NAME,
               cluster_orig=cluster,
               share_pop_orig=share_pop) %>%
        dplyr::select(length,time,orig,dest,cluster_orig,share_pop_orig) %>%
        rename(node_id=dest) %>%
        left_join(pop_data) %>%
        rename(dest=NAME,
               cluster_dest=cluster,
               share_pop_dest=share_pop) %>%
        filter(orig!=dest) %>%
        group_by(orig,dest) %>%
        summarize(uw_length = mean(length),
                  w_length = sum(share_pop_dest*share_pop_orig*length),
                  uw_time = mean(time),
                  w_time = sum(share_pop_dest*share_pop_orig*time)) %>%
        ungroup() 
      
      write.csv2(data_to_output,paste0(path,"/",file_name,"_paths.csv"),row.names=F)
      
      sum_stats = round(data.frame(Min = apply(matrix %>% select(-orig,-dest), 2, min),
                                   P10 = apply(matrix %>% select(-orig,-dest), 2, function(x) quantile(x,0.10)),
                                   P25 = apply(matrix %>% select(-orig,-dest), 2, function(x) quantile(x, 0.25)),
                                   P50 = apply(matrix %>% select(-orig,-dest), 2, median),
                                   P75 = apply(matrix %>% select(-orig,-dest), 2, function(x) quantile(x, 0.75)),
                                   P90 = apply(matrix %>% select(-orig,-dest), 2, function(x) quantile(x,0.90)),
                                   Max = apply(matrix %>% select(-orig,-dest), 2, max),
                                   Mean = round(colMeans(matrix %>% select(-orig,-dest)),2)),2)
      
      write.csv2(sum_stats,paste0(path,"/",file_name,"_sum_stats.csv"),row.names=F)
      
    }
s