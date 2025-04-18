# ------------------------------------------------------------------------------
#
#   This script opens the network and:
#     - test if the graph is connected
#     - connect it
#
# ------------------------------------------------------------------------------

    # Open the network and the centroids -------------------------------------------
    
    nodes = read_sf(paste0(path_folder,"/output/data/graph_nodes_with_centroids.shp"))
    edges = read_sf(paste0(path_folder,"/output/data/graph_edges_with_centroids.shp")) 
    
    # Is this newly created graph connected ? --------------------------------------
    
    graph = tbl_graph(nodes = nodes,
                      edges = as_tibble(edges),
                      directed=F) 
    is_connected(graph) 
    
      # Sadly, no
      # We need to make this graph connected
    
    # # Extract the largest connected component
    # largest_connected = largest_component(graph)
    # # 39240 edges are in this graph (out of 61522)
    # components = data.frame(node_id = as.integer(V(graph)),
    #                                       component = components(graph)$membership) %>%
    #   arrange(node_id) %>%
    #   right_join(edges,join_by(node_id==from)) %>%
    #   rename(from=node_id)
    # 
    # components %>%
    #   group_by(component) %>%
    #   slice(1) %>%
    #   nrow()
    # # There are 4234 subgraphs
    # 
    # # Let's join subgraph 2 to the big network (component==1)
    # edge_1 = components %>% filter(component==1) %>% st_as_sf()
    # edge_2 = components %>% filter(component==2) %>% st_as_sf()
    # 
    # # Visual inspection
    # plot_box = st_bbox(edge_2)
    # 
    # ggplot() +
    #   geom_sf(data=edge_2,color="blue") +
    #   geom_sf(data=edge_1,color="black") +
    #   scale_x_continuous(limits = c(plot_box$xmin-0.05,plot_box$xmax+0.1)) + 
    #   scale_y_continuous(limits = c(plot_box$ymin-0.05,plot_box$ymax+0.1)) +
    #   theme_bw()
    # ggsave(paste0(path_folder,"/output/graph/iteration_0.pdf"))
    # 
    # distance_1_2 = data.frame(distance = as.numeric(st_distance(edge_1,edge_2))/1000) %>%
    #   mutate(edge_id = row_number()) %>%
    #   arrange(distance) %>%
    #   slice(1)
    # 
    # ggplot() +
    #   geom_sf(data=edge_2,color="blue") +
    #   geom_sf(data=edge_1,color="black") +
    #   geom_sf(data=edge_1 %>% filter(row_number() == distance_1_2$edge_id),color="purple",lwd=1) +
    #   scale_x_continuous(limits = c(plot_box$xmin-0.05,plot_box$xmax+0.1)) + 
    #   scale_y_continuous(limits = c(plot_box$ymin-0.05,plot_box$ymax+0.1)) +
    #   theme_bw()
    # ggsave(paste0(path_folder,"/output/graph/iteration_1.pdf"))
    # 
    # link_1_2 = st_nearest_points(edge_1 %>% filter(row_number() == distance_1_2$edge_id),edge_2)
    # 
    # ggplot() +
    #   geom_sf(data=edge_2,color="blue") +
    #   geom_sf(data=edge_1,color="black") +
    #   geom_sf(data=edge_1 %>% filter(row_number() == distance_1_2$edge_id),color="purple",lwd=1) +
    #   geom_sf(data=link_1_2,color="indianred",lwd=2) +
    #   scale_x_continuous(limits = c(plot_box$xmin-0.01,plot_box$xmax+0.01)) + 
    #   scale_y_continuous(limits = c(plot_box$ymin-0.01,plot_box$ymax+0.01)) +
    #   theme_bw()
    # ggsave(paste0(path_folder,"/output/graph/iteration_2.pdf"))
    # 
    # # Now we need to know if what does this link links? Which extremity of edge_1 and edge_2 are connected
    # endpoints1 = st_cast(st_geometry(edge_1 %>% filter(row_number() == distance_1_2$edge_id)), "POINT")
    # endpoints2 = st_cast(st_geometry(edge_2), "POINT")
    # 
    # # Calculate the distance between all endpoints and find the closest pair
    # dist_matrix = st_distance(endpoints1, endpoints2)
    # min_index   = which(dist_matrix == min(dist_matrix), arr.ind = TRUE)
    # point1      = endpoints1[min_index[1]]
    # point2      = endpoints2[min_index[2]]# There are 439 meters between the two
    # 
    # origin      = if (min_index[1] == 1) edge_1 %>% filter(row_number()==distance_1_2$edge_id) %>% pull(from) else edge_1$to
    # destination = if (min_index[2] == 1) edge_2$from else edge_2$to
    # 
    # # Create a line connecting the closest endpoints
    # connecting_line = st_sfc(st_linestring(rbind(st_coordinates(point1), st_coordinates(point2))), crs = 4326) %>%
    #   st_as_sf() %>% rename(geometry=x) %>%
    #   mutate(from = origin, to = destination, TAH = 0, R_current = 10) %>%
    #   mutate(length = as.numeric(st_length(geometry))/1000) %>%
    #   mutate(edge_id = nrow(edge_1)+1) %>%
    #   mutate(component = 2)
    # 
    # ggplot() +
    #   geom_sf(data=edge_2,color="blue") +
    #   geom_sf(data=edge_1,color="black") +
    #   geom_sf(data=edge_1 %>% filter(row_number() == distance_1_2$edge_id),color="purple",lwd=1) +
    #   geom_sf(data=connecting_line,color="indianred",lwd=2) +
    #   geom_sf(data=point1) + geom_sf(data=point2) +
    #   scale_x_continuous(limits = c(plot_box$xmin-0.01,plot_box$xmax+0.01)) + 
    #   scale_y_continuous(limits = c(plot_box$ymin-0.01,plot_box$ymax+0.01)) +
    #   theme_bw()
    # ggsave(paste0(path_folder,"/output/graph/iteration_3.pdf"))
    # 
    # edge_1 = rbind(edge_1,connecting_line)
    # 
    # rm(distance_1_2,edge_1,edge_2,endpoints1,endpoints2,link_1_2,point1,point2,plot_box)
    
        #---------------------------------------------
        # Populate the largest subgraph with all subgraphs
        # 
        # I proceed iteratively. Take all subgraphs and compute
        # their distance to subgraph_1. Take the closest subgraph.
        # Join it. Recompute the distances. And again.
        # 
        #---------------------------------------------
    
    cond = TRUE
    threshold = 0
    edges_with_links = edges
    connecting_edges = data.frame()
    iter = 0
    
    while(cond==TRUE){
    
    graph = tbl_graph(nodes = nodes,
                      edges = as_tibble(edges_with_links),
                      directed=F) 
    cat("\nThere are",components(graph)$no-1,"subgraphs\n")
    
    components = data.frame(node_id = as.integer(V(graph)),
                            component = components(graph)$membership) %>%
      arrange(node_id) %>%
      right_join(edges,join_by(node_id==from)) %>%
      rename(from=node_id)
    
    edge_main_subgraph = components %>% filter(component == 1) %>% st_as_sf()
    
    if(components(graph)$no>1){
      threshold = threshold + 1
      cat("\nStart the loop with a threshold of",threshold,"\n")
    }
    else{
      cond = FALSE
      cat("\nDone!")
      next
    }
    
    for(sub in 2:(components(graph)$no)){
      iter = iter + 1
      edge_subgraph = components %>% filter(component == sub) %>% st_as_sf()
    
      distance_main_to_sub = st_distance(edge_main_subgraph,edge_subgraph) %>%
        as.data.frame() %>%
        mutate(across(everything(), ~ as.numeric(.) / 1000)) %>%
        mutate(min_value = pmap_dbl(across(everything()), min)) %>%
        mutate(edge_id = row_number()) %>%
        filter(min_value == min(min_value)) %>%
        filter(min_value <= threshold) %>%
        slice(1) %>%
        rowwise() %>%
        mutate(min_col_index = which.min(c_across(everything()))) %>%
        ungroup() 
      
      if(nrow(distance_main_to_sub)==0){
        next
      }
      
      endpoints_main = rbind(st_startpoint(edge_main_subgraph %>% dplyr::filter(row_number() == distance_main_to_sub$edge_id)) %>% st_as_sf(),
                             st_endpoint(edge_main_subgraph %>% dplyr::filter(row_number() == distance_main_to_sub$edge_id)) %>% st_as_sf())
      endpoints_sub  = rbind(st_startpoint(edge_subgraph %>% dplyr::filter(row_number() == distance_main_to_sub$min_col_index)) %>% st_as_sf(),
                             st_endpoint(edge_subgraph %>% dplyr::filter(row_number() == distance_main_to_sub$min_col_index)) %>% st_as_sf())
      
      # Calculate the distance between all endpoints and find the closest pair
      dist_matrix = st_distance(endpoints_main, endpoints_sub)
      min_index   = which(dist_matrix == min(dist_matrix), arr.ind = TRUE)
      point_main  = endpoints_main[min_index[1],]
      point_sub   = endpoints_sub[min_index[2],]
      
      origin      = if (min_index[1] == 1) edge_main_subgraph %>% filter(row_number()==distance_main_to_sub$edge_id) %>% pull(from) else edge_main_subgraph %>% filter(row_number()==distance_main_to_sub$edge_id) %>% pull(to)
      destination = if (min_index[2] == 1) edge_subgraph %>% filter(row_number() == distance_main_to_sub$min_col_index) %>% pull(from) else edge_subgraph %>% filter(row_number() == distance_main_to_sub$min_col_index) %>% pull(to)
      
      # Create a line connecting the closest endpoints
      connecting_line = st_sfc(st_linestring(rbind(st_coordinates(point_main), st_coordinates(point_sub))), crs = 4326) %>%
        st_as_sf() %>% rename(geometry=x) %>%
        mutate(from = origin, to = destination, TAH = 0, R_current = 10) %>%
        mutate(length = as.numeric(st_length(geometry))/1000) %>%
        mutate(edge_id = nrow(edges)+sub-1) %>%
        mutate(component = sub)
      
      connecting_edges = rbind(connecting_edges,connecting_line)
      cat("\rSubgraph",sub,": Length is",round(connecting_line$length,2),"km.")
      
      rm(connecting_line,endpoints_main,endpoints_sub,distance_main_to_sub)
    }
    cat("\n")
    edges_with_links = rbind(edges,connecting_edges %>% dplyr::select(-component))
    
    }
    
    # Test if the graph is connected 
    graph_with_links = tbl_graph(nodes = nodes,
                                 edges = as_tibble(edges_with_links),
                                 directed=F) 
    components(graph_with_links)$no
    
    if(components(graph_with_links)$no==1){
    write_sf(edges_with_links, dsn = paste0(path_folder,"/output/data/graph_edges_with_centroids_and_links.shp"))
    }
    
    
    ### Inspect what are those graphs
    
    # edges_with_links = read_sf(paste0(path_folder,"/output/data/graph_edges_with_centroids_and_links.shp"))
    # 
    # ggplot(edges_with_links %>% filter(R_current == 10), aes(x=log(length))) +
    #   geom_density() +
    #   theme_bw() +
    #   xlab("Link length (log)") + ylab("Density")
    # 
    # edges_with_links %>% 
    #   st_drop_geometry() %>%
    #   filter(R_current == 10) %>%
    #   mutate(length = log(length)) %>%
    #   group_by(length_bin = round(length,2)) %>%
    #   summarize(count = sum(n())) %>%
    #   ungroup() %>%
    #   ggplot(mapping=aes(x=length_bin,y=..count..)) +
    #   geom_histogram(binwidth = 0.1, color = "NA", fill = "gray30",alpha=1) +
    #   xlab("Link length (log)") + ylab("Count") +
    #   theme_bw()
    # 
    # ggplot(edges_with_links %>% filter(R_current == 10), aes(x=log(length),y=..density..)) +
    #   geom_histogram(binwidth = 0.1, color = "white", fill = "white",alpha=1) +
    #   geom_histogram(binwidth = 0.1, color = NA, fill = "cadetblue",alpha=0.75) +
    #   geom_density(color="indianred",lwd=0.75) +
    #   theme_bw() +
    #   xlab("Link length (log)") + ylab("Freq.")
    # ggsave(paste0(path_folder,"/output/graph/graph_length_links.pdf"))
    # africa_sf = read_sf(paste0(path_folder,"/raw/afrilearnr/africountries.shp"))
    # 
    # ggplot(data = edges_with_links %>% filter(R_current == 10 & length > 10)) +
    #   geom_sf(data = africa_sf) +
    #   geom_sf(color="red",lwd=1) +
    #   theme_minimal() 
    # 
    # 
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 10))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 25))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 50))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 75))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 100))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 125))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 150))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 175))
    # nrow(edges_with_links %>% filter(R_current == 10 & length > 200))
    
    rm(list = setdiff(ls(), "path_folder"))
    