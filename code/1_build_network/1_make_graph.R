# ------------------------------------------------------------------------------
#
# This script takes the JS road network
# and make from it a weighted graph.
#
# ------------------------------------------------------------------------------
#
# Two output: one with the current network, one with the TAH.
# Inspired from here: https://r-spatial.org/r/2019/09/26/spatial-networks.html
#
# ------------------------------------------------------------------------------

rm(list=ls())

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"

pacman::p_load(igraph,rmapshaper,sf,igraph,tidyr,tidygraph,tmap,units,dplyr,ggplot2)

for(network in c("current_roads","future_roads")){

# 0. Open the road network -----------------------------------------------------

roads.shp = read_sf(paste0(path_folder,"/temp/shapefile/",network,"_network.shp")) %>%
  mutate(length = as.numeric(st_length(geometry))/1000)

# I. Make a graph --------------------------------------------------------------

  #-----------------------------------------------------------------------------
  # The package only takes from, to and a weight as input
  # So I extract the nodes from each road and -try to- associate each node
  # to a road. 
  #-----------------------------------------------------------------------------

  # 1) I "explode" all MULTILINESTRING into LINESTRING 

edges = roads.shp %>%
  mutate(edge_id = row_number()) %>%
  ms_explode() %>%
  filter(!st_is_empty(geometry))
  # I obtain 60912 roads in SSA

  # 2) Extract the starting point and the ending point of each road segment

nodes = edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edge_id=L1) %>%
  group_by(edge_id) %>%
  slice(c(1,n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start','end'),times = n()/2)) %>%
  mutate(xy = paste(.$X,.$Y)) %>%
  mutate(node_id = group_indices(.,factor(xy, levels = unique(xy)))) %>%
  select(-xy)

source_nodes = nodes %>% # Extract the "origin" node
  filter(start_end == "start") %>%
  pull(node_id)

target_nodes = nodes %>% # Extract the "destination" node
  filter(start_end == "end") %>%
  pull(node_id)

  # 3) Add those starting and ending nodes to the edges dataset

edges = edges %>%
  mutate(from = source_nodes,
         to = target_nodes)

  # 4) Remove the duplicates

nodes = nodes %>%
  distinct(node_id, .keep_all = TRUE) %>%
  select(-c(edge_id, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

  #-----------------------------------------------------------------------------
  # We obtain a shapefiles with a common ID for orig and dest and the associated
  # roads length and speed. We should make it a graph. 
  #-----------------------------------------------------------------------------

# II. Make it a tibble graph and export ----------------------------------------

graph = tbl_graph(nodes = nodes,
                  edges = as_tibble(edges),
                  directed=F) 

# To export the graph, we need the nodes and the edges saved separately
write_sf(nodes, dsn = paste0(path_folder,"/output/data/graph_nodes_",network,"africa.shp"))
write_sf(edges, dsn = paste0(path_folder,"/output/data/graph_edges_",network,"africa.shp"))

  # We can visualize it on an interactive map 

tmap_mode("view")
tm_shape(graph %>% activate(edges) %>% as_tibble() %>% st_as_sf() %>% filter(TAH == 0)) +
  tm_lines(lwd = 0.85, col = "cadetblue") +
  tm_shape(graph %>% activate(edges) %>% as_tibble() %>% st_as_sf() %>% filter(TAH == 1)) +
  tm_lines(lwd = 5, col = "blue") +
  tm_shape(graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf()) +
  tm_dots(size = 0.005, col = "indianred") +
  tmap_options(basemaps = "OpenStreetMap")

# III. Compute shortest path (example) -----------------------------------------

from_node = graph %>%
  activate(nodes) %>%
  filter(node_id == 452) %>%
  pull(node_id)

to_node = graph %>%
  activate(nodes) %>%
  filter(node_id == 2892) %>%
  pull(node_id)

path = shortest_paths(graph = graph, from = from_node, to = to_node, output = "both",
                     weights = graph %>% activate(edges) %>% pull(length))

path_graph = graph %>%
  subgraph_from_edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()

ggplot() +
  geom_sf(data = sa.shp, fill = 'white') +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), color = 'gray80', lwd = 0.15) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph %>% activate(nodes) %>% filter(node_id %in% c(from_node, to_node)) %>% as_tibble() %>% st_as_sf(), size = 2) +
  theme_bw()

}