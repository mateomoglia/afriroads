# ---------------------------
#
# This script takes the JS road network
# and make from it a weighted graph.
#
# ---------------------------
#
# Notes: It may take several hour to run.
# Two output: one with the current network, one with the TAH.
#
# ---------------------------

path = "/Users/mmoglia/Dropbox/research/unil/transafrican"

pacman::p_load(igraph,shp2graph,sfnetworks,ggraph,tidygraph,validate,units,lwgeom)

  # I. Open the road network -----

# current_roads.shp = read_sf(paste0(path,"/temp/shapefiles/current_roads_network.shp"))
# future_roads.shp = read_sf(paste0(path,"/temp/shapefiles/future_roads_network.shp"))

current_roads.shp = read_sf(paste0(path,"/temp/shapefiles/current_sa_roads_network.shp")) %>%
  mutate(length = as.numeric(st_length(geometry))/1000) 

ggplot() +
  geom_sf(data=sa.shp,color="black",fill="white") +
  geom_sf(data=current_roads.shp, aes(alpha=R_current), linewidth=0.25, color = "blue") +
  theme(legend.position = NULL) +
  theme_bw()

# future_roads.shp = read_sf(paste0(path,"/temp/shapefiles/future_sa_roads_network.shp")) %>%
#  mutate(length = as.numeric(st_length(geometry))/1000)

  # Make the graph -----

  # The package only takes from, to and a weight as input
  # So (i) compute each segment length (ii) attribute a speed or a time
  # Repeat it from current network and future network

exploded_shp = current_roads.shp %>%
  mutate(road_id = row_number()) %>%
  ms_explode() %>%
  filter(!st_is_empty(geometry))

nodes = exploded_shp %>%
  mutate(start = st_startpoint(geometry),
         end   = st_endpoint(geometry)) %>%
  mutate(start_lon = st_coordinates(start)[,1],
         start_lat = st_coordinates(start)[,2],
         end_lon = st_coordinates(end)[,1],
         end_lat = st_coordinates(end)[,2]) %>%
  as.data.frame() %>%
  select(-geometry,-start,-end) 
connections = nodes %>%
  dplyr::inner_join(nodes, by = c("start_lon" = "end_lon", "start_lat" = "end_lat"), suffix = c("_current", "_next"),unmatched = "drop")

ggplot() +
  geom_sf(data=sa.shp,color="black",fill="white") +
  geom_sf(data=exploded_shp, aes(alpha=R_current), linewidth=0.25, color = "blue") +
  theme(legend.position = NULL) +
  theme_bw()


network = as_sfnetwork(exploded_shp) %>%
  activate(nodes) 
 
largest_network = convert(network, to_largest_component) 
largest_network_dta = as.data.frame(largest_network)

results = st_network_paths(largest_network, 1, 15)

