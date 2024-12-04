#___________________________________________
# 
# Open the RP of Jedwab and Storeygard (2022)
# 
# Mateo Moglia
#
#___________________________________________

pacman::p_load(dplyr,tidyr,stringr,tibble,data.table)
pacman::p_load(ggplot2,viridis,grid)
pacman::p_load(foreign,haven)
pacman::p_load(sf,rmapshaper)
pacman::p_load(DescTools)

path = "/Users/mmoglia/Dropbox/research/unil/transafrican"

zone = c("CS","NE","NW","SA")
z = zone[1]

  # 0. Open ethnic groups shp -----

ethnies.shp = read_sf(paste0(path,"/raw/ethnies/ethnies_murdock_encoded_no_island_country.shp")) %>%
  ms_simplify(keep = 0.01, keep_shapes = FALSE) # WGS84
countries.shp = read_sf(paste0(path,"/raw/afrilearnr/africountries.shp"))
sa.shp = countries.shp %>%
  filter(name_fr == "Lesotho" | name_fr == "Afrique du Sud")

  # I. Open the .shp -----

# A. TAH

tah_CS = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[1],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_NE = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[2],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_NW = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[3],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_SA = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[4],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 

tah = bind_rows(tah_CS,tah_NE) %>% bind_rows(tah_NW) %>% bind_rows(tah_SA) 

rm(tah_CS,tah_NE,tah_NW,tah_SA)

# B. All roads with year

roads_CS = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[1],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
roads_NE = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[2],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
roads_NW = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[3],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 

roads = bind_rows(roads_CS,roads_NE) %>% bind_rows(roads_NW)

rm(roads_CS,roads_NE,roads_NW)

  # II. Export the .shp for the latest year available -----

current_roads.shp = tah %>%
  rowwise() %>%
  mutate(R_current = max(R2012,R2014,na.rm=T)) %>%
  mutate(TAH = ifelse(R_current == 8, 1, 0)) %>%
  select(R_current,TAH,geometry) %>%
  ungroup() %>%
  st_transform(4326)
write_sf(current_roads.shp,paste0(path,"/temp/shapefiles/current_roads_network.shp"))

future_roads.shp = tah %>%
  rowwise() %>%
  mutate(R_future = RFUTURE) %>%
  mutate(TAH = ifelse(RFUTURE == 10, 1, 0)) %>%
  select(R_future,TAH,geometry) %>%
  ungroup() %>%
  st_transform(4326)
write_sf(current_roads.shp,paste0(path,"/temp/shapefiles/future_roads_network.shp"))

# Make sub_shp for South Africa only
current_sa_roads.shp = st_intersection(current_roads.shp,sa.shp) %>%
  dplyr::select(geometry,R_current,TAH) 
current_sa_roads.shp = st_collection_extract(current_sa_roads.shp, "LINE")
write_sf(current_sa_roads.shp,paste0(path,"/temp/shapefiles/current_sa_roads_network.shp")) 

future_sa_roads.shp = st_intersection(future_roads.shp,sa.shp) %>%
  select(geometry,R_future,TAH)
future_sa_roads.shp = st_collection_extract(future_sa_roads.shp, "LINE")
write_sf(future_sa_roads.shp,paste0(path,"/temp/shapefiles/future_sa_roads_network.shp"))

  # III. Make a map ------

ggplot() +
  geom_sf(data=ethnies.shp, colour = "black", fill = "white", size = 0.01, lwd=0.025) +
  geom_sf(data=current_roads.shp %>% filter(TAH == 0), colour="blue", size=0.15, lwd = 0.05) +
  geom_sf(data=current_roads.shp %>% filter(TAH == 1), colour="indianred", size=0.35, lwd = 0.35) +
  geom_sf(data=future_roads.shp %>% filter(TAH == 1), colour="indianred", size=0.35, lwd = 0.35) +
  theme_bw()
ggsave(paste0(path,"/output/map/m_road_network.pdf"))
