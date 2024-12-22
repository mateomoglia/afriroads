#-------------------------------------------------------------------------------
#
# This script opens the population grid cell data from WorldPop and extracts 
# the relevant data for covered ethnic groups.
#
#------------------------------------------

pacman::p_load(ggplot2,sf,rmapshaper,sp)

path = "/Users/mmoglia/Dropbox/research/unil/transafrican"

# I. Open the raster data ------------------------------------------------------

ethnies.shp = read_sf(paste0(path,"/raw/ethnies/ethnies_murdock_encoded_no_island_country.shp")) %>%
  ms_simplify(keep = 0.01, keep_shapes = FALSE) # WGS84

  # II. Make a map -----

ggplot(ethnies.shp) +
  geom_sf(fill="wheat") +
  theme_bw()
ggsave(paste0(path,"/output/map/m_ethnies_murdock.pdf"))

  # III. Compute the centroids ------

ethnies.centroids = ethnies.shp %>%
  select(NAME,geometry) %>%
  st_centroid() %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  as.data.frame() %>%
  select(-geometry)
write.csv2(ethnies.centroids,paste0(path,"/temp/centroids/centroids_ethnies_murdock.csv"),row.names = F)

