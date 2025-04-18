#-------------------------------------------------------------------------------
# 
# Open the replication package of Jedwab and Storeygard (JEEA, 2022)
# 
# Mateo Moglia
#
#-------------------------------------------------------------------------------
    
    zone = c("CS","NE","NW","SA")
    
      # 0. Open ethnic groups shp ----------------------------------------------
    
    ethnies.shp = read_sf(paste0(path_folder,"/raw/ethnies/ethnies_murdock_encoded_no_island_country.shp")) %>%
      ms_simplify(keep = 0.01, keep_shapes = FALSE) # WGS84
    countries.shp = read_sf(paste0(path_folder,"/raw/afrilearnr/africountries.shp"))

      # I. Open the .shp -------------------------------------------------------
    
          # A. TAH
    
    tah_CS = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[1],"_remi_polyline.shp")) %>%
      ms_simplify(keep = 0.1, keep_shapes = FALSE) 
    tah_NE = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[2],"_remi_polyline.shp")) %>%
      ms_simplify(keep = 0.1, keep_shapes = FALSE) 
    tah_NW = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[3],"_remi_polyline.shp")) %>%
      ms_simplify(keep = 0.1, keep_shapes = FALSE) 
    tah_SA = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[4],"_remi_polyline.shp")) %>%
      ms_simplify(keep = 0.1, keep_shapes = FALSE) 
    
    tah = bind_rows(tah_CS,tah_NE) %>% bind_rows(tah_NW) %>% bind_rows(tah_SA) 
    
    rm(tah_CS,tah_NE,tah_NW,tah_SA)
    
      # II. Export the .shp for the latest year available ----------------------
    
    current_roads.shp = tah %>%
      rowwise() %>%
      mutate(R_current = max(R2012,R2014,0,na.rm=T)) %>%
      mutate(R_future = RFUTURE) %>%
      dplyr::select(R_current,R_future,geometry) %>%
      ungroup() %>%
      st_transform(4326)
    write_sf(current_roads.shp,paste0(path_folder,"/temp/shapefiles/current_roads_network.shp"))
    
      # III. Make a map --------------------------------------------------------
    
    ggplot() +
      geom_sf(data=ethnies.shp, colour = "black", fill = "white", size = 0.01, lwd=0.025) +
      geom_sf(data=current_roads.shp %>% filter(R_current != 8), colour="blue", size=0.15, lwd = 0.05) +
      geom_sf(data=current_roads.shp %>% filter(R_future == 10), colour="indianred", size=0.35, lwd = 0.35) +
      theme_bw()
    ggsave(paste0(path_folder,"/output/map/m_road_network.pdf"))

    rm(list = setdiff(ls(), "path_folder"))