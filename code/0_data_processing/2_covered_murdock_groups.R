#-------------------------------------------------------------------------------
#
# This script opens the .shp for Murdock ethnic groups (Nunn, 2004). 
# 
# The output is a shapefile for covered ethnic groups
# A covered ethnic group is an ethnic group that is covered by the road network
#
#-------------------------------------------------------------------------------

    # I. Open the shapefile for Murdock ethnic groups --------------------------
    
    ethnies.shp = read_sf(paste0(path_folder,"/raw/ethnies/ethnies_murdock_encoded_no_island_country.shp")) %>%
      ms_simplify(keep = 0.01, keep_shapes = FALSE) %>%
      st_transform(crs=4326) %>%
      dplyr::select(NAME) %>%
      arrange(NAME) %>%
      mutate(ethnic_id = row_number())
    
    # II. Open the road network ----------------------------------------------------
    
    roads.shp = read_sf(paste0(path_folder,"/temp/shapefiles/current_roads_network.shp")) %>%
      mutate(road_id = row_number())
    
    # III. Extract the ethnic groups that are covered by the network -----------
    
    covered = st_crosses(ethnies.shp,roads.shp)
    covered.dta = as.data.frame(covered)
    names(covered.dta) = c("ethnic_id","road_id")
    covered.dta = inner_join(covered.dta,roads.shp) %>%
      mutate(length = as.numeric(st_length(geometry))/1000) %>%
      group_by(ethnic_id) %>%
      summarize(length = sum(length)) %>%
      ungroup()
    covered.shp = ethnies.shp %>% filter(ethnic_id %in% covered.dta$ethnic_id) %>%
      left_join(covered.dta %>% dplyr::select(ethnic_id,length))
    
    write_sf(covered.shp,paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))

    rm(list = setdiff(ls(), "path_folder"))