# ------------------------------------------------------------------------------
#
# This script take the Murdock shapefiles and extract the Africapolis cities
# for each ethnic group.
#
# ------------------------------------------------------------------------------

    # I. Open the shapefile for -covered- Murdock ethnic groups ----------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    
    # II. Open Africapolis data for Africa -------------------------------------
    
    africapolis = readxl::read_xlsx(paste0(path_folder,"/raw/Africapolis/Africapolis_agglomeration_2024.xlsx"),skip=15) %>%
      dplyr::select(Longitude,Latitude,Population_2015) %>%
      dplyr::filter(Population_2015 > 0) %>%
      rename(pop = Population_2015) %>%
      mutate(city_id = row_number())
    cities_sf = africapolis %>% 
      st_as_sf(coords = c("Longitude","Latitude"),crs=4326)
      
    
    # III. Extract the cities that are on Murdock group ------------------------
  
    cross_cities_ethnies = st_intersects(cities_sf,ethnies) %>%
      as.data.frame()
    names(cross_cities_ethnies) = c("city_id","ethnic_id")  
    cross_cities_ethnies = cross_cities_ethnies %>%
      left_join(ethnies %>% as.data.frame() %>% dplyr::select(-geometry,-length)) %>%
      left_join(cities_sf) %>%
      st_as_sf()
    
    write_sf(cross_cities_ethnies,paste0(path_folder,"/temp/cities/cities_africapolis.shp"))
    
    # How many ethnies are covered by a city?
    covered_cities = cross_cities_ethnies %>%
      as.data.frame() %>%
      group_by(ethnic_id) %>%
      summarize(count = sum(n())) %>%
      ungroup()
    
    nrow(covered_cities)  # 590 ethnic groups are covered
    sum(covered_cities$count) # 4903 cities are in an ethnic group 
    mean(covered_cities$count) # On avg. 8 cities per group
    
    rm(list = setdiff(ls(), "path_folder"))