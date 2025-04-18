# ------------------------------------------------------------------------------
#
# This script extracts all points with attribute "CITY" in OSM for African countries
#
# ------------------------------------------------------------------------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))

    # I. Extract all countries ID ----------------------------------------------
    
    query_country_id = '
    [out:csv(::id,"ISO3166-1:alpha3")];
    (
      area["ISO3166-1"="DZ"];
      area["ISO3166-1"="AO"];
      area["ISO3166-1"="BJ"];
      area["ISO3166-1"="BW"];
      area["ISO3166-1"="BF"];
      area["ISO3166-1"="BI"];
      area["ISO3166-1"="CM"];
      area["ISO3166-1"="CV"];
      area["ISO3166-1"="CF"];
      area["ISO3166-1"="TD"];
      area["ISO3166-1"="KM"];
      area["ISO3166-1"="CD"];
      area["ISO3166-1"="CG"];
      area["ISO3166-1"="DJ"];
      area["ISO3166-1"="EG"];
      area["ISO3166-1"="GQ"];
      area["ISO3166-1"="ER"];
      area["ISO3166-1"="SZ"];
      area["ISO3166-1"="ET"];
      area["ISO3166-1"="GA"];
      area["ISO3166-1"="GM"];
      area["ISO3166-1"="GH"];
      area["ISO3166-1"="GN"];
      area["ISO3166-1"="GW"];
      area["ISO3166-1"="KE"];
      area["ISO3166-1"="LS"];
      area["ISO3166-1"="LR"];
      area["ISO3166-1"="LY"];
      area["ISO3166-1"="MA"];
      area["ISO3166-1"="MG"];
      area["ISO3166-1"="MW"];
      area["ISO3166-1"="ML"];
      area["ISO3166-1"="MR"];
      area["ISO3166-1"="MU"];
      area["ISO3166-1"="YT"];
      area["ISO3166-1"="NA"];
      area["ISO3166-1"="NE"];
      area["ISO3166-1"="NG"];
      area["ISO3166-1"="RW"];
      area["ISO3166-1"="ST"];
      area["ISO3166-1"="SN"];
      area["ISO3166-1"="SC"];
      area["ISO3166-1"="SL"];
      area["ISO3166-1"="SO"];
      area["ISO3166-1"="ZA"];
      area["ISO3166-1"="SS"];
      area["ISO3166-1"="SD"];
      area["ISO3166-1"="TZ"];
      area["ISO3166-1"="TG"];
      area["ISO3166-1"="TN"];
      area["ISO3166-1"="UG"];
      area["ISO3166-1"="ZM"];
      area["ISO3166-1"="ZW"];
    );
    out;
    '
      
    sopq = overpass_query(query_country_id)
      
    africa_id = read.table(text = sopq, sep="\t", header=TRUE, 
               check.names=FALSE, stringsAsFactors=FALSE)
    africa_id = c(africa_id$`@id`)
    
    # II. Extract all cities in all countries in OSM ---------------------------
    
    cities = data.frame()
    for(x in africa_id){
      query_city <- paste0('[out:csv(::lat,::lon,"population")];
      area(',x,')->.a;
      (nwr["place"="city"](area.a); );
      out;
      ')
    opq = overpass_query(query_city)
    
    temp = read.table(text = opq, sep="\t", header=TRUE,
               check.names=FALSE, stringsAsFactors=FALSE)
    temp = temp %>%
      filter(population != "0" | !is.na(population)) %>%
      filter(population!="") %>%
      filter(!is.na(`@lat`)) %>%
      filter(!is.na(`@lon`)) %>%
      mutate(pop = gsub(" ","",population)) %>%
      mutate(pop = as.numeric(pop)) %>%
      dplyr::select(-population)
    names(temp) = c("lat","lon","pop")
    cities = rbind(cities,temp)
    }
    
    cities_sf = st_as_sf(cities,coords=c("lon","lat"),crs=4326) %>%
      dplyr::select(pop,geometry) %>% 
      mutate(city_id = row_number())
    nrow(cities_sf)
    # We extracted 797 cities in OSM with a population greater than 0 (and non missing)
    
    # III. Which ethnic groups are covered by at least one of these cities -----
    
    cross_cities_ethnies = st_intersects(cities_sf,ethnies) %>%
      as.data.frame()
    names(cross_cities_ethnies) = c("city_id","ethnic_id")  
    cross_cities_ethnies = cross_cities_ethnies %>%
      left_join(ethnies %>% as.data.frame() %>% dplyr::select(-geometry,-length)) %>%
      left_join(cities_sf) %>%
      st_as_sf()
    
    write_sf(cross_cities_ethnies,paste0(path_folder,"/temp/cities/osm_cities.shp"))
    
    # How many ethnies are covered by a city?
    covered_cities = cross_cities_ethnies %>%
      as.data.frame() %>%
      group_by(ethnic_id) %>%
      summarize(count = sum(n())) %>%
      ungroup()
    
    nrow(covered_cities)  # 261 ethnic groups are covered
    sum(covered_cities$count) # 547 cities are in an ethnic group 
    mean(covered_cities$count) # On avg. 2 cities per group
    
    rm(list = setdiff(ls(), "path_folder"))
    