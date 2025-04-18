# ------------------------------------------------------------------------------
#
# This script take the Murdock shapefiles and extract the
# centroid for each ethnic group.
#
# ------------------------------------------------------------------------------

    # I. Open the shapefile for -covered- Murdock ethnic groups ----------------
    
    ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
    
    # II. Compute the centroid of each ethnic group ----------------------------
    
    centroid = ethnies %>%
      st_centroid() %>%
      dplyr::select(-length)
    
    write_sf(centroid,paste0(path_folder,"/temp/centroids/geographic_centroids.shp"))

    rm(list = setdiff(ls(), "path_folder"))