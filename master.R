#-------------------------------------------------------------------------------
# 
#   MASTER DO-FILE "afriroads"
#   
#   Mateo Moglia
#   mateo.moglia@gmail.com
#
#-------------------------------------------------------------------------------

    rm(list=ls())
  
    # Install the packages -----------------------------------------------------
    # --------------------------------------------------------------------------

        # Africa shapes
    # remotes::install_github("afrimapr/afrilearndata")
    library(afrilearndata)
    
        # Scrapping
    pacman::p_load(overpass)
    
        # Dataviz
    pacman::p_load(ggplot2)
    
        # Spatial and network analysis
    pacman::p_load(sf,terra,igraph,rmapshaper,units,raster,flexclust,lwgeom)
    library(tidygraph)
    
        # Data manipulation
    # install.packages("data.table")
    pacman::p_load(tidyr,dplyr,stringr,tibble,foreign,haven)
    library(dplyr,warn.conflicts = F)
    
    # Set the project paths ----------------------------------------------------
    
    path_folder = "C:/users/mateomoglia/Dropbox/research/unil/afriroads"
    
    # Clean the temp folder ----------------------------------------------------
    
    # unlink(file.path(path_folder, "temp"), recursive = TRUE)
    #   dir.create(file.path(path_folder, "temp"))
    #   dir.create(file.path(paste0(path_folder,"/temp"),"cities"))
    #   dir.create(file.path(paste0(path_folder,"/temp"),"centroids"))
    #   dir.create(file.path(paste0(path_folder,"/temp"),"shapefiles"))
    #   dir.create(file.path(paste0(path_folder,"/temp"),"distances"))
      
    # Prepare the data ---------------------------------------------------------
    
    source(paste0(path_folder, "/code/0_data_processing/1_open_road_network.R"))
    source(paste0(path_folder, "/code/0_data_processing/2_covered_murdock_groups.R"))
    
    # Build the network --------------------------------------------------------
    
    source(paste0(path_folder, "/code/1_build_network/1_make_graph.R"))
    
    # Define the points --------------------------------------------------------
    
    source(paste0(path_folder, "/code/2_define_points/1_geographic_centroids.R"))
    source(paste0(path_folder, "/code/2_define_points/2_population_centroids.R"))
    source(paste0(path_folder, "/code/2_define_points/3_kmean_centroids.R"))
    source(paste0(path_folder, "/code/2_define_points/4_africapolis.R"))
    source(paste0(path_folder, "/code/2_define_points/5_osm.R"))
    
    # Compute distance and time ------------------------------------------------
    
    source(paste0(path_folder, "/code/4_compute_distance/1_add_the_points.R"))
    source(paste0(path_folder, "/code/4_compute_distance/2_connect_the_graph.R"))
    source(paste0(path_folder, "/code/4_compute_distance/3_compute_internal_time.R"))
    
    # Open 4_compute_bilateral_distance_matrix.R to choose the counterfactuals
    # Three choices: increase in the speed of all road type
    #                achievement of the TAH
    #                increase in the specific path
    