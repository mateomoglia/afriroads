#-------------------------------------------------------------------------------
# 
#   MASTER DO-FILE "afriroads"
#   
#   Mateo Moglia
#   mateo.moglia@gmail.com
#
#-------------------------------------------------------------------------------

# Install the packages ---------------------------------------------------------

    # Africa shapes
remotes::install_github("afrimapr/afrilearndata")
library(afrilearndata)

    # Data manipulation
pacman::p_load(tidyverse,dplyr)

    # Dataviz
pacman::p_load(ggplot2)

    # Spatial and network analysis
pacman::p_load(sf,terra,igraph)

# Set the project paths --------------------------------------------------------

path = "/Users/mmoglia/Dropbox/research/unil/afriroads"

# Prepare the data -------------------------------------------------------------

source(paste0(path, "/0_data_processing.R"))
