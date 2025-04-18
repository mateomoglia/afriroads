# ------------------------------------------------------------------------------
#
# This script compares the diverse measures of internal distances
#
# ------------------------------------------------------------------------------

rm(list=ls())

pacman::p_load(tidyverse,ggplot2,sf,exactextractr,raster)

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"

# Open the pop and the centroids -----------------------------------------------

ethnies = read_sf(paste0(path_folder,"/temp/shapefiles/covered_ethnic_groups.shp"))
hm      = data.table::fread(paste0(path_folder,"/temp/distances/head-mayer_internal_distance.csv"))
longest = data.table::fread(paste0(path_folder,"/temp/distances/longest_internal_distance.csv"))
uw      = data.table::fread(paste0(path_folder,"/temp/distances/uw_avg_internal_distance.csv"))
w       = data.table::fread(paste0(path_folder,"/temp/distances/w_avg_internal_distance.csv"))

# Compute the corr coefficient between all -------------------------------------

corr_uw_longest = cor(uw$uw_length,longest$longest_length)
corr_uw_hm = cor(uw$uw_length,hm$internal_hm)


ggplot() +
  geom_point(aes(x = log(longest$longest_length), y = log(uw$uw_length),color="Longest road")) +
  geom_point(aes(x = log(hm$internal_hm), y = log(uw$uw_length),color="Head-Mayer")) +
  scale_color_manual(values=c("Longest road" = "red", "Head-Mayer" = "blue"), name = "Method") +
  xlab("Unweighted road length average (log)") + ylab("Internal distance (log)") +
  theme_bw()

