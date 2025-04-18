# ------------------------------------------------------------------------------
#
# This script takes all centroids and compute the distance to the network
#
# ------------------------------------------------------------------------------

rm(list=ls())

pacman::p_load(tidyverse,ggplot2,sf)

path_folder = "/Users/mmoglia/Dropbox/research/unil/afriroads"
path_folder = "C:/Users/mateomoglia/Dropbox/research/unil/afriroads"

# Open the network and the centroids -------------------------------------------

zone = c("CS","NE","NW","SA")

roads_CS = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[1],"_polyline.shp"))
roads_NE = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[2],"_polyline.shp"))
roads_NW = read_sf(paste0(path_folder,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[3],"_polyline.shp"))

roads = bind_rows(roads_CS,roads_NE) %>% bind_rows(roads_NW)

# Highlight the roads that changed types ---------------------------------------

roads_long = roads %>%
  st_drop_geometry() %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols=starts_with("R"),
               values_to = "type",
               names_to = "year", 
               names_prefix = "R") %>%
  arrange(id,year) %>%
  group_by(id) %>%
  mutate(FirstNonMissingYear = min(year[!is.na(type)], na.rm = TRUE)) %>%
  mutate(type = ifelse(is.na(type) & year <= FirstNonMissingYear, 0, type)) %>%
  select(-FirstNonMissingYear) %>%
  mutate(change = ifelse(max(type,na.rm=T) == mean(type,na.rm=T),0,1)) %>%
  ungroup() 

# How many road segments experience a change in type ever?
roads_long %>%
  group_by(id) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(change) %>%
  summarize(count = sum(n()))
# Around 17836 out of 59003 roads changed type

# What is the year distribution of changes
roads_long %>%
  group_by(id) %>%
  filter(type != lag(type)) %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(count = sum(n()),
            length = sum(LENGTH_KM)) %>%
  ggplot(aes(x=year,y=count)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45,vjust=0.5))
# Big change in the early 1970s, a bit in the 1980s but few changes in the 1990s and after
# Interpretation is the same in count or when summing the length

# What is the change per decade per type
roads_long_change_decade = roads_long %>%
  group_by(id) %>%
  mutate(lag_type = lag(type)) %>%
  filter(type != lag(type)) %>%
  ungroup() %>%
  mutate(decade = ceiling(as.numeric(year)/10)*10) %>%
  mutate(change = paste0(lag_type,"-",type)) %>%
  group_by(decade,change) %>%
  summarize(count = sum(n()),
            length = sum(LENGTH_KM)) 

ggplot(data = roads_long_change_decade %>% filter(decade > 1970), aes(x=decade,y=length,fill=change)) +
  geom_col() 
