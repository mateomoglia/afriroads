#___________________________________________
# 
# Open the RP of Jedwab and Storeygard (2022)
# and the 
# 
# Mateo Moglia
#
#___________________________________________

rm(list=ls())

pacman::p_load(dplyr,tidyr,stringr,tibble,data.table)
pacman::p_load(ggplot2,viridis,grid)
pacman::p_load(foreign,haven)
pacman::p_load(sf,rmapshaper)
pacman::p_load(DescTools)

path = "/Users/mmoglia/Dropbox/research/unil/transafrican"

zone = c("CS","NE","NW","SA")
z = zone[1]

  # 0. Open Africa shp

afrihighways = read_sf(paste0(path,"/raw/afrilearnr/trans-african-highway.kml"))
africountries = read_sf(paste0(path,"/raw/afrilearnr/africountries.shp"))

  # I. Open the .shp -----

      # A. TAH

tah_CS = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[1],"_remi_polyline.shp")) %>%
    ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_NE = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[2],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_NW = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[3],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
tah_SA = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/tah/roads_",zone[4],"_remi_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 

tah = bind_rows(tah_CS,tah_NE) %>% bind_rows(tah_NW) %>% bind_rows(tah_SA) %>%
  rowwise() %>% mutate(R_current = max(R2012,R2014,na.rm=T))

rm(tah_CS,tah_NE,tah_NW,tah_SA)

    # B. All roads with year

roads_CS = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[1],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
roads_NE = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[2],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 
roads_NW = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_",zone[3],"_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) 

roads = bind_rows(roads_CS,roads_NE) %>% bind_rows(roads_NW)

rm(roads_CS,roads_NE,roads_NW)

    # C. All roads from UDEP

roads_2000 = read_sf(paste0(path,"/raw/JedwabStoreygardJEEA/digitalized/roads_polyline.shp")) %>%
  ms_simplify(keep = 0.1, keep_shapes = FALSE) ## UDEP
  
  # II. Map -----
dev.off()
p = ggplot() +
  geom_sf(data = africountries, colour = "black", fill = "white", size = 0.01) +
  # geom_sf(data = tah %>% filter(RFUTURE == 10), colour="red", size=0.1, linetype = "11") +
  geom_sf(data = afrihighways, colour="indianred", size=0.1) +
  geom_sf(data = roads_2000, colour="black", size=0.1, lwd = 0.05) +
  theme_bw() +
  labs(title = "Road Network in Africa",
       subtitle = "Data: Jedwab and Storeygard (2022); Map: Matéo Moglia") + 
  theme(
    axis.text.x = element_text(family = "Times New Roman", size = 12),
    axis.text.y = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 30, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(family = "Times New Roman", size = 12, hjust = 0.5, colour = "gray25")
  )

legend <- grobTree(
  rectGrob(x = 0.45, y = 0.65, width = 0.9, height=0.3,gp = gpar(fill = "white", col = "black", lwd=0)),
  linesGrob(x = c(0.1, 0.25), y = c(0.7, 0.7), gp = gpar(col = "black", lwd = 1)),
  textGrob("All roads", x = 0.3, y = 0.7, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "Times New Roman")),
  linesGrob(x = c(0.1, 0.25), y = c(0.6, 0.6), gp = gpar(col = "indianred", lwd = 2)),
  textGrob("Transafrican Highway", x = 0.3, y = 0.6, hjust = 0, gp = gpar(fontsize = 14, fontfamily = "Times New Roman"))
)

png(paste0(path,"/output/m_available_roads_hd.png"), width = 12, height = 8, units = "in", res = 700)

grid.newpage()
grid.draw(p)
vp <- viewport(x = 0.2, y = -0.15, width = 0.3, height = 0.5, just = c("left", "bottom"))
pushViewport(vp)
grid.draw(legend)
popViewport()
dev.off()

ggsave(paste0(path,"/output/m_available_roads.pdf"))
ggsave(paste0(path,"/output/m_available_roads_hd.png"), dpi = 700, width = 21, height = 29.7, units = "cm")

