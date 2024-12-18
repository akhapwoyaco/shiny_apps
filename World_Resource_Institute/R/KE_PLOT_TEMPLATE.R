#
library(sf)
#
kenya_county <- read_sf(
  'D:/DATAPROJECTS/shiny_apps/World_Resource_Institute/Kenya_Data/kenya county boundaries/kenya county boundaries/Kenya_County_Boundaries.shp'
)
# lakes
ken_lakes <- st_read('D:/DATAPROJECTS/shiny_apps/World_Resource_Institute/Kenya_Data/kenlakes/KEN_Lakes/KEN_Lakes.shp')
# rivers
ken_rivers <- st_read('D:/DATAPROJECTS/shiny_apps/World_Resource_Institute/Kenya_Data/kenrivers/KEN_Rivers/KEN_Rivers.shp')
#
# KE plots
#
ke_plot <- ggplot() + 
  geom_sf(data = kenya_county$geometry, color = "grey"
  ) +
  #geom_sf(data = ke_waterbodies, fill = 'blue', color = 'blue')
  geom_sf(data = ken_lakes, fill = 'blue') 
#
theme_template <- theme(
  axis.title = element_blank(),
  panel.grid.major = element_blank(),
  axis.text = element_blank(),
  # element_line(
  # color = gray(0.5), linetype = "dashed", 
  # size = 0.5
  # ), 
  axis.ticks = element_blank(),
  plot.title = element_text(hjust = 0.5),
  legend.position = c(1.125,0.6),
  legend.background = element_blank(), 
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text=element_text(size = 8),
  panel.background = element_rect(fill = "white"))
#


#
# ggplot() + 
#   #geom_sf(data = kenya_county$geometry, color = "grey"
#   #) +
#   geom_sf(data = ke_waterbodies, fill = 'blue', color = 'blue')
#
# 
# library(ggspatial)
# library(scales)
# 
# #
# ke_agriculture$AGRICULTUR <- gsub(pattern = '\\) ', replacement = '\\) \n', x = ke_agriculture$AGRICULTUR)
# #
# ggplot() + 
#   geom_sf(data = kenya_county$geometry, color = "grey"
#           ) +
#   geom_sf(data = ken_lakes, fill = 'blue') +
#   geom_sf(data = ken_rivers, fill = 'blue', color = 'blue') +
#   geom_sf(data = ke_agriculture,  
#           aes(fill = AGRICULTUR)) + 
#   ggtitle("KE Agriculture") + 
#   coord_sf(datum = NA) + 
# #  geom_label(data = kenya_county, aes(geometry = geometry, label = COUNTY), 
#  #             size = 5, fontface = "bold") +
#   scale_fill_viridis_d(option = 'A') +
#   coord_sf(expand = FALSE) +
#   theme(
#     axis.title = element_blank(),
#     panel.grid.major = element_blank(),
#     axis.text = element_blank(),
#       # element_line(
#       # color = gray(0.5), linetype = "dashed", 
#       # size = 0.5
#       # ), 
#     legend.position = c(1.125,0.6),
#     legend.background = element_blank(), 
#     legend.key = element_blank(),
#     legend.title = element_blank(),
#     legend.text=element_text(size = 8),
#     panel.background = element_rect(fill = "white"))
# #,
#  #   legend.position = c(0.1,.2))
# #
# # ggplot() + 
# #   geom_sf(data = kenya_county$geometry, color = "blue") +
# #   geom_sf(data = ke_agriculture, size = 1.5, 
# #           color = "black", fill = "cyan1") + 
# #   ggtitle("KE Agriculture") + 
# #   coord_sf(datum = NA) + 
# #   theme(panel.grid.major = element_line(colour = "transparent"))
# #
#   
# # gj = ("C:\\Users\\Administrator\\Downloads\\africawaterbody.geojson")
# # gj
# # #gj is a path in the file system. Now read it to an sf spatial data frame:
# # gjsf = st_read(gj)
# # ggplot() +
# #   geom_sf(data = gjsf$geometry)
# #
# 
# 
# 
# ggplot() +
#   geom_sf(data = ken_all) +
#  


#