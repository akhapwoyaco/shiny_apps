library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)

setwd('Base_Data/')
list.files(pattern = '*.shp$', recursive = TRUE)
#
#
for (dir_F in list.files(pattern = '*.shp$', recursive = TRUE)){
  dir_split <- strsplit(x = dir_F, split = '/')[[1]]
  file_index <- grep(x = dir_F, pattern = '.shp$')
  
  obj_name <- gsub(pattern = '-', replacement = '_', x = dir_split)[1]
  print(obj_name)
  # print(dir_F)
  # print(dir_split[file_index])
  eval(
    call(
      "<-", as.name(obj_name),
      st_read(
        dir_F
      )
    ))
}


#
ke_plot <- ggplot() + 
  geom_sf(data = kenya_county$geometry, color = "grey"
  ) +
  geom_sf(data = ken_lakes, fill = 'blue') #+
#geom_sf(data = ken_rivers, fill = 'blue', color = 'blue')
theme_template <- theme(
  axis.title = element_blank(),
  panel.grid.major = element_blank(),
  axis.text = element_blank(),
  # element_line(
  # color = gray(0.5), linetype = "dashed", 
  # size = 0.5
  # ), 
  legend.position = 'inside',
  legend.position.inside = c(1.125,0.6),
  legend.background = element_blank(), 
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text=element_text(size = 8),
  panel.background = element_rect(fill = "white"))

# ke_district_boundaries.zip District administrative boundaries in Kenya
ke_district_boundaries_plot = ke_plot + 
  geom_sf(data = ke_district_boundaries) + 
  theme() + theme_void()
ke_district_boundaries_plot
ggsave(
  plot = ke_district_boundaries_plot, 
  filename = "ke_district_boundaries_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)


# ke_major-rivers.zip Permanent and non-permanent rivers in Kenya
ke_major_rivers_plot = ke_plot + 
  geom_sf(data = ke_major_rivers) + 
  theme() +theme_void()
ke_major_rivers_plot
ggsave(
  plot = ke_major_rivers_plot, 
  filename = "ke_major_rivers_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)


# ke_key-airfields.zip Major airfields in Kenya.
ke_key_airfields_plot = ke_plot + 
  geom_sf(data = ke_key_airfields, aes(color = TYPE)) + 
  theme() +
  theme_void()
ke_key_airfields_plot
ggsave(
  plot = ke_key_airfields_plot, 
  filename = "ke_key_airfields_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)

# e_major-roads.zip Major roads in Kenya.
ke_major_roads_plot = ke_plot + 
  geom_sf(data = ke_major_roads) + 
  theme() +
  theme_void()
ke_major_roads_plot
ggsave(
  plot = ke_major_roads_plot, 
  filename = "ke_major_roads_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)

# ke_major-towns.zip Major towns in Kenya.
ke_major_towns_plot = ke_plot + 
  geom_sf(data = ke_major_towns[-55,], aes(label = TOWN_NAME)) +
  geom_sf_text(data = ke_major_towns[-55,], aes(label = TOWN_NAME), 
               check_overlap = !T, hjust = 0.2) +
  theme_classic() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
ke_major_towns_plot
ggsave(
  plot = ke_major_towns_plot, 
  filename = "ke_major_towns_plot.png", 
  width = 35, height = 35, units = "cm", dpi = 650)

#

# ke_protected-areas.zip Protected areas in Kenya.
ke_protected_areas_plot = ke_plot + 
  geom_sf(data = ke_protected_areas) + 
  theme()+
  theme_classic()
ke_protected_areas_plot
ggsave(
  plot = ke_protected_areas_plot, 
  filename = "ke_protected_areas_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)


# ke_waterbodies.zip Water bodies in Kenya
ke_waterbodies_plot = ke_plot +
  geom_sf(data = ke_waterbodies |> st_set_crs(4326),
          fill = 'blue', color = 'blue') + 
  theme() +
  theme_void()
ke_waterbodies_plot
ggsave(
  plot = ke_waterbodies_plot, 
  filename = "ke_waterbodies_plot.png", 
  width = 30, height = 30, units = "cm", dpi = 450)
#
