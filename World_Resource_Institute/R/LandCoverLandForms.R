library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)

setwd('..')
setwd('Land_Cover_And_Land_Form/')
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

# Land cover & land form
# ke_bareareas Bare areas (areas naturally devoid of vegetation) in Kenya.
ke_plot +
  geom_sf(data = ke_bareareas)

# ke_coral_reefs Coral reefs on the eastern coast of Kenya.
ke_plot +
  geom_sf(data = ke_coral_reefs)

# ke_floodplains Floodplains and valley bottoms in Kenya.
ke_plot +
  geom_sf(data = ke_floodplains)

# ke_forests Forest types in Kenya.
ke_plot +
  geom_sf(data = ke_forests)

# ke_mangroves Mangroves on the eastern coast of Kenya as polygons.
ke_plot +
  geom_sf(data = ke_mangroves)

# ke_mangroves_undp-line Mangroves on the eastern coast of Kenya as lines.
ke_plot +
  geom_sf(data = ke_mangroves_undp_line)

# ke_modis Percent treecover in Kenya.
#'TODO 
# ke_plot +
#   geom_sf(data = )

# ke_rangeland Areas of savanna and grassland in Kenya.
ke_plot +
  geom_sf(data = ke_rangeland)

# ke_sand_beaches Sand beaches in Kenya.
ke_plot +
  geom_sf(data = ke_sand_beaches)

# ke_urban Urban areas in Kenya.
ke_plot +
  geom_sf(data = ke_urban)

# ke_wetlands Wetlands areas in Kenya.
ke_plot +
  geom_sf(data = ke_wetlands)
#
