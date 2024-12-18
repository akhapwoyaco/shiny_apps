library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)

# https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
setwd('..')
setwd('Water_Irrigation_And_hydropower/')
list.files(pattern = '*.shp$', recursive = TRUE)
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
# Water, irrigation, & hydropower
# ke_hydropower-dams Large hydropower dams in Kenya.
ke_plot + 
  geom_sf(data = ke_hydropower_dams) +
  theme_template
# ke_micro-hydro Micro-hydropower sites in Kenya.
ke_plot + 
  geom_sf(data = ke_micro_hydro) +
  theme_template
# ke_micro-hydro_proposed Proposed micro-hydropower sites in Kenya.
ke_plot + 
  geom_sf(data = ke_micro_hydro_proposed) +
  theme_template
# ke_mombasa_dams Water supplies serving Mombasa, Kenya.
ke_plot + 
  geom_sf(data = ke_mombasa_dams) +
  theme_template
# ke_nairobi-dams Water supplies serving Nairobi, Kenya.
ke_plot + 
  geom_sf(data = ke_nairobi_dams) +
  theme_template
# ke_proposed-irrigation Large-scale irrigation schemes in Kenya.
ke_plot + 
  geom_sf(data = ke_proposed_irrigation) +
  theme_template
# ke_small-scale_irrigation Existing small-scale irrigation and drainage points in Kenya.
ke_plot + 
  geom_sf(data = ke_small_scale_irrigation) +
  theme_template
# ke_water-balance Annual projected water balance by subdrainage area in Kenya for 2000 and 2010.
ke_plot + 
  geom_sf(data = ke_water_balance) +
  theme_template
# ke_water_basins River basins and sub-basins (catchments and sub-catchments) in Kenya.
ke_plot + 
  geom_sf(data = ke_water_basins) +
  theme_template
# ke_water-consumption_wildlife-livestock Average water consumption of livestock and wildlife by sub-basin in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_water_consumption_wildlife_livestock) +
  theme_template
