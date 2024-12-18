library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)
# https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
setwd('..')
setwd('Tourism/')
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
# Tourism
# ke_hotel-beds_coast.zip Capacity, or number of beds, for major tourist hotels on the eastern coast of Kenya.
ke_plot + 
  geom_sf(data = ke_hotel_beds_coast, aes(color = SUM_BEDS)) +
  theme_template

# ke_hotels-lodges-campsites.zip Tourist accommodations in Kenya.
ke_plot + 
  geom_sf(data = ke_hotels_lodges_campsites, aes(color = NAME)) +
  theme_template
#