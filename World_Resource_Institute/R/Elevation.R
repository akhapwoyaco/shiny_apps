library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)
#
setwd('Elevation/')
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
# Elevation
# dem_250m.zip Kenya's digital elevation model at 250-meter resolution.

# hillshd_90m.zip Kenya's shaded relief surface at 90-meter resolution.

# hillshd_250m.zip Kenya's shaded relief surface at 250-meter resolution.

# ke_srtm.zip Kenya's digital elevation model at 90-meter resolution.

