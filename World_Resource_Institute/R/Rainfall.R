library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)
library(adfExplorer)
library(terra)
library(ggrepel)
library(tidyterra)
# https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
setwd('..')
setwd('Rainfall')

list.files(pattern = '*x.adf$', recursive = TRUE)
#
for (dir_F in list.files(pattern = '*x.adf$', recursive = TRUE)){
  dir_split <- paste0(
    strsplit(x = dir_F, split = '/')[[1]][3:4], 
    collapse = '_')
  #
  obj_name <- gsub(pattern = '.adf$', replacement = '', x = dir_split)
  print(obj_name)
  eval(
    call(
      "<-", as.name(obj_name),
      terra::rast(
        dir_F
      )
    ))
}
#
#
# dat2 <- terra::rast(
#   'ke_totann/ke_totann/ke_totann/w001001x.adf')

# 
# Rainfall
# ke_totann.zip Average annual rainfall in Kenya.
plot(ke_totann_w001001x+0)

#

ggplot() +
  geom_spatraster(data = ke_totann_w001001x+0) + 
  scale_fill_whitebox_c(
    palette ='deep', n.breaks = 5
  ) + 
  labs(
    fill = '', title = 'Average Annual Rainfall in Kenya'
  ) +
  geom_sf(data = kenya_county, fill = NA)  +
  geom_sf(data = ken_lakes, fill = 'blue') + 
  geom_text_repel(
    data = kenya_county[kenya_county$FID %in% c(1:10),], 
    aes(label = COUNTY, geometry = geometry), 
    stat = 'sf_coordinates', min.segment.length = 0, size = 3.5,
    segment.color = NA
  ) +
  theme_template +
  theme(
    legend.position = c(0.1,0.1),
    legend.spacing.x = unit(0, 'cm'),
    legend.direction = 'horizontal'
  ) 
#
#

# prec_2.zip World's average monthly rainfall in February.
plot(prec_2_w001001x+0)

# prec_11.zip World's average monthly rainfall in November.
plot(prec_11_w001001x+0)

# prec_4.zip World's average monthly rainfall in April.
plot(prec_4_w001001x+0)

# prec_7.zip World's average monthly rainfall in July.
plot(prec_7_w001001x+0)
#
