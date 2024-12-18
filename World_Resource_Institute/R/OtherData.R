library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)
# https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
setwd('..')
setwd('Other_Data/')
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
# dist_water.zip Distance to water points (boreholes, etc) in northern Kenya.
# ke_plot + 
#   geom_sf(data = ) +
#   theme_template
# ke_biomass_totalsqkm.zip Annual growth of biomass outside of croplands and the theoretically harvestable biomass yield outside of croplands in Kenya.
ke_plot + 
  geom_sf(data = ke_biomass_totalsqkm) +
  theme_template

# ke_charcoal-sources.zip Sources of wood for charcoal in selected administrative Districts of Kenya in 2004.
ke_plot + 
  geom_sf(data = ke_charcoal_sources) +
  theme_template

# ke_market_centers.zip Market centers in western Kenya.
ke_plot + 
  geom_sf(data = ke_market_centers) +
  theme_template
#