#
# load library for file import
library(sf)
# load plotting data
library(ggplot2)
#
setwd('SHP_FILES/')
#
# KE Agriculture ----------------------------------------------------------
#
# load data
ke_agriculture <- st_read(
  'ke_agriculture.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_agriculture, size = 1.5, 
          color = "black", fill = "cyan1") + 
  ggtitle("KE Agriculture") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# crop diversity ----------------------------------------------------------
#
# load data
ke_crops_diversity <- st_read(
  'ke_crops_diversity.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_crops_diversity, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Crops Diversity") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE Crops Foodshare ------------------------------------------------------
#
# load data
ke_crops_foodshare <- st_read(
  'ke_crops_foodshare.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_crops_foodshare, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Crops Foodshare") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE Crops Intensity ------------------------------------------------------
#
# load data
ke_crops_intensity <- st_read(
  'ke_crops_intensity.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_crops_intensity, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Crops Intensity") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#
# KE Crops Irrigation -------------------------------------------------
#
# load data
ke_crops_irrig <- st_read(
  'ke_crops_irrig.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_crops_irrig, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Crops Irrigation") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#
#  KE Crops Size ----------------------------------------------------------
#
# load data
ke_crops_size <- st_read(
  'ke_crops_size.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_crops_size, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Crops Size") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE Livestock 1990 -------------------------------------------------------
#
# load data
ke_livestock_1990 <- st_read(
  'ke_livestock_1990.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_livestock_1990, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Livestock 1990") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE Milk-production ------------------------------------------------------
#
# load data
ke_milk_production <- st_read(
  'ke_milk-production.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_milk_production, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Milk Production") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE milk surplus deficit -------------------------------------------------
# load data
ke_milk_surplus_deficit <- st_read(
  'ke_milk-surplus-deficit.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_milk_surplus_deficit, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Milk Surplu Deficit") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#
# KE Pineapple Plantations ------------------------------------------------
# load data
ke_pineapple_plantations <- st_read(
  'ke_pineapple_plantations.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_pineapple_plantations, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Pineapple Plantations") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#
# KE Tree Plantations --------------------------------------------------
# load data
ke_tree_plantations <- st_read(
  'ke_tree-plantations.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_tree_plantations, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Tree Plantations") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

# KE Woodlots in cropland -------------------------------------------------
# load data
ke_woodlots_in_cropland <- st_read(
  'ke_woodlots-in-cropland.shp'
)
# plot
ggplot() + 
  geom_sf(data = ke_woodlots_in_cropland, size = 1.5, 
          color = "black", fill = "cyan1"
  ) + 
  ggtitle("KE Woodlots in Cropland") + 
  coord_sf(datum = NA) + 
  theme(panel.grid.major = element_line(colour = "transparent"))
#

