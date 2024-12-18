
# Load Data ---------------------------------------------------------------


#
library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)

setwd("./Agriculture/")
list.files(pattern = '*.shp$', recursive = TRUE)
#
#
for (dir_F in list.files(pattern = '*.shp$', recursive = TRUE)){
  dir_split <- strsplit(x = dir_F, split = '/')[[1]]
  file_index <- grep(x = dir_F, pattern = '.shp$')
  
  obj_name <- gsub(pattern = '-', replacement = '_', x = dir_split)[1]
  obj_name <- gsub(pattern = '\\.shp', replacement = '',obj_name)
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
# load data
ls()
# agric
# ke_agriculture$AGRICULTUR <- gsub(pattern = '\\) ', replacement = '\\) \n', x = ke_agriculture$AGRICULTUR)
#

#
# plots
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
  legend.position = c(1.125,0.6),
  legend.background = element_blank(), 
  legend.key = element_blank(),
  legend.title = element_blank(),
  legend.text=element_text(size = 8),
  panel.background = element_rect(fill = "white"))
#
AGRICULTURE_PLOT = grid.arrange(
  
  #
  # KE Agriculture ----------------------------------------------------------
  # plot
  #
  ke_plot +
    geom_sf(data = ke_agriculture#,  
            # aes(fill = AGRICULTUR)
            ) + 
    ggtitle("KE Agriculture") + 
    coord_sf(datum = NA) + 
    #  geom_label(data = kenya_county, aes(geometry = geometry, label = COUNTY), 
    #             size = 5, fontface = "bold") +
    scale_fill_viridis_d(option = 'A') +
    coord_sf(expand = FALSE) +
    theme_template,
  #
  # crop diversity ----------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_crops_diversity, size = 1.5, 
            color = "black", aes(fill = SP_AVG)
    ) + 
    ggtitle("KE Crops Diversity") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Crops Foodshare ------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_crops_foodshare |> sf::st_set_crs(4326), size = 1.5,
            color = "black", aes(fill = FOOD_SHARE)
    ) +
    ggtitle("KE Crops Foodshare") +
    coord_sf(datum = NA) +
    theme_template,
  #
  # KE Crops Intensity ------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_crops_intensity, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Crops Intensity") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Crops Irrigation -------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_crops_irrig, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Crops Irrigation") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  #  KE Crops Size ----------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_crops_size, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Crops Size") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Livestock 1990 -------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_livestock_1990, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Livestock 1990") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Milk-production ------------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_milk_production, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Milk Production") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE milk surplus deficit -------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_milk_surplus_deficit, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Milk Surplu Deficit") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Pineapple Plantations ------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_pineapple_plantations, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Pineapple Plantations") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Tree Plantations --------------------------------------------------
  # plot
  ke_plot +
    geom_sf(data = ke_tree_plantations, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Tree Plantations") + 
    coord_sf(datum = NA) + 
    theme_template,
  #
  # KE Woodlots in cropland -------------------------------------------------
  # plot
  ke_plot + 
    geom_sf(data = ke_woodlots_in_cropland, size = 1.5, 
            color = "black", fill = "cyan1"
    ) + 
    ggtitle("KE Woodlots in Cropland") + 
    coord_sf(datum = NA) + 
    theme_template, ncol = 3
  #
)
#
ggsave(
  plot = AGRICULTURE_PLOT, filename = "AGRICULTURE_PLOT.png", 
  width = 40, height = 40, units = "cm", 
  dpi = 500)
#