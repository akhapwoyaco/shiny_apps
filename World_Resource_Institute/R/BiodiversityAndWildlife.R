library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)
# https://rviews.rstudio.com/2018/06/20/reading-rrd-files/
setwd('Biodiversity_And_Wildlife/')
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
# Biodiversity & wildlife
# ke_dugong-dolphin-sites Dugong sites and dolphin schools on the eastern coast of Kenya.
ke_plot + 
  geom_sf(data = ke_dugong_dolphin_sites, aes(color = NAME)) +
  theme_template +
  theme_void()

# ke_eba Endemic Bird Areas (EBAs) in Kenya.
ke_plot + 
  geom_sf(data = ke_eba, aes(fill = EBANAME)) + 
  theme_template

# ke_grevys_1970s Number of observed Grevy's zebra from 1977 to 1978 in Kenya.
ke_plot + 
  geom_sf(data = ke_grevys_1970s, aes(fill = ZG70M)) + 
  theme_template

# ke_grevys_1990s Number of observed Grevy's zebra from 1994 to 1996 in Kenya.
ke_plot + 
  geom_sf(data = ke_grevys_1990s, aes(fill = ZG90M)) + 
  theme_template

# ke_grevys_range Potential range of Grevy's zebra in Kenya.
ke_plot + 
  geom_sf(data = ke_grevys_range, aes(fill = COUNT)) + 
  theme_template

# ke_iba-status Important Bird Areas (IBAs) and their status in Kenya in 2003-2004.
ke_plot + 
  #geom_sf(data = )
  geom_sf(data = ke_iba_status, aes(fill = STATUS2, color = NATIONAL_N)) + 
  theme_template

# ke_mammals Predicted mammal diversity, or the total number of mammal species, in Kenya.
ke_plot + 
  geom_sf(data = ke_mammals, aes(color = MAMMALS)
          ) + 
  theme_template

# ke_numbers_buffalo Spatial distribution of buffalo numbers observed from low-altitude flights in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_numbers_buffalo, aes(fill = BF90M)) + 
  theme_template

# ke_numbers_elephant Spatial distribution of elephant numbers observed from low-altitude flights in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_numbers_elephant, aes(fill = EL90M)) + 
  theme_template

# ke_numbers_giraffe Spatial distribution of giraffe numbers observed from low-altitude flights in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_numbers_giraffe, aes(fill = GF90M)) + 
  theme_template

# ke_numbers_wildebeest Spatial distribution of wildebeest numbers observed from low-altitude flights in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_numbers_wildebeest, aes(fill = WL90M)) + 
  theme_template

# ke_numbers_wildebeest_kitengela Number of wildebeest observed in 1977-78 and 1994-96 in the Athi-Kapiti Plains, Kenya.
ke_plot + 
  geom_sf(data = ke_numbers_wildebeest_kitengela, aes(fill = WL7080)) + 
  theme_template

ke_plot + 
  geom_sf(data = ke_numbers_wildebeest_kitengela, aes(fill = WL9000)) + 
  theme_template

# ke_numbers_zebra Spatial distribution of zebra numbers observed from low-altitude flights in Kenya from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_numbers_zebra, aes(fill = ZB90M)) + 
  theme_template

# ke_sable_antelope_sites Sable antelope sites on the eastern coast of Kenya.
ke_plot + 
  geom_sf(data = ke_sable_antelope_sites, aes(color = NAME)) + 
  theme_template

# ke_turtle_breeding_site Turtle nesting and breeding sites on the eastern coast of Kenya.
ke_plot + 
  geom_sf(data = ke_turtle_breeding_site, aes(fill = PRIORITY)) + 
  theme_template

# ke_wildlife_1970-90 Change in wildlife density in Kenya's rangelands between 1977-78 and 1994-96.
ke_plot + 
  geom_sf(data = ke_wildlife_1970_90, aes(fill = CHG7090)) + 
  theme_template

# ke_wildlife_1970 Wildlife density in Kenya's rangelands from 1977 to 1978.
ke_plot + 
  geom_sf(data = ke_wildlife_1970, aes(fill = WILDL_DEN)) + 
  theme_template

# ke_wildlife_1990 Wildlife density in Kenya 's rangelands from 1994 to 1996.
ke_plot + 
  geom_sf(data = ke_wildlife_1990, aes(fill = ALL_WL_DEN)) + 
  theme_template
#
