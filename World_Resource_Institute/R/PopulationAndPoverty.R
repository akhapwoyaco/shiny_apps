library(sf)
library(gridExtra)
library(ggplot2)
library(ggspatial)
library(scales)

setwd('..')
setwd('Population_And_Poverty/')
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
# # Population & poverty
# ke_gini_constituency Average inequality of per capita expenditures in Kenya (also known as the GINI Index) by Constituency in 1999.
ke_gini_constituency[ke_gini_constituency$AVG_GINI == -999, 'AVG_GINI'] <- NA

ggplot() +
  geom_sf(data = ke_gini_constituency, aes(fill = AVG_GINI)) +
  theme_template

# ke_popd89lmb.zip Population density, or the number of people per square kilometer, of Kenya in 1989.
ggplot() +
  geom_sf(data = ke_popd89lmb, aes(fill = TOTAL))

ggplot() +
  geom_sf(data = ke_popd89lmb, aes(fill = MALES))
ggplot() +
  geom_sf(data = ke_popd89lmb, aes(fill = FEMALES))

ggplot() +
  geom_sf(data = ke_popd89lmb, aes(fill = HOUSEDENS))

ggplot() +
  geom_sf(data = ke_popd89lmb, aes(fill = HOUSEHOLDS))

# ke_popd99lmb.zip Population density, or the number of people per square kilometer, in Kenya in 1999.
ggplot() +
  geom_sf(data = ke_popd99lmb)

# ke_poverty1999-cbs_location.zip Poverty rate, poverty gap, poverty density and the percentage of poor housing at Location level in Kenya in 1999.
ggplot() +
  geom_sf(data = ke_poverty1999_cbs_location)

# ke_poverty_eastern-constit.zip Poverty rate, poverty gap, poverty density and amount of Kenyan Shillings (per month per square kilometer) to close the poverty gap for Constituencies in eastern Kenya in 1999.
ggplot() +
  geom_sf(data = ke_poverty_eastern_constit)

# ke_poverty_theoretical-investment.zip Minimum amount of Kenyan Shillings needed per square kilometer per month to close the poverty gap at Location level in Kenya in 1999.
ggplot() +
  geom_sf(data = ke_poverty_theoretical_investment)
#
