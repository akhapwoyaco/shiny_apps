#
library(readxl)
Table_1_H_D_I_c <- read_excel(
  path = 'data/HDR23-24_Statistical_Annex_HDI_Table.xlsx', 
  sheet = "HDI", "A8:O230", col_names = F)
Table_1_H_D_I_c <- Table_1_H_D_I_c[,c(1,2,3,5,7,9,11,13,15)]
colnames(Table_1_H_D_I_c) <- c(
  "HDI Rank", "Country", "Human Development Index (HDI)", 
  "Life expectancy at birth", "Expected years of schooling", 
  "Mean years of schooling", "Gross national income (GNI) per capita", 
  "GNI per capita rank minus HDI rank", "HDI rank" 
)
Table_1_H_D_I_c