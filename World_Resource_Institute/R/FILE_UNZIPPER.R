
#
### VERSION 2 ##############################################################
#
zip_files = list.files(path = ".", recursive = T,pattern = '*.zip', full.names = T)
#
zip_files
#
list.files()
for (zip_file in zip_files){
  ex_dir_name = gsub(
    pattern = "\\.zip$", replacement = '', zip_file)
  unzip(zipfile = zip_file, exdir = ex_dir_name)  
}
# basename("./Water_Irrigation_And_hydropower/ke_hydropower-dams.zip")
# dirname("./Water_Irrigation_And_hydropower/ke_hydropower-dams.zip")
#
