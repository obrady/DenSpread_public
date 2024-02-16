# processes the Malaria Atlas space time cube covariates from their natuve resolutions and extents to a common gridded format common to the other covariates

rm(list = ls())

require(raster)
require(sf)
require(dplyr)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")

# raster template
template <- raster("Covariates/Suitability/Aegypti_2020.grd")

globe <- st_read("Reference/Admin2(2011)/admin2.shp")
Brazil = filter(globe, COUNTRY_ID == "BRA")
Mexico = st_read("Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp")


resample = TRUE

# go through each covariate class year by yeara dn resample them to a common template

## EVI

f_list <-list.files("Covariates/MAP_covariates/EVI")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/EVI/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/EVI/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}


## LandCover
f_list <-list.files("Covariates/MAP_covariates/LandCover")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/LandCover/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    hold = values(temp)
    hold = round(hold, 0)
    values(temp) = hold
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/LandCover/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}

## LST day
f_list <-list.files("Covariates/MAP_covariates/LST_day")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/LST_day/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/LST_day/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}

## LST night
f_list <-list.files("Covariates/MAP_covariates/LST_night")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/LST_night/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/LST_night/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}

## TCB
f_list <-list.files("Covariates/MAP_covariates/TCB")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/TCB/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/TCB/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}

## TCW
f_list <-list.files("Covariates/MAP_covariates/TCW")
f_list = f_list[!grepl("_resam", f_list)]
for(i in 1:length(f_list)){
  temp <- raster(paste0("Covariates/MAP_covariates/TCW/", f_list[i]))
  if(resample){
    temp = resample(temp, template)
    writeRaster(temp, file = paste0("Covariates/MAP_covariates/TCW/", gsub(".tif", "", f_list[i]), "_resam.tif"))
  }
  temp = crop(temp, Brazil)
  plot(temp, main = f_list[i])
}







############################################################
# part 2- summarising covariates by Brazilian municipality
############################################################

# custom function for summarisign raster covarites within admin units
# effectively just gives the mean (or mode for landcover) in a more efficient way
admin.extract <- function(raster_list, admin_units, Directory){
  sum_mat = matrix(NA, nrow = nrow(admin_units), ncol = length(raster_list))
  for(i in 1:length(raster_list)){
    f_ras <- raster(paste0(Directory, raster_list[i]))
    f_ras = crop(f_ras, admin_units)
    if(grepl("LandCover", raster_list[i])){
      hold <- extract(f_ras, admin_units)
      sum_mat[, i] = unlist(lapply(hold, function(x){sum(x == 13) / sum(!is.na(x))})) # gives proportion pixels in urban and build up area category
    }else{
      sum_mat[, i] <- extract(f_ras, admin_units, fun = mean, na.rm = T)
    }
  }
  return(sum_mat)
}


# EVI
f_list <-list.files("Covariates/MAP_covariates/EVI")
f_list = f_list[grepl("_resam", f_list)]
Brazil_EVI_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/EVI/")
Mexico_EVI_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/EVI/")
colnames(Brazil_EVI_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_EVI_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
save(Brazil_EVI_sum, file = "Data/Intermediate_datasets/Brazil_EVI_sum.RData")
save(Mexico_EVI_sum, file = "Data/Intermediate_datasets/Mexico_EVI_sum.RData")

# Landcover
f_list <-list.files("Covariates/MAP_covariates/LandCover")
f_list = f_list[grepl("_resam", f_list)]
Brazil_LandCover_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/LandCover/")
Mexico_LandCover_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/LandCover/")
colnames(Brazil_LandCover_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_LandCover_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
save(Brazil_LandCover_sum, file = "Data/Intermediate_datasets/Brazil_LandCover_sum.RData")
save(Mexico_LandCover_sum, file = "Data/Intermediate_datasets/Mexico_LandCover_sum.RData")

# LST day
f_list <-list.files("Covariates/MAP_covariates/LST_day")
f_list = f_list[grepl("_resam", f_list)]
Brazil_LST_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/LST_day/")
Mexico_LST_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/LST_day/")
colnames(Brazil_LST_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_LST_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
Brazil_LST_day_sum = Brazil_LST_sum
Mexico_LST_day_sum = Mexico_LST_sum
# 1 municipality has no data- assume the mean
Brazil_LST_day_sum[is.na(Brazil_LST_day_sum[, 1]), ] = apply(Brazil_LST_day_sum, 2, mean, na.rm = T)
save(Brazil_LST_day_sum, file = "Data/Intermediate_datasets/Brazil_LST_day_sum.RData")
save(Mexico_LST_day_sum, file = "Data/Intermediate_datasets/Mexico_LST_day_sum.RData")

# LST night
f_list <-list.files("Covariates/MAP_covariates/LST_night")
f_list = f_list[grepl("_resam", f_list)]
Brazil_LST_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/LST_night/")
Mexico_LST_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/LST_night/")
colnames(Brazil_LST_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_LST_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
Brazil_LST_night_sum = Brazil_LST_sum
Mexico_LST_night_sum = Mexico_LST_sum
Brazil_LST_night_sum[is.na(Brazil_LST_night_sum[, 1]), ] = apply(Brazil_LST_night_sum, 2, mean, na.rm = T)
save(Brazil_LST_night_sum, file = "Data/Intermediate_datasets/Brazil_LST_night_sum.RData")
save(Mexico_LST_night_sum, file = "Data/Intermediate_datasets/Mexico_LST_night_sum.RData")

# TCB
f_list <-list.files("Covariates/MAP_covariates/TCB")
f_list = f_list[grepl("_resam", f_list)]
Brazil_TCB_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/TCB/")
Mexico_TCB_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/TCB/")
colnames(Brazil_TCB_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_TCB_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
save(Brazil_TCB_sum, file = "Data/Intermediate_datasets/Brazil_TCB_sum.RData")
save(Mexico_TCB_sum, file = "Data/Intermediate_datasets/Mexico_TCB_sum.RData")

# TCW
f_list <-list.files("Covariates/MAP_covariates/TCW")
f_list = f_list[grepl("_resam", f_list)]
Brazil_TCW_sum = admin.extract(f_list, Brazil, "Covariates/MAP_covariates/TCW/")
Mexico_TCW_sum = admin.extract(f_list, Mexico, "Covariates/MAP_covariates/TCW/")
colnames(Brazil_TCW_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
colnames(Mexico_TCW_sum) = gsub(pattern = "_resam.tif", replacement = "", f_list)
save(Brazil_TCW_sum, file = "Data/Intermediate_datasets/Brazil_TCW_sum.RData")
save(Mexico_TCW_sum, file = "Data/Intermediate_datasets/Mexico_TCW_sum.RData")


### static covariates
Brazil_Aegypti_sum = admin.extract("Aegypti_2020.grd", 
                                   Brazil, 
                                   "Covariates/Suitability/")
Mexico_Aegypti_sum = admin.extract("Aegypti_2020.grd", 
                                   Mexico, 
                                   "Covariates/Suitability/")
Brazil_Aegypti_sum[is.na(Brazil_Aegypti_sum[, 1]), 1] = 0
colnames(Brazil_Aegypti_sum) = "Aegypti_suit"
colnames(Mexico_Aegypti_sum) = "Aegypti_suit"
save(Brazil_Aegypti_sum, file = "Data/Intermediate_datasets/Brazil_Aegypti_sum.RData")
save(Mexico_Aegypti_sum, file = "Data/Intermediate_datasets/Mexico_Aegypti_sum.RData")









