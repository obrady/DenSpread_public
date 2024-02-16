rm(list = ls())

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting


setwd("/Users/eideobra/Dropbox/C1_Reference/Oli_SSP370/")

# load in admin shapefile
ad0 <- readOGR("/Users/eideobra/Dropbox/C1_Reference/Country_shapefiles/Admin0(2011)/admin0.shp")

ad0_bra <- ad0[ad0$NAME == "Brazil", ]
ad0_mex <- ad0[ad0$NAME == "Mexico", ]


NCDF_filename = 'monthly/temperature/gfdl-esm4_r1i1p1f1_w5e5_ssp370_tasmaxAdjust_global_monthly_2015_2099.nc'

netCDF_rasterizer <- function(NCDF_filename, country1, country2){
  nc_data <- nc_open(NCDF_filename)
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, names(nc_data$var)[1])
  tmax.array <- ncvar_get(nc_data, names(nc_data$var)[2])
  fillvalue <- ncatt_get(nc_data, names(nc_data$var)[2], "_FillValue")
  nc_close(nc_data)
  
  tmax.array[tmax.array == fillvalue$value] <- NA
  r_brick <- brick(tmax.array, 
                   xmn=min(lat), 
                   xmx=max(lat), 
                   ymn=min(lon), 
                   ymx=max(lon), 
                   crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  # now summarise at national level 
  toolik_series_c1 <- extract(r_brick, country1, method='simple', fun = mean, na.rm = T)
  toolik_series_c2 <- extract(r_brick, country2, method='simple', fun = mean, na.rm = T)
  
  toolik_df <- data.frame(year= seq(2015,2101, (1/12))[-1], 
                          var_c1= t(toolik_series_c1),
                          var_c2 = t(toolik_series_c2))
  
  return(toolik_df)
}


model_list <- c("gfdl-esm4",
                "ipsl-cm6a-lr",
                "mpi-esm1-2-hr",
                "mri-esm2-0",
                "ukesm1-0-ll")

variable_list <- data.frame(folder = c("temperature", "temperature", "temperature",
                                       "humidity",
                                       "precipitation"),
                            variable = c("tas", "tasmax", "tasmin",
                                         "hurs",
                                         "pr"))

nat_proj_bra <- nat_proj_mex <- data.frame(year= seq(2015,2101, (1/12))[-1],
                                           tas = NA,
                                           tasmax = NA,
                                           tasmin = NA,
                                           hum = NA,
                                           pr = NA)



for(i in 1:nrow(variable_list)){
  c1_ensemble <- data.frame(year = seq(2015,2101, (1/12))[-1])
  c2_ensemble <- data.frame(year = seq(2015,2101, (1/12))[-1])
  
  for(k in 1:length(model_list)){
    f_filename = paste0("monthly/",
                        variable_list$folder[i],
                        "/",
                        model_list[k],
                        "_r1i1p1f1_w5e5_ssp370_",
                        variable_list$variable[i],
                        "Adjust_global_monthly_2015_2099.nc")
    ts_df <- netCDF_rasterizer(f_filename, country1 = ad0_bra, country2 = ad0_mex)
    c1_ensemble = cbind(c1_ensemble, ts_df$var_c1)
    c2_ensemble = cbind(c2_ensemble, ts_df$var_c2)
  }
  
  # mean across climate models
  c1_mean = apply(c1_ensemble[, 2:ncol(c1_ensemble)], 1, mean)
  c2_mean = apply(c2_ensemble[, 2:ncol(c2_ensemble)], 1, mean)
  
  # assign values to top table
  nat_proj_bra[, variable_list$variable[[i]]] = c1_mean
  nat_proj_mex[, variable_list$variable[[i]]] = c2_mean
}


# summarising to annual scales
nat_proj_bra$year_bin <- floor(nat_proj_bra$year)
nat_proj_mex$year_bin <- floor(nat_proj_mex$year)

nat_proj_bra_ann <- data.frame(aggregate(tas ~ year_bin, data = nat_proj_bra, FUN = mean),
                               aggregate(tas ~ year_bin, data = nat_proj_bra, FUN = sd)[, 2],
                               aggregate(tasmax ~ year_bin, data = nat_proj_bra, FUN = mean)[, 2],
                               aggregate(tasmax ~ year_bin, data = nat_proj_bra, FUN = sd)[, 2],
                               aggregate(tasmin ~ year_bin, data = nat_proj_bra, FUN = mean)[, 2],
                               aggregate(tasmin ~ year_bin, data = nat_proj_bra, FUN = sd)[, 2],
                               aggregate(hurs ~ year_bin, data = nat_proj_bra, FUN = mean)[, 2],
                               aggregate(hurs ~ year_bin, data = nat_proj_bra, FUN = sd)[, 2],
                               aggregate(pr ~ year_bin, data = nat_proj_bra, FUN = mean)[, 2],
                               aggregate(pr ~ year_bin, data = nat_proj_bra, FUN = sd)[, 2])

nat_proj_mex_ann <- data.frame(aggregate(tas ~ year_bin, data = nat_proj_mex, FUN = mean),
                               aggregate(tas ~ year_bin, data = nat_proj_mex, FUN = sd)[, 2],
                               aggregate(tasmax ~ year_bin, data = nat_proj_mex, FUN = mean)[, 2],
                               aggregate(tasmax ~ year_bin, data = nat_proj_mex, FUN = sd)[, 2],
                               aggregate(tasmin ~ year_bin, data = nat_proj_mex, FUN = mean)[, 2],
                               aggregate(tasmin ~ year_bin, data = nat_proj_mex, FUN = sd)[, 2],
                               aggregate(hurs ~ year_bin, data = nat_proj_mex, FUN = mean)[, 2],
                               aggregate(hurs ~ year_bin, data = nat_proj_mex, FUN = sd)[, 2],
                               aggregate(pr ~ year_bin, data = nat_proj_mex, FUN = mean)[, 2],
                               aggregate(pr ~ year_bin, data = nat_proj_mex, FUN = sd)[, 2])
colnames(nat_proj_bra_ann) = colnames(nat_proj_mex_ann) = c("year", "tmean_m", "tmean_sd", "tmax_m","tmax_sd", 
                                                            "tmin_m", "tmin_sd", "hurs_m", "hurs_sd", 
                                                            "pr_m", "pr_sd")

#remove 2101 as based on not a full year
nat_proj_bra_ann = nat_proj_bra_ann[nat_proj_bra_ann$year != 2101, ]
nat_proj_mex_ann = nat_proj_mex_ann[nat_proj_mex_ann$year != 2101, ]







######################################
### load MAP covaraites and process to a comparable table
######################################

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/Covariates/MAP_covariates/")

ras_years <- 2000:2015

national_var_trend <- data.frame(year = ras_years,
                                 LST_d_m_bra = NA,
                                 LST_d_s_bra = NA,
                                 LST_n_m_bra = NA,
                                 LST_n_s_bra = NA,
                                 EVI_m_bra = NA,
                                 EVI_s_bra = NA,
                                 TCB_m_bra = NA,
                                 TCB_s_bra = NA,
                                 TCW_m_bra = NA,
                                 TCW_s_bra = NA,
                                 LandCover_bra = NA,
                                 LST_d_m_mex = NA,
                                 LST_d_s_mex = NA,
                                 LST_n_m_mex = NA,
                                 LST_n_s_mex = NA,
                                 EVI_m_mex = NA,
                                 EVI_s_mex = NA,
                                 TCB_m_mex = NA,
                                 TCB_s_mex = NA,
                                 TCW_m_mex = NA,
                                 TCW_s_mex = NA,
                                 LandCover_mex = NA)

for(i in 1:length(ras_years)){
  # load rasters
  LST_day_ras <- raster(paste0("LST_day/LST_mean_", ras_years[i], "_resam.tif"))
  LST_day_stDev_ras <- raster(paste0("LST_day/LST_stdDev_", ras_years[i], "_resam.tif"))
  LST_night_ras <- raster(paste0("LST_night/LST_Night_Mean_", ras_years[i], "_resam.tif"))
  LST_night_stdDev_ras <- raster(paste0("LST_night/LST_night_stdDev_", ras_years[i], "_resam.tif"))
  EVI_ras <- raster(paste0("EVI/EVI_mean_", ras_years[i], "_resam.tif"))
  EVI_stdDev_ras <- raster(paste0("EVI/EVI_stdDev_", ras_years[i], "_resam.tif"))
  
  if(ras_years[i] > 2014){
    TCB_ras <- TCB_stdDev_ras <- TCW_ras <- TCW_stdDev_ras <- EVI_stdDev_ras
    values(TCB_ras) <- values(TCB_stdDev_ras) <- values(TCW_ras) <- 
      values(TCW_stdDev_ras) <- NA
  }else{
    TCB_ras <- raster(paste0("TCB/TCB_mean_", ras_years[i], "_resam.tif"))
    TCB_stdDev_ras <- raster(paste0("TCB/TCB_stdDev_", ras_years[i], "_resam.tif"))
    TCW_ras <- raster(paste0("TCW/TCW_mean_", ras_years[i], "_resam.tif"))
    TCW_stdDev_ras <- raster(paste0("TCW/TCW_stdDev_", ras_years[i], "_resam.tif"))
  }
  
  if((ras_years[i] < 2002) |(ras_years[i] > 2012)){
    LandCover_ras <- TCW_stdDev_ras
    values(LandCover_ras) <- NA
  }else{
    LandCover_ras <- raster(paste0("LandCover/LandCover_mean_", ras_years[i], "_resam.tif"))
  }
  
  # stack and crop to Brazil and Mexico
  ras_stack <- stack(LST_day_ras, LST_day_stDev_ras, LST_night_ras, LST_night_stdDev_ras,
                     EVI_ras, EVI_stdDev_ras, TCB_ras, TCB_stdDev_ras, TCW_ras,
                     TCW_stdDev_ras, LandCover_ras)
  ras_stack_bra  =mask(crop(ras_stack, ad0_bra), ad0_bra)
  ras_stack_mex  =mask(crop(ras_stack, ad0_mex), ad0_mex)
  
  # country-wide summary
  bra_sum = extract(ras_stack_bra, ad0_bra, fun = mean, na.rm = T)
  mex_sum = extract(ras_stack_mex, ad0_mex, fun = mean, na.rm = T)
  
  national_var_trend[i, 2:ncol(national_var_trend)] = c(bra_sum[1, ], mex_sum[1, ])
}







##############################
# comparison between contemproary and future projections and extrapolation
################################



# regression extrapolation and plotting function

climate.Extrap <- function(combinedDF, TCWlog = FALSE){
  # limit RCP data to 2040 and delete any years that overlap with MAP data
  combinedDF = combinedDF[combinedDF$Year < 2041, ]
  if(TCWlog){
    combinedDF = combinedDF[!((combinedDF$Year == 2015) & (combinedDF$type == "MAP")), ]
    }
  
  # !!!! exclude year 2015 due to anomalous temperature data in MAP covariates (v cold not supported by ground obs)
  combinedDF = combinedDF[!((combinedDF$Year == 2015) & (combinedDF$type == "MAP")), ]
  
  # scale datasets separately
  beforeMean = mean(combinedDF$val[combinedDF$type == "MAP"])
  beforeSD = sd(combinedDF$val[combinedDF$type == "MAP"] - beforeMean)
  
  scaledDF = combinedDF
  scaledDF$val[scaledDF$type == "MAP"] = scale(scaledDF$val[scaledDF$type == "MAP"])
  scaledDF$val[scaledDF$type == "RCP"] = scale(scaledDF$val[scaledDF$type == "RCP"])
  
  
  # fit a GLM for all data together and separately for checks
  scaledDF_mod = glm(val ~ Year + type, data = scaledDF)
  scaledDF_mod_MAP = glm(val ~ Year, data = scaledDF[scaledDF$type == "MAP", ])
  scaledDF_mod_RCP = glm(val ~ Year, data = scaledDF[scaledDF$type == "RCP", ])
  
  # make new predictions from each of the three models
  newpreds <- data.frame(Year = 1980:2040,
                         val = c(predict(scaledDF_mod, newdata = data.frame(Year = 1980:2040,
                                                                           type = "MAP")),
                                 predict(scaledDF_mod, newdata = data.frame(Year = 1980:2040,
                                                                           type = "RCP")),
                                 predict(scaledDF_mod_MAP, newdata = data.frame(Year = 1980:2040)),
                                 predict(scaledDF_mod_RCP, newdata = data.frame(Year = 1980:2040))),
                         type = c(rep("All_data_MAP_prediction", 61),
                                  rep("All_data_RCP_prediction", 61),
                                  rep("MAP_data_prediction", 61),
                                  rep("RCP_data_prediction", 61)))
  
  
  # fit figure
  g1 <- ggplot(scaledDF) +
    geom_point(aes(x = Year, y = val, col = type)) +
    geom_line(data = newpreds, aes(x = Year, y = val, group = type, col = type))
  
  # extrapolation and extrapolation figure
  set.seed(123)
  year_dev_sample <- sample(residuals(scaledDF_mod), length(1980:(min(combinedDF$Year) - 1)))
  # extrapolated predictions combine 1) randomly sampled deviations from the linear trend 1985-2000
  # 2) MAP data 2001-2014 and 3)magnitude and variancecorrected RCP data 2015-2039
  
  # magnitude corection
  beforeVals = coefficients(scaledDF_mod)[1] + coefficients(scaledDF_mod)[2]*(1980:(min(combinedDF$Year) - 1)) + year_dev_sample
  MAPVals = scaledDF$val[scaledDF$type == "MAP"]
  afterVals = scaledDF$val[scaledDF$type == "RCP"] - coefficients(scaledDF_mod)[3]
  
  extrap_predictions <- data.frame(Year = 1980:2040,
                                   val = c(beforeVals, MAPVals, afterVals),
                                   type = c(rep("Sampled", length(1980:(min(combinedDF$Year) - 1))),
                                            rep("MAP", sum(combinedDF$type == "MAP")),
                                            rep("RCP", sum(combinedDF$type == "RCP"))))
  
  # convert predictions back to original scale (i.e. reverse scaling)
  extrap_predictions$val = (extrap_predictions$val * beforeSD) + beforeMean
  
  g2 <- ggplot(extrap_predictions) +
    geom_point(aes(x = Year, y = val, col = type))
  
  # return extrapolation prediction and the two graphs
  return(list(predictions = extrap_predictions,
              fit_figure = g1,
              extrap_figure = g2,
              fit_figure_dat = scaledDF,
              extrap_figure_dat = extrap_predictions))
}


# set wd to save future projections
coreFP = "/Users/eideobra/Dropbox/08_Serotype_spread/Covariates/Future_projections"

## 01 Mean night time temperature
# Annual mean monthly minimum temperature

# assemble data
B_cdf <- data.frame(Year = c(national_var_trend$year, nat_proj_bra_ann$year),
                         val = c(national_var_trend$LST_n_m_bra, nat_proj_bra_ann$tmin_m),
                         type = c(rep("MAP", nrow(national_var_trend)),
                                  rep("RCP", nrow(nat_proj_bra_ann))))
M_cdf <- data.frame(Year = c(national_var_trend$year, nat_proj_mex_ann$year),
                    val = c(national_var_trend$LST_n_m_mex, nat_proj_mex_ann$tmin_m),
                    type = c(rep("MAP", nrow(national_var_trend)),
                             rep("RCP", nrow(nat_proj_mex_ann))))

# run extrapolation
var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

# combined country extrapolation plots
var1_comb = rbind(var1_Brazil$extrap_figure_dat,
                  var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])),
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National mean night time temperature")


# save outputs
ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/MeanNightTemperature_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanNightTemperature_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/MeanNightTemperature_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanNightTemperature_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/MeanNightTemperature_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/MeanNightTemperature_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/MeanNightTemperature_Mexico.csv"))



## 02 SD daytime temperature
# Annual standard deviation of monthly maximum temperature
B_cdf$val = c(national_var_trend$LST_d_s_bra, nat_proj_bra_ann$tmax_sd)
M_cdf$val = c(national_var_trend$LST_d_s_mex, nat_proj_mex_ann$tmax_sd)

var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National SD daytime temperature")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/SDDayTemperature_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDDayTemperature_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/SDDayTemperature_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDDayTemperature_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/SDDayTemperature_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/SDDayTemperature_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/SDDayTemperature_Mexico.csv"))


## 03 Mean day time temperature
# Annual mean monthly mean temperature
B_cdf$val = c(national_var_trend$LST_d_m_bra, nat_proj_bra_ann$tmean_m)
M_cdf$val = c(national_var_trend$LST_d_m_mex, nat_proj_mex_ann$tmean_m)

var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National mean daytime temperature")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/MeanDayTemperature_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanDayTemperature_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/MeanDayTemperature_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanDayTemperature_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/MeanDayTemperature_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/MeanDayTemperature_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/MeanDayTemperature_Mexico.csv"))


## 04 SD night time temperature
# Annual standard deviation of monthly minimum temperature
B_cdf$val = c(national_var_trend$LST_n_s_bra, nat_proj_bra_ann$tmin_sd)
M_cdf$val = c(national_var_trend$LST_n_s_mex, nat_proj_mex_ann$tmin_sd)

var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National SD nighttime temperature")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/SDNightTemperature_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDNightTemperature_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/SDNightTemperature_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDNightTemperature_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/SDNightTemperature_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/SDNightTemperature_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/SDNightTemperature_Mexico.csv"))

## 05 SD EVI
# Annual standard deviation of monthly mean humidity
B_cdf$val = c(national_var_trend$EVI_s_bra, nat_proj_bra_ann$hurs_sd)
M_cdf$val = c(national_var_trend$EVI_s_mex, nat_proj_mex_ann$hurs_sd)

var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National SD EVI")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/SDEVI_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDEVI_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/SDEVI_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDEVI_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/SDEVI_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/SDEVI_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/SDEVI_Mexico.csv"))


## 06 Mean EVI
# Annual mean of monthly mean humidity
B_cdf$val = c(national_var_trend$EVI_m_bra, nat_proj_bra_ann$hurs_m)
M_cdf$val = c(national_var_trend$EVI_m_mex, nat_proj_mex_ann$hurs_m)

var1_Brazil <- climate.Extrap(B_cdf)
var1_Mexico <- climate.Extrap(M_cdf)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National mean EVI")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/MeanEVI_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanEVI_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/MeanEVI_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/MeanEVI_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/MeanEVI_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/MeanEVI_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/MeanEVI_Mexico.csv"))


## 07 SD TCW
# Annual standard deviation of monthly mean precipitation
B_cdf$val = c(national_var_trend$TCW_s_bra, nat_proj_bra_ann$pr_sd)
M_cdf$val = c(national_var_trend$TCW_s_mex, nat_proj_mex_ann$pr_sd)

var1_Brazil <- climate.Extrap(B_cdf, TCWlog = TRUE)
var1_Mexico <- climate.Extrap(M_cdf, TCWlog = TRUE)

var1_comb = rbind(var1_Brazil$extrap_figure_dat,var1_Mexico$extrap_figure_dat)
var1_comb$Country = c(rep("Brazil", length(var1_Brazil$extrap_figure_dat[, 1])), 
                      rep("Mexico", length(var1_Mexico$extrap_figure_dat[, 1])))
g_comb <- ggplot(var1_comb) +
  geom_point(aes(x = Year, y = val, col = type, shape = Country)) +
  ylab("National SD TCW")

ggsave(var1_Brazil[[2]], filename = paste0(coreFP, "/figures/fit/SDTCW_Brazil.png"))
ggsave(var1_Brazil[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDTCW_Brazil.png"))
ggsave(var1_Mexico[[2]], filename = paste0(coreFP, "/figures/fit/SDTCW_Mexico.png"))
ggsave(var1_Mexico[[3]], filename = paste0(coreFP, "/figures/extrapolation/SDTCW_Mexico.png"))
ggsave(g_comb, filename = paste0(coreFP, "/figures/extrapolation/SDTCW_Both.png"))

write.csv(var1_Brazil[[1]], file = paste0(coreFP, "/predictions/SDTCW_Brazil.csv"))
write.csv(var1_Mexico[[1]], file = paste0(coreFP, "/predictions/SDTCW_Mexico.csv"))




