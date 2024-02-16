################################################################################
# Plots maps of model-predicted invasion in the contemporary period
################################################################################

# Oliver Brady
# 21 April 2022

rm(list = ls())

require(sf)
require(sp)
require(tmap)
require(rgdal)
require(raster)
require(tidyverse)
require(ggplot2)
require(mgcv)

# working directory
setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")
# setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")

# load in predicitons from the model for Mexico and Brazil
dat <- read.csv("Predictions/Mex-Fig1-ContemporaryPredsJan24.csv")
dat_bra <- read.csv("Predictions/Bra-Fig1-ContemporaryPredsJan24.csv")


# limit residuals to -5 or 5 ( ~5% of values outside this range are never predicted or never invaded,
# so artificial values anyway)
dat$Residual[dat$Residual < -5] = -5
dat$Residual[dat$Residual > 5] = 5
dat_bra$Residual[dat_bra$Residual < -5] = -5
dat_bra$Residual[dat_bra$Residual > 5] = 5

# load administrative unit shapefules
admin2 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))

admin2_bra <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
admin2_bra = admin2_bra[admin2_bra$COUNTRY_ID == "BRA", ]

admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")


# assign predictions from models to shapefiles
admin2$YearPredicted = dat$YearPredicted[match(admin2$GAUL_CODE, dat$GAUL)]
admin2$Residual = dat$Residual[match(admin2$GAUL_CODE, dat$GAUL)]
admin2_bra$YearPredicted = dat_bra$YearPredicted[match(admin2_bra$GAUL_CODE, dat_bra$GAUL)]
admin2_bra$Residual = dat_bra$Residual[match(admin2_bra$GAUL_CODE, dat_bra$GAUL)]

# load in original data to add an observed date of arrival column
dat_bra <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
dat_mex <- read.csv("Data/Mexico_mun_dat_cleaned_thresholded.csv")
admin2$YearObserved <- dat_mex$year[match(admin2$GAUL_CODE, dat_mex$GAUL_A2)]
admin2_bra$YearObserved <- dat_bra$year[match(admin2_bra$GAUL_CODE, dat_bra$GAUL)]

#### mapping

# 01 Observed invasion date
map_obs <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "YearObserved", palette = "Spectral", style = "cont", title = "Invasion\n year",
          breaks = c(1995, 2000, 2005, 2010, 2015, 2019),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 1995, expression("" <=1995), formatC(x, digits=0, format="d"))),
            scale = 1.8)


map_obs_bra <- tm_shape(admin0, bbox = extent(admin2_bra)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2_bra) +
  tm_fill(col = "YearObserved", palette = "Spectral", style = "cont", title = "Invasion\n year",
          breaks = c(2001, 2005, 2010, 2015, 2019),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 2001, expression("" <=2001), formatC(x, digits=0, format="d"))),
            scale = 1.8)

tmap_save(map_obs, filename = "Plots/Jan24/Mex_contemporary_observed.png")
tmap_save(map_obs_bra, filename = "Plots/Jan24/Bra_contemporary_observed.png")

# 01b histograms of cumulative invaded municipalities
cum_inv_mex = table(dat_mex$Year)
cum_inv_bra = table(dat_bra$Year)

cum_inv_mex = data.frame(Year = as.numeric(names(cum_inv_mex)),
                         Count = cumsum(cum_inv_mex))
cum_inv_bra = data.frame(Year = as.numeric(names(cum_inv_bra)),
                         Count = cumsum(cum_inv_bra))

p1 <- ggplot(cum_inv_mex, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Cumulative municipalities",
                     breaks = c(0, 500, 1000, 1500, 2000, 2456),
                     labels = c(0, 500, 1000, 1500, 2000, 2456),
                     limits = c(0, 2456)) +
  theme_bw()
p1
p2 <- ggplot(cum_inv_bra, aes(x = Year, y = Count)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(name = "Cumulative municipalities",
                     breaks = c(0, 1000, 2000, 3000, 4000, 5000, 5510),
                     labels = c(0, 1000, 2000, 3000, 4000, 5000, 5510),
                     limits = c(0, 5510)) +
  theme_bw()
p2

ggsave(filename = "Plots/Jan24/Cumulative_muns_infected_Brazil.png", height = 3, width = 4, p1)
ggsave(filename = "Plots/Jan24/Cumulative_muns_infected_Mexico.png", height = 3, width = 4, p2)


# 01 raw predictions
map_preds <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "YearPredicted", palette = "Spectral", style = "cont", title = "Invasion\n year",
          breaks = c(1995, 2000, 2005, 2010, 2015, 2019),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 1995, expression("" <=1995), formatC(x, digits=0, format="d"))),
            scale = 1.8)


map_preds_bra <- tm_shape(admin0, bbox = extent(admin2_bra)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2_bra) +
  tm_fill(col = "YearPredicted", palette = "Spectral", style = "cont", title = "Invasion\n year",
          breaks = c(2001, 2005, 2010, 2015, 2019),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 2001, expression("" <=2001), formatC(x, digits=0, format="d"))),
            scale = 1.8)

tmap_save(map_preds, filename = "Plots/Jan24/Mex_contemporary.png")
tmap_save(map_preds_bra, filename = "Plots/Jan24/Bra_contemporary.png")


# 02 raw residuals
map_resids <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "Residual", palette = "BrBG", style = "cont", title = "Observed year - Predicted year") +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
            legend.title.size=2,
            legend.text.size = 1.2)


map_resids_bra <- tm_shape(admin0, bbox = extent(admin2_bra)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2_bra) +
  tm_fill(col = "Residual", palette = "BrBG", style = "cont", title = "Observed year - Predicted year") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
            legend.title.size=2,
            legend.text.size = 1.2)

tmap_save(map_resids, filename = "Plots/Jan24/Mex_contemporary_residual.png")
tmap_save(map_resids_bra, filename = "Plots/Jan24/Bra_contemporary_residual.png")






### continuous approximation maps

# load template raster to make predictions to:
temras <- raster("Covariates/MAP_covariates/LST_day/LST_mean_2000_resam.tif")
temras_mex <- mask(crop(temras, admin2), admin2)
temras_bra <- mask(crop(temras, admin2_bra), admin2_bra)

# set up fitting dataframes
gam_dat <- data.frame(arrival = admin2$YearPredicted,
                      residual = admin2$Residual,
                      st_coordinates(st_centroid(admin2)))
gam_dat_bra <- data.frame(arrival = admin2_bra$YearPredicted,
                          residual = admin2_bra$Residual,
                      st_coordinates(st_centroid(admin2_bra)))

# artificially assign NAs a cutoff value?
gam_dat$arrival[is.na(gam_dat$arrival)] = 2020
gam_dat_bra$arrival[is.na(gam_dat_bra$arrival)] = 2020

# fit GAMs
# k set to 200 for now based on preliminary explorations
approx_gam <- gam(arrival ~ s(X, Y, k = 200), data = gam_dat)
approx_gam_bra <- gam(arrival ~ s(X, Y, k = 200), data = gam_dat_bra)

approx_resid_gam <- gam(residual ~ s(X, Y, k = 200), data = gam_dat)
approx_resid_gam_bra <- gam(residual ~ s(X, Y, k = 200), data = gam_dat_bra)

# make predicitons to all municipalities
newdat <- as.data.frame(coordinates(temras_mex))
colnames(newdat) = c("X", "Y")
newdat$X =  newdat$X * (values(temras_mex) >= 0)
newdat$Y =  newdat$Y * (values(temras_mex) >= 0)

newdat_bra <- as.data.frame(coordinates(temras_bra))
colnames(newdat_bra) = c("X", "Y")
newdat_bra$X =  newdat_bra$X * (values(temras_bra) >= 0)
newdat_bra$Y =  newdat_bra$Y * (values(temras_bra) >= 0)

newdat$Approx_pred <- predict(approx_gam, newdata = newdat)
newdat$Approx_pred[newdat$Approx_pred > 2019] = NA
newdat$Approx_pred_resid <- predict(approx_resid_gam, newdata = newdat)
# constrain residual predictions to +/- 5 years
newdat$Approx_pred_resid[newdat$Approx_pred_resid < -5] = -5
newdat$Approx_pred_resid[newdat$Approx_pred_resid > 5] = 5

# assign back to rasters
approxras <- approxras_resid <- temras_mex
values(approxras) = round(as.numeric(newdat$Approx_pred) * (values(temras_mex) >= 0), 0)
values(approxras_resid) = round(as.numeric(newdat$Approx_pred_resid) * (values(temras_mex) >= 0), 0)

# hack to include full range of values in Mexico residuals map's legend
values(approxras_resid)[1:11] = seq(-5, 5, 1)


newdat_bra$Approx_pred <- predict(approx_gam_bra, newdata = newdat_bra)
newdat_bra$Approx_pred[newdat_bra$Approx_pred > 2019] = NA
newdat_bra$Approx_pred_resid <- predict(approx_resid_gam_bra, newdata = newdat_bra)
newdat_bra$Approx_pred_resid[newdat_bra$Approx_pred_resid < -5] = -5
newdat_bra$Approx_pred_resid[newdat_bra$Approx_pred_resid > 5] = 5
approxras_bra <- approxras_resid_bra <- temras_bra
values(approxras_bra) = round(as.numeric(newdat_bra$Approx_pred) * (values(temras_bra) >= 0), 0)
values(approxras_resid_bra) = round(as.numeric(newdat_bra$Approx_pred_resid) * (values(temras_bra) >= 0), 0)


# hack to include full range of values in Brazil residuals maps
values(approxras_resid_bra)[1:11] = seq(-5, 5, 1)



mexico <- admin0[admin0$COUNTRY_ID == "MEX", ]
brazil <- admin0[admin0$COUNTRY_ID == "BRA", ]

# predictions
map_preds2 <- tm_shape(admin0, bbox = extent(approxras)) +
  tm_fill(col = rgb(215/255,215/255,215/255)) +
  tm_shape(mexico) +
  tm_fill(col = rgb(150/255,150/255,150/255)) +
  tm_shape(approxras) +
  tm_raster(palette = "Spectral", style = "cont", 
            breaks = c(1995, 2000, 2005, 2010, 2015, 2019),
            title = "Invasion\n year\n (trend)") +
  tm_shape(admin0) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 1995, expression("" <=1995), formatC(x, digits=0, format="d"))),
            scale = 1.8)

map_preds2_bra <- tm_shape(admin0, bbox = extent(approxras_bra)) +
  tm_fill(col = rgb(215/255,215/255,215/255)) +
  tm_shape(brazil) +
  tm_fill(col = rgb(150/255,150/255,150/255)) +
  tm_shape(approxras_bra) +
  tm_raster(palette = "Spectral", style = "cont", 
            breaks = c(2001, 2005, 2010, 2015, 2019),
            title = "Invasion\n year\n (trend)") +
  tm_shape(admin0) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 2001, expression("" <=2001), formatC(x, digits=0, format="d"))),
            scale = 1.8)

tmap_save(map_preds2, filename = "Plots/Jan24/Mex_contemporary_smooth.png")
tmap_save(map_preds2_bra, filename = "Plots/Jan24/Bra_contemporary_smooth.png")


# residuals
map_resids2 <- tm_shape(admin0, bbox = extent(approxras)) +
  tm_fill(col = rgb(215/255,215/255,215/255)) +
  tm_shape(mexico) +
  tm_fill(col = rgb(150/255,150/255,150/255)) +
  tm_shape(approxras_resid) +
  tm_raster(palette = "BrBG",
            style = "cat",
            breaks= seq(-5,5,1),
            title = "Residual\n Years\n (trend)") +
  tm_shape(admin0) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 5, "5 (model early)", ifelse(x == -5, "-5 (model late)", x))),
            scale = 1.4)

map_resids2_bra <- tm_shape(admin0, bbox = extent(approxras_bra)) +
  tm_fill(col = rgb(215/255,215/255,215/255)) +
  tm_shape(brazil) +
  tm_fill(col = rgb(150/255,150/255,150/255)) +
  tm_shape(approxras_resid_bra) +
  tm_raster(palette = "BrBG",
            style = "cat",
            breaks= seq(-5,5,1),
            title = "Residual\n Years\n (trend)") +
  tm_shape(admin0) +
  tm_borders(lwd = 0.1) +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 5, "5 (model early)", ifelse(x == -5, "-5 (model late)", x))),
            scale = 1.4)

tmap_save(map_resids2, filename = "Plots/Jan24/Mex_contemporary_residuals_smooth.png")
tmap_save(map_resids2_bra, filename = "Plots/Jan24/Bra_contemporary_residuals_smooth.png")





