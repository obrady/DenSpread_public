################################################################################
# Plots maps of model-predicted invasion in the future period
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
require(MetBrewer)
require(exactextractr)


# working directory
setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")
# setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")


# load in model predictions
FP_Mex <- read.csv("Predictions/Mex-Fig3-FuturePredsJan24.csv")
FP_Bra <- read.csv("Predictions/Bra-Fig3-FuturePredsJan24.csv")

# summary statistics
# national total infected municipalities
bra_sum_stats <- apply(FP_Bra[, c("YearInfected", "YearInfectedLower", "YearInfectedUpper")],
                       2,
                       function(x) sum(!is.na(x)))
100 * bra_sum_stats / 5510

mex_sum_stats <- apply(FP_Mex[, c("YearInfected", "YearInfectedLower", "YearInfectedUpper")],
                       2,
                       function(x) sum(!is.na(x)))
100 * mex_sum_stats / 2456

# admin shapefiles
admin2 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))

admin2_bra <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
admin2_bra = admin2_bra[admin2_bra$COUNTRY_ID == "BRA", ]

admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")


# link shapfiles to data
admin2$Year = FP_Mex$YearInfected[match(admin2$GAUL_CODE, FP_Mex$GAUL)]
admin2_bra$Year = FP_Bra$YearInfected[match(admin2_bra$GAUL_CODE, FP_Bra$GAUL)]



# maps
map_preds <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "Year", palette = "OrRd", style = "cont", title = "Invasion\n year",
          breaks= c(2019, 2020, 2025, 2030, 2035, 2039),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("left","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 2019, "Before 2020", formatC(x, digits=0, format="d"))),
            scale = 1.6)

map_preds_bra <- tm_shape(admin0, bbox = extent(admin2_bra)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2_bra) +
  tm_fill(col = "Year", palette = "OrRd", style = "cont", title = "Invasion\n year",
          breaks= c(2019, 2020, 2025, 2030, 2035, 2039),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x == 2019, "Before 2020", formatC(x, digits=0, format="d"))),
            scale = 1.6)


tmap_save(map_preds, filename = "Plots/Jan24/Mex_Future.png")
tmap_save(map_preds_bra, filename = "Plots/Jan24/Bra_Future.png")

# save source data
write.csv(as.data.frame(admin2)[, c("ADM2_ES", "ADM2_PCODE", "GAUL_CODE", 
                                    "Year")], 
          file = "Figure_source_data/SD_Fig_7A.csv")
write.csv(as.data.frame(admin2_bra)[, c("NAME", "GAUL_CODE", 
                                        "Year")], 
          file = "Figure_source_data/SD_Fig_7C.csv")



# load in historical predictions (Brazil only)
D2 <- read.csv("Predictions/Bra-Fig2-S6-RioFortManJan24.csv")

# load in contemporary predictions
dat <- read.csv("Predictions/Mex-Fig1-ContemporaryPredsJan24.csv")
dat_bra <- read.csv("Predictions/Bra-Fig1-ContemporaryPredsJan24.csv")

### load in original data as it contains "Region" to link to GAUL codes
bra_OD <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
mex_OD <- read.csv("Data/Mexico_mun_dat_cleaned_thresholded.csv")

# note need mun info file for this informaiton for Brazilian municipalities
mun_info <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")


# summarise across datasets
bra_full <- rbind(as.matrix(cbind(D2[D2$YearInfected < 2001, c("GAUL", "YearInfected")], NA, NA)), 
                  as.matrix(cbind(dat_bra[dat_bra$YearPredicted < 2019, c("GAUL", "YearPredicted")], NA, NA)), 
                  as.matrix(FP_Bra[, c("GAUL", "YearInfected", "YearInfectedLower", "YearInfectedUpper")]))
colnames(bra_full)[3:4] = c("YearInfectedLower", "YearInfectedUpper")
bra_full_mid <- aggregate(YearInfected ~ GAUL, data = bra_full, FUN = min)
bra_full_low <- aggregate(YearInfectedLower ~ GAUL, data = bra_full, FUN = min)
bra_full_upper <- aggregate(YearInfectedUpper ~ GAUL, data = bra_full, FUN = min)
bra_full = bra_full_upper
bra_full$YearInfectedLower = bra_full_low$YearInfectedLower[match(bra_full$GAUL, bra_full_low$GAUL)]
bra_full$YearInfected = bra_full_mid$YearInfected[match(bra_full$GAUL, bra_full_mid$GAUL)]
# set CIs to observed value before 2020
bra_full$YearInfectedUpper[bra_full$YearInfectedUpper < 2020] = bra_full$YearInfected[bra_full$YearInfectedUpper < 2020]
index = is.na(bra_full$YearInfectedLower) | (bra_full$YearInfectedLower < 2020)
bra_full$YearInfectedLower[index] = bra_full$YearInfected[index]


mex_full <- rbind(cbind(as.matrix(dat[, c("GAUL", "YearPredicted")]), NA, NA), 
                  as.matrix(FP_Mex[, c("GAUL", "YearInfected", "YearInfectedLower", "YearInfectedUpper")]))
colnames(mex_full)[3:4] = c("YearPredictedLower", "YearPredictedUpper")
mex_full_mid <- aggregate(YearPredicted ~ GAUL, data = mex_full, FUN = min)
mex_full_low <- aggregate(YearPredictedLower ~ GAUL, data = mex_full, FUN = min)
mex_full_upper <- aggregate(YearPredictedUpper ~ GAUL, data = mex_full, FUN = min)
mex_full = mex_full_upper
mex_full$YearPredictedLower = mex_full_low$YearPredictedLower[match(mex_full$GAUL, mex_full_low$GAUL)]
mex_full$YearPredicted = mex_full_mid$YearPredicted[match(mex_full$GAUL, mex_full_mid$GAUL)]



# add region and summarise then convert to cumulative counts
bra_full$Region = mun_info$Region[match(bra_full$GAUL, mun_info$GAUL_CODE)]

u_regions <- c("South", "Southeast", "Northeast", "North", "Centralwest")
years = 1983:2039

bra_RS <- data.frame(Year = rep(years, 5),
                     Region = rep(u_regions, each = length(years)),
                     Cum_count_mid = NA,
                     Cum_count_lower = NA,
                     Cum_count_upper = NA)

for(i in 1:length(u_regions)){
  ss_bra_full <- bra_full[bra_full$Region == u_regions[i], ]
  for(k in 1:length(years)){
    bra_RS$Cum_count_mid[((i - 1) * length(years) + k)] = sum(ss_bra_full$YearInfected <= years[k], na.rm = T)
    bra_RS$Cum_count_lower[((i - 1) * length(years) + k)] = sum(ss_bra_full$YearInfectedLower <= years[k], na.rm = T)
    bra_RS$Cum_count_upper[((i - 1) * length(years) + k)] = sum(ss_bra_full$YearInfectedUpper <= years[k], na.rm = T)
  }
}

# reorder to put South on top
bra_RS$NewOrder = as.numeric(bra_RS$Region)
bra_RS$NewOrder[bra_RS$Region == "Centralwest"] = 4
bra_RS$NewOrder[bra_RS$Region == "South"] = 1
bra_RS$Region = with(bra_RS, reorder(Region, NewOrder, median))


# assign cumulative totals across brazil to southern region for error bar plotting and change to NA for all others
nat_low = aggregate(Cum_count_lower ~ Year, FUN = sum, data = bra_RS)[, 2]
nat_high = aggregate(Cum_count_upper ~ Year, FUN = sum, data = bra_RS)[, 2]
bra_RS$Cum_count_lower = bra_RS$Cum_count_upper = NA
bra_RS$Cum_count_lower[1:length(nat_low)] = nat_low
bra_RS$Cum_count_upper[1:length(nat_high)] = nat_high

# error bars only for 2020 onwards
bra_RS$Cum_count_lower[bra_RS$Year < 2020] = NA
bra_RS$Cum_count_upper[bra_RS$Year < 2020] = NA

p1 <- ggplot(bra_RS, aes(fill=Region, y=Cum_count_mid, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=met.brewer("Peru1", 5)) +
  geom_errorbar(aes(ymin = Cum_count_lower, ymax = Cum_count_upper), width = 0.2) +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000, 5510), limits = c(0, 5510)) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2020, 2030, 2040), limits = c(1982, 2040)) +
  ylab("Areas invaded") +
  theme_minimal()

ggsave(p1, filename = "Plots/Jan24/Futurecase_histo_bra.png", height = 3, width = 6)

# save source data
write.csv(bra_RS, file = "Figure_source_data/SD_Fig_7D.csv")





# load in mexican elevation data
# source: https://www.diva-gis.org/gdata 
r <- raster("Data/MEX_msk_alt/MEX_msk_alt.grd")


admin2$Avg_elevation <-exact_extract(r, admin2, "mean")


# add elevation data
mex_full$Elevation = admin2$Avg_elevation[match(mex_full$GAUL, admin2$GAUL)]
mex_full$Elevation = mex_full$Elevation > 1000

u_opts <- c(FALSE, TRUE)
years = 1995:2039


mex_RS <- data.frame(Year = rep(years, 2),
                     Elevation = rep(u_opts, each = length(years)),
                     Cum_count_mid = NA,
                     Cum_count_lower = NA,
                     Cum_count_upper = NA)

for(i in 1:length(u_opts)){
  ss_mex_full <- mex_full[mex_full$Elevation == u_opts[i], ]
  for(k in 1:length(years)){
    mex_RS$Cum_count_mid[((i - 1) * length(years) + k)] = sum(ss_mex_full$YearPredicted <= years[k], na.rm = T)
    mex_RS$Cum_count_lower[((i - 1) * length(years) + k)] = sum(ss_mex_full$YearPredictedLower <= years[k], na.rm = T)
    mex_RS$Cum_count_upper[((i - 1) * length(years) + k)] = sum(ss_mex_full$YearPredictedUpper <= years[k], na.rm = T)
  }
}

mex_RS$NewOrder = as.numeric(mex_RS$Elevation)
#m1 <- mex_RS$NewOrder == 0
#m2 <- mex_RS$NewOrder == 1
#mex_RS$NewOrder[m1] = 1
#mex_RS$NewOrder[m2] = 0
mex_RS$Elevation = with(mex_RS, reorder(Elevation, NewOrder, median))

mex_RS$Elevation = c("Below 1000m", "Above 1000m")[mex_RS$Elevation]

# assign cumulative totals across mexico to "Above 1000m for error bar plotting and change to NA for all others
nat_low = aggregate(Cum_count_lower ~ Year, FUN = sum, data = mex_RS)[, 2]
nat_high = aggregate(Cum_count_upper ~ Year, FUN = sum, data = mex_RS)[, 2]
mex_RS$Cum_count_lower = mex_RS$Cum_count_upper = NA
mex_RS$Cum_count_lower[1:length(nat_low)] = nat_low
mex_RS$Cum_count_upper[1:length(nat_high)] = nat_high
# error bars only for 2020 onwards
mex_RS$Cum_count_lower[mex_RS$Year < 2020] = NA
mex_RS$Cum_count_upper[mex_RS$Year < 2020] = NA


p2 <- ggplot(mex_RS, aes(fill=Elevation, y=Cum_count_mid, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=met.brewer("Peru1", 5)) +
  geom_errorbar(aes(ymin = Cum_count_lower, ymax = Cum_count_upper), width = 0.2) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2456), limits = c(0, 2456)) +
  scale_x_continuous(breaks = c(1995, 2000, 2010, 2020, 2030, 2040), limits = c(1994, 2040)) +
  ylab("Areas invaded") +
  theme_minimal()
p2

ggsave(p2, filename = "Plots/Jan24/Futurecase_histo_mex.png", height = 3, width = 6)

# save source data
write.csv(mex_RS, file = "Figure_source_data/SD_Fig_7B.csv")







