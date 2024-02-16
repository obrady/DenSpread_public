################################################################################
# Plots for proceessing into an animation of past and future spread
################################################################################

# Oliver Brady
# 6 February 2023


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







# Mexico current and future predictions
dat <- read.csv("Predictions/Mex-Fig1-ContemporaryPredsJan24.csv")
# trim to just those predicted invaded in the past
dat = dat[!is.na(dat$YearPredicted), ]
FP_Mex <- read.csv("Predictions/Mex-Fig3-FuturePredsJan24.csv")
FP_Mex = FP_Mex[FP_Mex$YearInfected > 2019, ]
FP_Mex = FP_Mex[!is.na(FP_Mex$YearInfected), ]
FP_Mex = FP_Mex[!(FP_Mex$GAUL %in% dat$GAUL), ]

# Brazil historical, current and future predictions
# past
D2 <- read.csv("Predictions/Bra-Fig2-S6-RioFortManJan24.csv")
D2 = D2[!is.na(D2$YearInfected), ]
D2 = D2[D2$YearInfected < 2001, ] # take the 2001 predictions from the current-day model

# present
dat_bra <- read.csv("Predictions/Bra-Fig1-ContemporaryPredsJan24.csv")
dat_bra = dat_bra[!is.na(dat_bra$YearPredicted), ]
dat_bra = dat_bra[!(dat_bra$GAUL %in% D2$GAUL), ]

# future
FP_Bra <- read.csv("Predictions/Bra-Fig3-FuturePredsJan24.csv")
FP_Bra = FP_Bra[FP_Bra$YearInfected > 2019, ]
FP_Bra = FP_Bra[!is.na(FP_Bra$YearInfected), ]
FP_Bra = FP_Bra[!(FP_Bra$GAUL %in% c(D2$GAUL, dat_bra$GAUL)), ]


# admin shapefiles
admin2 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))

admin2_bra <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
admin2_bra = admin2_bra[admin2_bra$COUNTRY_ID == "BRA", ]

admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")


# Mexico 
# assign final invasion date to each shapefile
# current predictions first
admin2$Year = dat$YearPredicted[match(admin2$GAUL_CODE, dat$GAUL)]
# future matching index
f_match <- match(admin2$GAUL_CODE[is.na(admin2$Year)], FP_Mex$GAUL)
admin2$Year[is.na(admin2$Year)] = FP_Mex$YearInfected[f_match]

# Brazil
# assign final invasion date to each shapefile
# current predictions first
admin2_bra$Year = D2$YearInfected[match(admin2_bra$GAUL_CODE, D2$GAUL)]

# current matching index
c_match <- match(admin2_bra$GAUL_CODE[is.na(admin2_bra$Year)], dat_bra$GAUL)
admin2_bra$Year[is.na(admin2_bra$Year)] = dat_bra$YearPredicted[c_match]

# future matching index
f_match <- match(admin2_bra$GAUL_CODE[is.na(admin2_bra$Year)], FP_Bra$GAUL)
admin2_bra$Year[is.na(admin2_bra$Year)] = FP_Bra$YearInfected[f_match]



# Mexico map loop
year_range = sort(unique(admin2$Year))
setwd("/Users/eideobra/Desktop/temp/")

for(i in 1:length(year_range)){
  # subset to just those invaded in this year or previous
  admin2_y <- admin2
  admin2_y$Invaded = admin2_y$Year < year_range[(i + 1)]
  admin2_y$Invaded[admin2_y$Invaded == FALSE]=NA
  
  #map_preds <- tm_shape(admin0, bbox = extent(admin2_y)) +
  #  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  #  tm_shape(admin2_y) +
  #  tm_fill(col = "Invaded", palette = "OrRd", style = "cont", title = year_range[i],
  #          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent",
  #          legend.show = FALSE) +
  #  tm_layout(title.position = c("left","bottom"),
  #            scale = 1.6,
  #            title= year_range[i])
  
  map_preds <- tm_shape(admin2_y) +
    tm_fill(col = "Invaded", palette = "OrRd", style = "cont", title = year_range[i],
            colorNA = rgb(150/255,150/255,150/255), textNA = "Absent",
            legend.show = FALSE) +
    tm_layout(title.position = c("left","bottom"),
              scale = 1.6,
              title= year_range[i])
  
  # saving
  file_n <- paste0("Mexico_", year_range[i], ".png")
  tmap_save(map_preds, filename = file_n, dpi = 300)
}





# Brazil map loop
year_range = sort(unique(admin2_bra$Year))
year_range_c = min(year_range):max(year_range)

for(i in 1:length(year_range_c)){
  # subset to just those invaded in this year or previous
  admin2_y <- admin2_bra
  admin2_y$Invaded = admin2_y$Year < year_range_c[(i + 1)]
  admin2_y$Invaded[admin2_y$Invaded == FALSE]=NA
  
  map_preds <- tm_shape(admin2_y) +
    tm_fill(col = "Invaded", palette = "OrRd", style = "cont", title = year_range[i],
            colorNA = rgb(150/255,150/255,150/255), textNA = "Absent",
            legend.show = FALSE) +
    tm_layout(title.position = c("right","bottom"),
              scale = 1.6,
              title= year_range_c[i])
  
  # saving
  file_n <- paste0("Brazil_", year_range_c[i], ".png")
  tmap_save(map_preds, filename = file_n, dpi = 300)
}





