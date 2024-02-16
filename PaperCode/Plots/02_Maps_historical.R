################################################################################
# Plots maps of model-predicted invasion in the historical period
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


# working directory
setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")
# setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")


# load predictions from the model
#A2 <- read.csv("Predictions/Bra-Fig2-S0-Rio83Jan24.csv")
B2 <- read.csv("Predictions/Bra-Fig2-S6-RioFortManJan24.csv")
#C2 <- read.csv("Predictions/2c.csv")
#D2 <- read.csv("Predictions/2d.csv")

# summary statistics for all scenarios
sum_stat <- read.csv("Predictions/Brazil_source_comparison.csv")

# ground truth (areas infected in 2001)
bra_OD <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
bra_OD = bra_OD[bra_OD$Year == 2001, ]

# administrative units
admin2 <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
admin2 = admin2[admin2$COUNTRY_ID == "BRA", ]

admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")


# assign predicitons to admin unit shapefiles
#admin2$YPA2 = A2$YearInfected[match(admin2$GAUL_CODE, A2$GAUL)]
admin2$YPB2 = B2$YearInfected[match(admin2$GAUL_CODE, B2$GAUL)]
#admin2$YPC2 = C2$YearInfected[match(admin2$GAUL_CODE, C2$GAUL)]
#admin2$YPD2 = D2$YearInfected[match(admin2$GAUL_CODE, D2$GAUL)]
admin2$Observed = admin2$GAUL_CODE %in% bra_OD$GAUL_A2



# maps for different hypothessied origins

#map_predsA2 <- tm_shape(admin0, bbox = extent(admin2)) +
#  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
#  tm_shape(admin2) +
#  tm_fill(col = "YPA2", palette = "Spectral", style = "cont", title = "Invasion\n year",
#          breaks = c(1983, 1990, 1995, 2000, 2001),
#          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
#  tm_layout(legend.position = c("right","bottom"),
#            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
#            scale = 1.8)

map_predsB2 <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "YPB2", palette = "Spectral", style = "cont", title = "Invasion\n year",
          breaks = c(1983, 1990, 1995, 2000, 2001),
          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
            scale = 1.8)

#map_predsC2 <- tm_shape(admin0, bbox = extent(admin2)) +
#  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
#  tm_shape(admin2) +
#  tm_fill(col = "YPC2", palette = "Spectral", style = "cont", title = "Invasion\n year",
#          breaks = c(1986, 1990, 1995, 2000, 2001),
#          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
#  tm_layout(legend.position = c("right","bottom"),
#            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
#            scale = 1.8)

#map_predsD2 <- tm_shape(admin0, bbox = extent(admin2)) +
#  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
#  tm_shape(admin2) +
#  tm_fill(col = "YPD2", palette = "Spectral", style = "cont", title = "Invasion\n year",
#          breaks = c(1986, 1990, 1995, 2000, 2001),
#          colorNA = rgb(150/255,150/255,150/255), textNA = "Absent") +
#  tm_layout(legend.position = c("right","bottom"),
#            legend.format=list(fun=function(x) formatC(x, digits=0, format="d")),
#            scale = 1.8)

map_predsObserved <- tm_shape(admin0, bbox = extent(admin2)) +
  tm_polygons(lwd = 0.1, col = rgb(215/255,215/255,215/255)) +
  tm_shape(admin2) +
  tm_fill(col = "Observed", 
          palette = c(rgb(150/255,150/255,150/255),
                      rgb(158/255,1/255,66/255)),
          labels = c("Dengue absent", "Dengue present"),
          title = "Observed in 2001") +
  tm_layout(legend.position = c("right","bottom"),
            legend.format=list(fun=function(x) ifelse(x, "Dengue present", "Dengue absent")),
            scale = 1.8)

#tmap_save(map_predsA2, filename = "Plots/Jan24/Origins_1.png")
tmap_save(map_predsB2, filename = "Plots/Jan24/Origins_2.png")
#tmap_save(map_predsC2, filename = "Plots/Origins_3.pdf")
#tmap_save(map_predsD2, filename = "Plots/Origins_4.pdf")
#tmap_save(map_predsObserved, filename = "Plots/Jan24/Origins_Observed.png")




#### Stacked bar comparison graph

#GT <- admin2$GAUL_CODE %in% bra_OD$GAUL_A2

# convert prediitons into TN, TP, FN, FP
#logicalPred <- !is.na(admin2$YPA2)
#logicalActual <- GT

# function for summarising predicitons
sum.stats.calc <- function(logicalPred, logicalActual){
  # counts
  TN <- sum((logicalPred == FALSE) & (logicalActual == FALSE))
  TP <- sum((logicalPred == TRUE) & (logicalActual == TRUE))
  FN <- sum((logicalPred == FALSE) & (logicalActual == TRUE))
  FP <- sum((logicalPred == TRUE) & (logicalActual == FALSE))
  
  # rates
  Tot <- sum(TN, TP, FN, FP)
  TNP = TN / Tot
  TPP = TP / Tot
  FNP = FN / Tot
  FPP = FP / Tot
  
  # accuracy
  ACC = (TP + TN) / (TP + TN + FP + FN)
  # sensitivity
  SN = TP / (TP + FN)
  # specificity
  SP = TN / (TN + FP)
  # PPV and NPV
  PPV = TP / (TP + FP)
  NPV = TN / (TN + FN)
  
  # return object
  #rtn_obj <- data.frame(TN = TNP, TP = TPP, FN = FNP, FP = FPP)
  #rtn_obj <- c(TNP, TPP, FNP, FPP)
  rtn_obj <- c(ACC, SN, SP, PPV, NPV)
  
  return(rtn_obj)
}





#comp_df <- data.frame(Type = rep(c("Accuracy",
#                                   "Sensitivity",
#                                   "Specificity",
#                                   "PPV",
#                                   "NPV"), 4),
#                      Source = c(rep("FOR-RIO", 5),
#                                 rep("FOR-RIO-RP", 5),
#                                 rep("FOR-RIO-MAN", 5),
#                                 rep("FOR-RIO-RP-MAN", 5)),
#                      Count = c(sum.stats.calc(!is.na(admin2$YPA2), GT),
#                                sum.stats.calc(!is.na(admin2$YPB2), GT),
#                                sum.stats.calc(!is.na(admin2$YPC2), GT),
#                                sum.stats.calc(!is.na(admin2$YPD2), GT)))

# trim scenario summary table down to best predictions for each number of sources
colnames(sum_stat)[1] = "Scenario"
sum_stat = sum_stat[, c(1:3, 5:8)] # remove balanced accuracy
comp_df = sum_stat[sum_stat$Scenario %in% c(0.1, 1, 6, 13, 15), ]
comp_df$Sources[comp_df$Sources == "RIO83"] = "RIO"

# % improvement in each metric relative to 2 sources
ref <- comp_df[1, 3:7]

comp_df[1, 3:7] = 100 * (comp_df[1, 3:7] - ref) / ref
comp_df[2, 3:7] = 100 * (comp_df[2, 3:7] - ref) / ref
comp_df[3, 3:7] = 100 * (comp_df[3, 3:7] - ref) / ref
comp_df[4, 3:7] = 100 * (comp_df[4, 3:7] - ref) / ref
comp_df[5, 3:7] = 100 * (comp_df[5, 3:7] - ref) / ref


# reformat table
comp_df = data.frame(Type = rep(c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV"), 5),
                     Source = rep(comp_df$Sources, each = 5),
                     Count = as.vector(t(as.matrix(comp_df[, 3:7]))))

p1 <- ggplot(comp_df, aes(fill=Type, y=Count, x=Source)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Improvement compared to RIO only (%)") +
  xlab("Introduction sources") +
  scale_fill_manual(values=met.brewer("Hokusai3", 5)) +
  theme_minimal() +
  guides(fill=guide_legend(title=""))

ggsave(p1, filename = "Plots/Jan24/Source_metrics.png", width = 6, height = 4)
