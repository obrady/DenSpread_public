
# part 1, defining cities in each Brazilian municipality

rm(list = ls())

require(dplyr)
require(sf)
require(raster)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")

# load in Brazil admin2 areas and worldpop surfaces
Brazil <- st_read("Reference/Admin2(2011)/admin2.shp")
Brazil = filter(Brazil, COUNTRY_ID == "BRA")
#Brazil = Brazil[Brazil$PARENT_ID == 688, ] # !!!! temporary for testing - just filter to Santa Caterina for now
Mexico = st_read("Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp")

pop <- raster("Reference/Pop2015.grd")


# choose country
f_geog = Mexico

pop = mask(crop(pop, f_geog), f_geog)

# loop through each municiaplity and extract the coordinates of the most densely populated pixel
# takes about 90s
pop_centre <- data.frame(X_COORD = rep(NA, nrow(f_geog)),
                         Y_COORD = rep(NA, nrow(f_geog)))

for(i in 1:nrow(f_geog)){
  # focus population raster in administrative unit
  f_pop <- mask(crop(pop, f_geog[i, ]), f_geog[i, ])
  # assemble list of candidate coordiantes
  cand_coords <- coordinates(f_pop)
  # extract and save coordinates of most densely populated pixel
  f_pop_v = as.vector(f_pop)
  if(any(!is.na(f_pop_v))){
    pop_centre[i, ] = cand_coords[which.max(as.vector(f_pop)), ]
  }else{ # if no population data available, choose the centroid
    pop_centre[i, ] = st_coordinates(st_centroid(f_geog[i, ]))
  }
}

if(nrow(f_geog) == nrow(Brazil)){
  write.csv(pop_centre, file = "Covariates/Connectivity/MAP_Accessibility/Brazil_pop_centres.csv")
}
if(nrow(f_geog) == nrow(Mexico)){
  write.csv(pop_centre, file = "Covariates/Connectivity/MAP_Accessibility/Mexico_pop_centres.csv")
}




################################
# start code from MAP website
################################

# Accessibility Mapping in Google Earth Engine (GEE)
# 
# Dan Weiss, Malaria Atlas Project, University of Oxford
# 2017-11-06
#
# This script requires the gdistance package (van Etten, J. R Package gdistance: Distances and Routes on Geographical Grids. Journal of Statistical Software 76, 1-21)
#
# This script requires the two user supplied datasets:
# (a) The friction surface, which is available here:  http://www.map.ox.ac.uk/accessibility_to_cities/
# (b) A user-supplied .csv of points (i.e., known geographic coordinates) 
#
# Notes:
# (a) All file paths and names should be changed as needed.
# (b) Important runtime details can be found in the comments.
# (c) This script is suitable only for analyses of moderately sized areas for most (e.g., up to 10 million km^2 in lower latitude settings - GLOBAL RUNS WILL NOT WORK).
#     We recommend using Google Earth Engine for larger areas, with the exception of high-latitude areas where custom approaches are typically required.
#
# Citation: D.J. Weiss, A. Nelson, H.S. Gibson, W. Temperley, S. Peedell, A. Lieber, M. Hancher, E. Poyart, S. Belchior, N. Fullman, B. Mappin, U. Dalrymple, J. Rozier, 
# T.C.D. Lucas, R.E. Howes, L.S. Tusting, S.Y. Kang, E. Cameron, D. Bisanzio, K.E. Battle, S. Bhatt, and P.W. Gething. A global map of travel time to cities to assess 
# inequalities in accessibility in 2015. (2018). Nature. doi:10.1038/nature25181.
# 

## Required Packages
require(gdistance)

# User Defined Variables - used if clipping from the global layer, if no clipping is needed, see lines 54-55 (currently commented out).
# This could also be accomplished by importing a shapefile (for example) 
# Geographic Coordinates (WGS84)
left   <- extent(f_geog)[1]
right  <- extent(f_geog)[2]
bottom <- extent(f_geog)[3]
top    <- extent(f_geog)[4]
transition.matrix.exists.flag <- 0 # if the geo-corrected graph has already been made, this can save time.  Uses the same T.GC.filename as specified using the T.GC.filename variable.

# Input and Output Files
friction.surface.filename <- "Covariates/Connectivity/MAP_Accessibility/2015_friction_surface_v1.geotiff"
if(nrow(f_geog) == nrow(Brazil)){
  point.filename <- "Covariates/Connectivity/MAP_Accessibility/Brazil_pop_centres.csv" # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.
  T.filename <- 'Covariates/Connectivity/MAP_Accessibility/Brazil_admin_connect.T.rds'
  T.GC.filename <- 'Covariates/Connectivity/MAP_Accessibility/Brazil_admin_connect.T.GC.rds'
  output.filename <- 'Covariates/Connectivity/MAP_Accessibility/Brazil_admin_connect.accessibility.tif'
}
if(nrow(f_geog) == nrow(Mexico)){
  point.filename <- "Covariates/Connectivity/MAP_Accessibility/Mexico_pop_centres.csv" # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.
  T.filename <- 'Covariates/Connectivity/MAP_Accessibility/Mexico_admin_connect.T.rds'
  T.GC.filename <- 'Covariates/Connectivity/MAP_Accessibility/Mexico_admin_connect.T.GC.rds'
  output.filename <- 'Covariates/Connectivity/MAP_Accessibility/Mexico_admin_connect.accessibility.tif'
}


# Read in the points table
points <- read.csv(file = point.filename)
points = points[, c("X_COORD", "Y_COORD")]
# ! just for testing
#points = points[1, ]

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]



#  Define the spatial template
friction <- raster(friction.surface.filename)
# cropping
fs1 <- crop(friction, extent(left, right, bottom, top))
# aggregation to 5km
friction_5km <- aggregate(friction, fact = 5, fun = sum)


# Use the following line instead of the preceding 2 if clipping is not needed (i.e., to run globally), but be warned that trying this will far exceed the computational capacity available to most users.
# fs1 <- raster(friction.surface.filename) 

# Make the graph and the geocorrected version of the graph (or read in the latter).
if (transition.matrix.exists.flag == 1) {
  # Read in the transition matrix object if it has been pre-computed
  T.GC <- readRDS(T.GC.filename)
} else {
  # Make and geocorrect the transition matrix (i.e., the graph)
  T <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
  saveRDS(T, T.filename)
  T.GC <- geoCorrection(T)                    
  saveRDS(T.GC, T.GC.filename)
}

# Convert the points into a matrix
xy.data.frame <- data.frame()
xy.data.frame[1:n.points,1] <- points[,1]
xy.data.frame[1:n.points,2] <- points[,2]
xy.matrix <- as.matrix(xy.data.frame)



# build the distance matrix
pop_centre_mat <- matrix(NA, nrow = nrow(xy.matrix), ncol = nrow(xy.matrix))
na.ind <- !is.na(xy.matrix[, 1])

for(i in 1:nrow(xy.matrix)){
  if(!is.na(xy.matrix[i, 1])){
    pop_centre_mat[i, na.ind] = costDistance(T.GC, xy.matrix[i, ], na.omit(xy.matrix))
  }
}

# now save the movement matrix
if(nrow(f_geog) == nrow(Brazil)){
  rownames(pop_centre_mat) = Brazil$GAUL_CODE
  colnames(pop_centre_mat) = Brazil$GAUL_CODE
  write.csv(pop_centre_mat, file = "Covariates/Connectivity/MAP_Accessibility/Brazil_accessibility_matrix.csv")
}

if(nrow(f_geog) == nrow(Mexico)){
  rownames(pop_centre_mat) = Mexico$ADM2_PCODE
  colnames(pop_centre_mat) = Mexico$ADM2_PCODE
  write.csv(pop_centre_mat, file = "Covariates/Connectivity/MAP_Accessibility/Mexico_accessibility_matrix.csv")
}

