################################################################################
# assemble fitting and valdiation datasets and combine them with covariate data
################################################################################



rm(list = ls())

require(raster)
require(rgdal)
require(rgeos)
require(geosphere)
require(tmap)
require(maptools)
require(spdep)
require(movement)
require(pROC)
require(mgcv)
require(ggplot2)
require(sf)

setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")
#setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")


########################
# part 1 loading data
########################

# choose country
country = "Brazil"

if(country == "Brazil"){
  # admin shapefile
  admin0 <- st_read(dsn = "Reference/Admin0(2011)/admin0.shp", layer = "admin0")
  admin1 <- st_read(dsn = "Reference/Admin1(2011)/admin1.shp", layer = "admin1")
  admin2 <- st_read(dsn = "Reference/Admin2(2011)/admin2.shp", layer = "admin2")
  # trim to Brazil
  admin0 = admin0[admin0$NAME == "Brazil", ]
  admin1 = admin1[admin1$COUNTRY_ID == "BRA", ]
  admin2 = admin2[admin2$COUNTRY_ID == "BRA", ]
}

if(country == "Mexico"){
  admin0 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm0_govmex_20200618.shp", layer = "mex_admbnda_adm0_govmex_20200618")
  admin1 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm1_govmex_20200618.shp", layer = "mex_admbnda_adm1_govmex_20200618")
  admin2 = st_read(dsn = "Data/Mexico_dat/MEX_shapefiles/mex_admbnda_adm2_govmex_20200618.shp", layer = "mex_admbnda_adm2_govmex_20200618")
  admin2$GAUL_CODE = as.numeric(gsub("MX", "", admin2$ADM2_PCODE))
}



# 1A- covariates and reference datasets

if(country == "Brazil"){
  # Aedes suitability map (non-time varying)
  load("Data/Intermediate_datasets/Brazil_Aegypti_sum.RData")
  
  # temporally varying MAP covariates:
  load("Data/Intermediate_datasets/Brazil_EVI_sum.RData")
  load("Data/Intermediate_datasets/Brazil_LandCover_sum.RData")
  load("Data/Intermediate_datasets/Brazil_LST_day_sum.RData")
  load("Data/Intermediate_datasets/Brazil_LST_night_sum.RData")
  load("Data/Intermediate_datasets/Brazil_TCB_sum.RData")
  load("Data/Intermediate_datasets/Brazil_TCW_sum.RData")

  ### dengue data
  dat <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
  
  # A) simple great circle distance for now
  #coords <- gCentroid(admin2,byid=TRUE)
  #dist_mat_GC <- distm(coords)
  #save(dist_mat_GC, file = "Data/Intermediate_datasets/dist_mat_GC_Brazil.RData")
  load("Data/Intermediate_datasets/dist_mat_GC_Brazil.RData")
  # GC transformation (emphasises close places)
  dist_mat_GC = 1 / (dist_mat_GC + 1)
  
  # B) gravity model
  load("Data/Intermediate_datasets/Gravity_matrix_Brazil.RData")
  
  # c) Radiation model
  load("Data/Intermediate_datasets/Radiation_matrix_Brazil.RData")
  
  # D) Adjacency model
  load("Data/Intermediate_datasets/dist_mat_Adjacency_Brazil.RData")
  
  # E) flight data
  load("Data/Intermediate_datasets/Brazil_air_move_mat_ad2.RData")
  
  # F) Worldpop internal migration flows (state level)
  load("Data/Intermediate_datasets/Brazil_Migration_mat.RData")
  
  # G) MAP friction surface- travel time between biggest city in each municipality
  load("Data/Intermediate_datasets/Brazil_Friction_mat.RData")
  # Friction transformation (emphasises accessible places)
  dist_mat_Friction = 1 / (dist_mat_Friction + 1)
  
  # Future and past projection annual multipliers
  LST_day_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanDayTemperature_Brazil.csv")
  LST_day_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDDayTemperature_Brazil.csv")
  LST_night_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanNightTemperature_Brazil.csv")
  LST_night_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDNightTemperature_Brazil.csv")
  EVI_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanEVI_Brazil.csv")
  EVI_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDEVI_Brazil.csv")
  TCW_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDTCW_Brazil.csv")
  Movement_FP <- read.csv("Covariates/Future_projections/predictions/Movement_Brazil.csv")
  
  
  # exclude self connections in all distance matrices
  diag(dist_mat_Adjacency) <- diag(dist_mat_Air) <- diag(dist_mat_Friction) <- NA
  diag(dist_mat_GC) <- diag(dist_mat_Grav) <- diag(dist_mat_Migration) <- diag(dist_mat_Rad) <- NA
  
  # renaming covariates to be country non-specific
  Aegypti_sum = Brazil_Aegypti_sum
  EVI_sum = Brazil_EVI_sum
  LandCover_sum = Brazil_LandCover_sum
  LST_day_sum = Brazil_LST_day_sum
  LST_night_sum = Brazil_LST_night_sum
  TCB_sum = Brazil_TCB_sum
  TCW_sum = Brazil_TCW_sum
  
  # remove 2015 values for LST and EVI due to anomalous values not supported by ground observations 
  # (annual trend still compensated due to future projection analysis)
  EVI_sum = EVI_sum[, !grepl("2015", colnames(EVI_sum))]
  LST_day_sum = LST_day_sum[, !grepl("2015", colnames(LST_day_sum))]
  LST_night_sum = LST_night_sum[, !grepl("2015", colnames(LST_night_sum))]
  
  # list of big cities
  Big_cities <- read.csv("Data/Intermediate_datasets/Brazil_big_cities.csv")
}

if(country == "Mexico"){
  # Aedes suitability map (non-time varying)
  load("Data/Intermediate_datasets/Mexico_Aegypti_sum.RData")
  
  # temporally varying MAP covariates:
  load("Data/Intermediate_datasets/Mexico_EVI_sum.RData")
  load("Data/Intermediate_datasets/Mexico_LandCover_sum.RData")
  load("Data/Intermediate_datasets/Mexico_LST_day_sum.RData")
  load("Data/Intermediate_datasets/Mexico_LST_night_sum.RData")
  load("Data/Intermediate_datasets/Mexico_TCB_sum.RData")
  load("Data/Intermediate_datasets/Mexico_TCW_sum.RData")
  
  ### dengue data
  dat <- read.csv("Data/Mexico_mun_dat_cleaned_thresholded.csv")
  
  # A) simple great circle distance for now
  #coords <- gCentroid(admin2,byid=TRUE)
  #dist_mat_GC <- distm(coords)
  #save(dist_mat_GC, file = "Data/Intermediate_datasets/dist_mat_GC_Mexico.RData")
  load("Data/Intermediate_datasets/dist_mat_GC_Mexico.RData")
  # GC transformation (emphasises close places)
  dist_mat_GC = 1 / (dist_mat_GC + 1)
  
  # B) gravity model
  load("Data/Intermediate_datasets/Gravity_matrix_Mexico.RData")
  
  # c) Radiation model
  load("Data/Intermediate_datasets/Radiation_matrix_Mexico.RData")
  
  # D) Adjacency model
  load("Data/Intermediate_datasets/dist_mat_Adjacency_Mexico.RData")
  
  # E) flight data
  load("Data/Intermediate_datasets/Mexico_air_move_mat_ad2.RData")
  
  # F) Worldpop internal migration flows (state level)
  load("Data/Intermediate_datasets/Mexico_Migration_mat.RData")
  
  # G) MAP friction surface- travel time between biggest cities in each municiaplity
  load("Data/Intermediate_datasets/Mexico_Friction_mat.RData")
  # Friction transformation (emphasises accessible places)
  dist_mat_Friction = 1 / (dist_mat_Friction + 1)
  
  # Future and past projection annual multipliers
  LST_day_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanDayTemperature_Mexico.csv")
  LST_day_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDDayTemperature_Mexico.csv")
  LST_night_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanNightTemperature_Mexico.csv")
  LST_night_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDNightTemperature_Mexico.csv")
  EVI_m_FP <- read.csv("Covariates/Future_projections/predictions/MeanEVI_Mexico.csv")
  EVI_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDEVI_Mexico.csv")
  TCW_sd_FP <- read.csv("Covariates/Future_projections/predictions/SDTCW_Mexico.csv")
  Movement_FP <- read.csv("Covariates/Future_projections/predictions/Movement_Mexico.csv")
  
  
  # exclude self connections in all distance matrices
  diag(dist_mat_Adjacency) <- diag(dist_mat_Air) <- diag(dist_mat_Friction) <- NA
  diag(dist_mat_GC) <- diag(dist_mat_Grav) <- diag(dist_mat_Migration) <- diag(dist_mat_Rad) <- NA
  
  # renaming covariates to be country non-specific
  Aegypti_sum = Mexico_Aegypti_sum
  EVI_sum = Mexico_EVI_sum
  LandCover_sum = Mexico_LandCover_sum
  LST_day_sum = Mexico_LST_day_sum
  LST_night_sum = Mexico_LST_night_sum
  TCB_sum = Mexico_TCB_sum
  TCW_sum = Mexico_TCW_sum
  
  # remove 2015 values for LST and EVI due to anomalous values not supported by ground observations 
  # (annual trend still compensated due to future projection analysis)
  EVI_sum = EVI_sum[, !grepl("2015", colnames(EVI_sum))]
  LST_day_sum = LST_day_sum[, !grepl("2015", colnames(LST_day_sum))]
  LST_night_sum = LST_night_sum[, !grepl("2015", colnames(LST_night_sum))]
  
  # list of big cities
  Big_cities <- read.csv("Data/Intermediate_datasets/Mexico_big_cities.csv")
}




















# functions
#spreadYear must be in range 1980:2040
add.Covariates <- function(GAULS_SOURCE, GAULS_DEST, spreadYear, Big_cities = NULL){
  
  # spreadyear warning
  if((spreadYear > 2040) | (spreadYear < 1980)){print("warning, spread year needs to be between 1980 and 2040")}
  
  # calculate cumulative connectivity
  # connectivity (GC, Gravity and radiation)
  # columns = destinations 
  # rows = sources
  con1 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con2 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con3 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con4 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con5 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con6 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  con7 <- matrix(0, nrow(dist_mat_GC), ncol = nrow(dist_mat_GC))
  
  # find matching columns of sources
  adMATCH_found = match(GAULS_SOURCE, admin2$GAUL_CODE)
  
  # enter connectivity for source columns
  con1[adMATCH_found, ] = dist_mat_GC[adMATCH_found, ]
  con2[adMATCH_found, ] = dist_mat_Grav[adMATCH_found, ]
  con3[adMATCH_found, ] = dist_mat_Rad[adMATCH_found, ]
  con4[adMATCH_found, ] = dist_mat_Adjacency[adMATCH_found, ]
  con5[adMATCH_found, ] = dist_mat_Air[adMATCH_found, ]
  con6[adMATCH_found, ] = dist_mat_Migration[adMATCH_found, ]
  con7[adMATCH_found, ] = dist_mat_Friction[adMATCH_found, ]
  
  # extract detailed movement matrices for big cities during invasion years
  if(!missing(Big_cities)){
    if(spreadYear %in% Big_cities$Year){
      # identify areas infected this year
      ss_BC <- Big_cities[Big_cities$Year == spreadYear, ]
      Big_city_con <- list()
      for(k in 1:nrow(ss_BC)){
        col_match <- match(ss_BC$GAUL[k], admin2$GAUL_CODE)
        Big_city_con[[k]] <- rbind(log(con3[, col_match] + 1),
                                   con4[, col_match],
                                   log(con5[, col_match] + 1),
                                   log(con6[, col_match] + 1),
                                   log(con7[, col_match] + 1))
        names(Big_city_con[[k]]) = ss_BC$GAUL[k]
      }
    }
  }
  
  # calculate the sum of the flux from all connected areas or max from most conected
  cpy1 = apply(con1, 2, max, na.rm = T)
  cpy2 = apply(con2, 2, max, na.rm = T)
  cpy3 = apply(con3, 2, max, na.rm = T)
  cpy4 = apply(con4, 2, max, na.rm = T)
  cpy5 = apply(con5, 2, max, na.rm = T)
  cpy6 = apply(con6, 2, max, na.rm = T)
  cpy7 = apply(con7, 2, max, na.rm = T)
  
  #cpy1 = apply(con1, 2, sum, na.rm = T)
  #cpy2 = apply(con2, 2, sum, na.rm = T)
  #cpy3 = apply(con3, 2, sum, na.rm = T)
  #cpy4 = apply(con4, 2, sum, na.rm = T)
  #cpy5 = apply(con5, 2, sum, na.rm = T)
  #cpy6 = apply(con6, 2, sum, na.rm = T)
  #cpy7 = apply(con7, 2, sum, na.rm = T)
  
  # apply year-based multiplier for connectivity
  cpy2 = cpy2 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  cpy3 = cpy3 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  # special case for Air hich is 2009-2019 average
  air_mult = mean(Movement_FP$Mult[Movement_FP$Year %in% 2009:2019])
  cpy5 = cpy5 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)] / air_mult
  cpy6 = cpy6 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  cpy7 = cpy7 / Movement_FP$Mult[match(spreadYear, Movement_FP$Year)] # divide instead of mutliply as lowe travel time = more accessible
  
  
  # now assemble a return data frame
  rtn_df <- data.frame(ae_suit = Aegypti_sum[match(GAULS_DEST, admin2$GAUL_CODE), 1],
                       EVI_mean = suit.assemble(EVI_sum, spreadYear, GAULS_DEST, FP = EVI_m_FP)[[1]],
                       EVI_stdDev = suit.assemble(EVI_sum, spreadYear, GAULS_DEST, FP = EVI_sd_FP)[[2]],
                       Landcover = suit.assemble(LandCover_sum, spreadYear, GAULS_DEST, FP = NA)[[1]],
                       LST_day_mean = suit.assemble(LST_day_sum, spreadYear, GAULS_DEST, FP = LST_day_m_FP)[[1]],
                       LST_day_stdDev = suit.assemble(LST_day_sum, spreadYear, GAULS_DEST, FP = LST_day_sd_FP)[[2]],
                       LST_night_mean = suit.assemble(LST_night_sum, spreadYear, GAULS_DEST, FP = LST_night_m_FP)[[1]],
                       LST_night_stdDev = suit.assemble(LST_night_sum, spreadYear, GAULS_DEST, FP = LST_night_sd_FP)[[2]],
                       TCB_mean = suit.assemble(TCB_sum, spreadYear, GAULS_DEST, FP = NA)[[1]],
                       TCB_stdDev = suit.assemble(TCB_sum, spreadYear, GAULS_DEST, FP = NA)[[2]],
                       TCW_mean = suit.assemble(TCW_sum, spreadYear, GAULS_DEST, FP = NA)[[1]],
                       TCW_stdDev = suit.assemble(TCW_sum, spreadYear, GAULS_DEST, FP = TCW_sd_FP)[[2]],
                       LOGimport_pres_GC = log(cpy1 + 1),
                       LOGimport_pres_Grav = log(cpy2 + 1),
                       LOGimport_pres_Rad = log(cpy3 + 1),
                       import_pres_Adjacency = cpy4,
                       LOGimport_pres_Air = log(cpy5 + 1),
                       LOGimport_pres_Mig = log(cpy6 + 1),
                       LOGimport_pres_Fric = log(cpy7 + 1))
  
  # return object
  if(!missing(Big_cities)){
    if(spreadYear %in% Big_cities$Year){
      rtn_df = list(rtn_df = rtn_df,
                    Big_city_con = Big_city_con)
    }else{
      rtn_df = list(rtn_df = rtn_df)
    }
  }else{
    rtn_df = list(rtn_df = rtn_df)
  }
  return(rtn_df)
}

# FP = future projection for use outside the year range of the MAP covariates 
# FP- can be NA in which case values before 2000 = 2000 and values after 2015 become 2015
suit.assemble <- function(sum_mat, Fyear, GAULS_DEST, FP){
  # if there isn't covariate data for the focus year
  # multiply data for the year 2000 by a national multiplier that accounts for past or future increases/decreases
  if(sum(grepl(Fyear, colnames(sum_mat))) == 0){
    # if no future projection data are available, otherwise use the FP data
    if(length(FP) == 1){
      # if before timeseries
      Year_opts = unique(as.numeric(sapply(colnames(sum_mat), function(x) substr(x, nchar(x)-4+1, nchar(x)))))
      if(all(Fyear < Year_opts)){
        near_year = min(Year_opts)
        multiplier = 1
      }else{
        near_year = max(Year_opts)
        multiplier = 1
      }
    }else{
      near_year = 2005
      multiplier = FP$val[FP$Year == Fyear] / FP$val[FP$Year == near_year]
    }
  }else{
    near_year = Fyear
    multiplier = 1
  }
  
  # return relevant covariate vectors
  mean_vec = sum_mat[, grepl(near_year, colnames(sum_mat)) & (grepl("mean", colnames(sum_mat)) | grepl("Mean", colnames(sum_mat)))]
  stdDev_vec = sum_mat[, grepl(near_year, colnames(sum_mat)) & grepl("stdDev", colnames(sum_mat))]
  
  # include multiplier (will be 1 if year is within range or no projection data are available)
  mean_vec = mean_vec * multiplier
  stdDev_vec = stdDev_vec * multiplier
  
  return(list(mean_vec[match(GAULS_DEST, admin2$GAUL_CODE)], stdDev_vec[match(GAULS_DEST, admin2$GAUL_CODE)]))
}




############################################
# part 01 assemble spread datasets year by year
############################################


# year to year spread and its sources
spread_dat <- list()

# list of big city connection matrices
Big_city_con_list <- list()

year_range <- min(dat$year):max(dat$year)

# save year range of data for later analyses
#save(year_range, file = "Data/Intermediate_datasets/year_range_prog.RData")




## loop through each successive year timestep and assemble:
# 1) areas infected in year i
# 2) areas infected and areas not infected by year (i + 1)
# 3) covariates linking 1 and 2


for(i in 1:(length(year_range) - 1)){
  # load up data for areas infected now and infected in next years
  fit_dat_now <- dat[dat$year <= year_range[i], ]
  fit_dat_next <- dat[dat$year <= year_range[(i + 1)], ]
  # Now including data for areas that were infected at both the start and the end of the timestep
  # (was previous just areas that went from status uninfected to infected)
  #fit_dat_next = fit_dat_next[!(fit_dat_next$GAUL_A2 %in% fit_dat_now$GAUL_A2), ]
  
  # define infected and non-infected areas
  # now
  infected <- admin2$GAUL_CODE[admin2$GAUL_CODE %in% fit_dat_now$GAUL_A2]
  vulnerable <- admin2$GAUL_CODE[!(admin2$GAUL_CODE %in% fit_dat_now$GAUL_A2)]
  
  # next
  vulnerable_infected_next <- admin2$GAUL_CODE[admin2$GAUL_CODE %in% fit_dat_next$GAUL_A2]
  vulnerable_NOT_infected_next = vulnerable[!(vulnerable %in% vulnerable_infected_next)]
  
  # assemble fitting dataset response variable
  model_fit_dat <- data.frame(GAUL = c(vulnerable_infected_next, 
                                       vulnerable_NOT_infected_next),
                              Infected = c(rep(1, length(vulnerable_infected_next)),
                                           rep(0, length(vulnerable_NOT_infected_next))))
  # sort so GAULs are in the same order as in the admin2 shapefile
  model_fit_dat$order = match(model_fit_dat$GAUL, admin2$GAUL_CODE)
  model_fit_dat = model_fit_dat[order(model_fit_dat$order), ]
  model_fit_dat = model_fit_dat[, 1:2]
  
  
  ## add covariates, save sources separately for later interrogation
  cov_finder <- add.Covariates(GAULS_SOURCE = infected,
                               GAULS_DEST = model_fit_dat$GAUL,
                               spreadYear = year_range[i],
                               Big_cities = Big_cities)
  model_fit_dat = data.frame(model_fit_dat,
                             cov_finder$rtn_df,
                             Year_end = year_range[(i + 1)])
  
  ## saving datasets to lists
  spread_dat[[i]] = model_fit_dat
  
  # saving big city connection matrices
  if(length(cov_finder) > 1){
    Big_city_con_list = c(Big_city_con_list, cov_finder$Big_city_con)
  }
}





# add names to the data lists and save
names(spread_dat) <- year_range[1:(length(year_range) - 1)] + 1
names(Big_city_con_list) <- Big_cities$GAUL[order(Big_cities$Year)]

if(country == "Brazil"){
  save(spread_dat, file = "Data/Intermediate_datasets/Retrospective_pred/Spread_datasets.RData")
}

if(country == "Mexico"){
  save(spread_dat, file = "Data/Intermediate_datasets/Retrospective_pred/Spread_datasets_Mexico.RData")
}


# save mean and sd of variables for normalisation of covariates
all_vals = do.call(rbind, lapply(spread_dat, function(x) x[, 3:21]))
vals_summary <- apply(all_vals, 2, function(x) c(mean(x), sd(x), min(x), max(x)))

if(country == "Brazil"){
  save(vals_summary, file = "Data/Intermediate_datasets/Retrospective_pred/Covariate_summaries.RData")
}

if(country == "Mexico"){
  save(vals_summary, file = "Data/Intermediate_datasets/Retrospective_pred/Covariate_summaries_Mexico.RData")
}

# save big city source lists

if(country == "Brazil"){
  save(Big_city_con_list, file = "Data/Intermediate_datasets/Retrospective_pred/Big_city_sources.RData")
}

if(country == "Mexico"){
  save(Big_city_con_list, file = "Data/Intermediate_datasets/Retrospective_pred/Big_city_sources_Mexico.RData")
}


