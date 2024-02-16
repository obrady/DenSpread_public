addCovariates_Bra <- function(GAULS_SOURCE, GAULS_DEST, spreadYear, fullDataset, admin1, admin2, location_data, regionLUT, big_cities = NULL){
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
  if(!missing(big_cities)){
    if(spreadYear %in% big_cities$YearInfected){
      # identify areas infected this year
      ss_BC <- big_cities[big_cities$YearInfected == spreadYear, ]
      big_city_con <- list()
      for(k in 1:nrow(ss_BC)){
        col_match <- match(ss_BC$GAUL[k], admin2$GAUL_CODE)
        big_city_con[[k]] <- rbind(log(con3[, col_match] + 1),
                                   con4[, col_match],
                                   log(con5[, col_match] + 1),
                                   log(con6[, col_match] + 1),
                                   log(con7[, col_match] + 1))
        names(big_city_con[[k]]) = ss_BC$GAUL[k]
      }
    }
  }
  
  # calculate the max of the flux from all connected areas
  cpy1 = apply(con1, 2, max, na.rm = T)
  cpy2 = apply(con2, 2, max, na.rm = T)
  cpy3 = apply(con3, 2, max, na.rm = T)
  cpy4 = apply(con4, 2, max, na.rm = T)
  cpy5 = apply(con5, 2, max, na.rm = T)
  cpy6 = apply(con6, 2, max, na.rm = T)
  cpy7 = apply(con7, 2, max, na.rm = T)
  
  # apply year-based multiplier for connectivity
  cpy2 = cpy2 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  cpy3 = cpy3 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  # special case for Air hich is 2009-2019 average
  air_mult = mean(Movement_FP$Mult[Movement_FP$Year %in% 2009:2019])
  cpy5 = cpy5 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)] / air_mult
  cpy6 = cpy6 * Movement_FP$Mult[match(spreadYear, Movement_FP$Year)]
  cpy7 = cpy7 / Movement_FP$Mult[match(spreadYear, Movement_FP$Year)] # divide instead of multiply as low travel time = more accessible
  
  # now assemble a return data frame
  rtn_df <- data.frame(ae_suit = Brazil_Aegypti_sum[match(GAULS_DEST, admin2$GAUL_CODE), 1],
                       EVI_mean = suit.assemble(Brazil_EVI_sum, spreadYear, GAULS_DEST, admin2, FP = EVI_m_FP)[[1]],
                       EVI_stdDev = suit.assemble(Brazil_EVI_sum, spreadYear, GAULS_DEST, admin2, FP = EVI_sd_FP)[[2]],
                       Landcover = suit.assemble(Brazil_LandCover_sum, spreadYear, GAULS_DEST, admin2, FP = NA)[[1]],
                       LST_day_mean = suit.assemble(Brazil_LST_day_sum, spreadYear, GAULS_DEST, admin2, FP = LST_day_m_FP)[[1]],
                       LST_day_stdDev = suit.assemble(Brazil_LST_day_sum, spreadYear, GAULS_DEST, admin2, FP = LST_day_sd_FP)[[2]],
                       LST_night_mean = suit.assemble(Brazil_LST_night_sum, spreadYear, GAULS_DEST, admin2, FP = LST_night_m_FP)[[1]],
                       LST_night_stdDev = suit.assemble(Brazil_LST_night_sum, spreadYear, GAULS_DEST, admin2, FP = LST_night_sd_FP)[[2]],
                       TCB_mean = suit.assemble(Brazil_TCB_sum, spreadYear, GAULS_DEST, admin2, FP = NA)[[1]],
                       TCB_stdDev = suit.assemble(Brazil_TCB_sum, spreadYear, GAULS_DEST, admin2, FP = NA)[[2]],
                       TCW_mean = suit.assemble(Brazil_TCW_sum, spreadYear, GAULS_DEST, admin2, FP = NA)[[1]],
                       TCW_stdDev = suit.assemble(Brazil_TCW_sum, spreadYear, GAULS_DEST, admin2, FP = TCW_sd_FP)[[2]],
                       LOGimport_pres_GC = log(cpy1 + 1),
                       LOGimport_pres_Grav = log(cpy2 + 1),
                       LOGimport_pres_Rad = log(cpy3 + 1),
                       import_pres_Adjacency = cpy4,
                       LOGimport_pres_Air = log(cpy5 + 1),
                       LOGimport_pres_Mig = log(cpy6 + 1),
                       LOGimport_pres_Fric = log(cpy7 + 1))
  
  #Add remaining missing fields (year, lat, long, GAUL, region, infected, test eligibility)
  rtn_df$Year_end <- spreadYear
  
  GAUL_index <- match(GAULS_DEST, location_data$location)
  rtn_df$GAUL <- location_data$location[GAUL_index] #GAUL is currently a factor
  rtn_df$x <- location_data$x[GAUL_index]
  rtn_df$y <- location_data$y[GAUL_index]
  
  #Add regions 
  rtn_df$Region = ""
  for(i in 1:nrow(rtn_df)){
    municipality_GAUL <- rtn_df$GAUL[i]
    admin2_index <- match(municipality_GAUL, admin2$GAUL_CODE)
    state_GAUL <- admin2[admin2_index,]$PARENT_ID
    admin1_index <- match(state_GAUL, admin1$GAUL_CODE)
    state_name <- admin1$NAME[admin1_index]
    if(state_name == "Name Unknown"){
      rtn_df$Region[i] <- NA
    }else{
      region_name <- regionLUT$Region[regionLUT$State == state_name]
      rtn_df$Region[i] <- region_name
    }
    
  }
  rtn_df$Region <- as.factor(rtn_df$Region)
  
  #Add labels 
  fullDataset <- fullDataset %>% dplyr::filter(Year_end == spreadYear)
  GAUL_index <- match(rtn_df$GAUL, fullDataset$GAUL)
  rtn_df$Infected <- fullDataset$Infected[GAUL_index]
  
  #Cast GAUL to be numeric once all of this is done, need to initially cast as character bc R otherwise casts factors in the order they appear 
  rtn_df$GAUL <- as.numeric(as.character(rtn_df$GAUL)) 
  
  # return object
  if(!missing(big_cities)){
    if(spreadYear %in% big_cities$YearInfected){
      rtn_df = list(rtn_df = rtn_df,
                    big_city_con = big_city_con)
    }else{
      rtn_df = list(rtn_df = rtn_df)
    }
  }else{
    rtn_df = list(rtn_df = rtn_df)
  }
  return(rtn_df)
}