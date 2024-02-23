# functuon for adding covariates during data assembly
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