# FP = future projection for use outside the year range of the MAP covariates 
# FP- can be NA in which case values before 2000 = 2000 and values after 2015 become 2015
suit.assemble <- function(sum_mat, Fyear, GAULS_DEST, admin2, FP){
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
