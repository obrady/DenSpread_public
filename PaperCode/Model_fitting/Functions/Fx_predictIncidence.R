# ++++++++++++++++++++++++++++
# futureProjections
# ++++++++++++++++++++++++++++

#INPUTS

#OUTPUTS

# function for predicting annual incidence based on the optimal fitting parametric survival model
# returns a data frame with incidence predictions for supplied years for 50% 2.5% and 97.5% quantiles
# includes a sampel option for future spread prediction- samples 1 or the five fitting models from the past 5 years
# if mod_sample = FALSE (default) the model from the closest year to Current_year is chosen
# mode can be "CV" or "Best"- the former is in cross validation mode- ie. waht woudl ahve been the best prediction in e.g. 2005 and is
# to be used for model cross validation and future projection. "Best" is to be used for best estimate reconstruction of past trends


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

predictIncidence <- function(Years, Country, Current_year, mod_sample = FALSE, mod_mode = "CV",
                              objFilepath = "/Users/eideobra/Dropbox/08_Serotype_spread/Fitted_models/Incidence_models/"){
  # error checking
  if(any(!(Years %in% 1983:2040))){
    print("Years not in range 1983-2040")
  }
  
  if(!(Country %in% c("Mexico", "Brazil"))){
    print("Country has to be Mexico or Brazil")
  }
  
  if(!(mod_mode %in% c("CV", "Best"))){
    print("Please select CV or Best")
  }
  
  if(!("CV_Survival_model_Mexico.RData" %in% list.files(objFilepath))){
    print("Check objFilepath contains model prediction files")
  }
  
  # load country-specific model predictions
  if(Country == "Mexico"){
    load(paste0(objFilepath, "CV_Survival_model_Mexico.Rdata"))
  }else{
    load(paste0(objFilepath, "CV_Survival_model_Brazil.Rdata"))
  }
  
  # subset to appropriate region, then return relevant years
  #IncidMod_pred_r = IncidMod_pred[IncidMod_pred$Region == Region, ]
  
  if(mod_mode == "CV"){
    # subset to appropriate fitting year
    # find nearest fitting year
    ref_year = preds_df$Prediction_year[which.min((preds_df$Prediction_year - Current_year)^2)]
    # sample from last 5 fitting years if sample is TRUE
    if(mod_sample){ref_year = sample((ref_year - 4):ref_year, 1)}
    
    ss_preds_df = preds_df[preds_df$Prediction_year == ref_year, ]
  }
  
  if(mod_mode == "Best"){
    ss_preds_df = preds_df[preds_df$Prediction_year == max(preds_df$Prediction_year), ]
  }
  
  # formatting and return
  rtn <- ss_preds_df[match(Years, ss_preds_df$Year), c("Year", "Incid", "Incid_lower", "Incid_upper")]
  # swap upper and lower as names have been reversed
  colnames(rtn)[3:4] = c("Incid_upper", "Incid_lower")
  return(rtn)
}

# wrapper funciton for future projection plus uncertainty
pred.Mun.Incid.Posterior <- function(Years, Country, objFilepath){
  # gather predictions from the 5 latest year models
  m1 <- predict.Mun.Incid(Years = Years, Country = Country, Current_year = 2019, objFilepath = objFilepath)
  m2 <- predict.Mun.Incid(Years = Years, Country = Country, Current_year = 2018, objFilepath = objFilepath)
  m3 <- predict.Mun.Incid(Years = Years, Country = Country, Current_year = 2017, objFilepath = objFilepath)
  m4 <- predict.Mun.Incid(Years = Years, Country = Country, Current_year = 2016, objFilepath = objFilepath)
  m5 <- predict.Mun.Incid(Years = Years, Country = Country, Current_year = 2015, objFilepath = objFilepath)
  
  # sample a distribution from each model then combine
  nsims = 100
  
  # assume normally distributed predictions
  d1 <- t(apply(m1, 1, function(x) rnorm(nsims, mean = x[2], sd = 0.5 * ((x[3] - x[2]) + (x[2] - x[4])) / 1.96)))
  d2 <- t(apply(m2, 1, function(x) rnorm(nsims, mean = x[2], sd = 0.5 * ((x[3] - x[2]) + (x[2] - x[4])) / 1.96)))
  d3 <- t(apply(m3, 1, function(x) rnorm(nsims, mean = x[2], sd = 0.5 * ((x[3] - x[2]) + (x[2] - x[4])) / 1.96)))
  d4 <- t(apply(m4, 1, function(x) rnorm(nsims, mean = x[2], sd = 0.5 * ((x[3] - x[2]) + (x[2] - x[4])) / 1.96)))
  d5 <- t(apply(m5, 1, function(x) rnorm(nsims, mean = x[2], sd = 0.5 * ((x[3] - x[2]) + (x[2] - x[4])) / 1.96)))
  
  # bind columns then summarise by year (rows)
  comb <- cbind(d1, d2, d3, d4, d5)
  comb_sum <- t(apply(comb, 1, quantile, probs = c(0.025, 0.5, 0.975)))
  ensemble_pred <- data.frame(Year = Years,
                              Incid = comb_sum[, 2],
                              Incid_upper = comb_sum[, 3],
                              Incid_lower = comb_sum[, 1])
  return(ensemble_pred)
}