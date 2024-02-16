# Incidence model fitting script for DENSpread
# Oliver Brady
# 21 SEP 2021


# workspace setup
rm(list = ls())

require(ggplot2)
#require(segmented)
#require(mgcv)
require(survival)
require(ggfortify)
require(flexsurv)
#require(survminer)


setwd("/Users/Vinyas/OneDrive/08_Serotype_spread/")
#setwd("/Users/eideobra/Dropbox/08_Serotype_spread/")



### A Data processing
# select country 
country = "Mexico"

if(country == "Brazil"){
  dat <- read.csv("Data/Brazil_mun_dat_cleaned_thresholded.csv")
  
  load("Data/Intermediate_datasets/Retrospective_pred/Spread_datasets.RData")
  invasion_date = 1983
  mun_info <- read.csv("Reference/GAUL_IBGE_conversion_full.csv")
  dat$Region = mun_info$Region[match(dat$GAUL_A2, mun_info$GAUL_CODE)]
}
  
if(country == "Mexico"){
  dat <- read.csv("Data/Mexico_mun_dat_cleaned_thresholded.csv")
  
  load("Data/Intermediate_datasets/Retrospective_pred/Spread_datasets_Mexico.RData")
  invasion_date = 1995
  # region data frame
  mun_info <- read.csv("Data/Mexico_dat/Mexico_regions.csv")
  dat$Region = mun_info$Region[match(dat$State, mun_info$State_code)]
  
}


surv.mod.wrap <- function(f_dat){
  # consolidate to year of first infection of each municipality
  GAUL_surv = data.frame(GAUL = f_dat$GAUL_A2,
                         YearInf = f_dat$Year)
  
  # now add not yet infected GAULs
  missing_GAULS = spread_dat[[1]]$GAUL[!(spread_dat[[1]]$GAUL %in% GAUL_surv$GAUL)]
  GAUL_surv = rbind(GAUL_surv, data.frame(GAUL = missing_GAULS, YearInf = NA))
  
  
  # convert years to years since 1 year before surveillance began
  GAUL_surv$YearInf = GAUL_surv$YearInf - invasion_date
  
  GAUL_surv$LeftCensor = GAUL_surv$YearInf == min(GAUL_surv$YearInf, na.rm = T)
  GAUL_surv$RightCensor = is.na(GAUL_surv$YearInf)
  GAUL_surv$YearInf[is.na(GAUL_surv$YearInf)] = max(GAUL_surv$YearInf, na.rm = T)
  
  Followup_time1 = Followup_time2 = GAUL_surv$YearInf
  Followup_time1[GAUL_surv$LeftCensor] = NA
  Followup_time2[GAUL_surv$RightCensor] = NA
  
  # region covariate
  Region = dat$Region[match(GAUL_surv$GAUL, dat$GAUL_A2)]
  
  
  
  # create survival object and plot Kaplan Meier curve
  surv_dat <- data.frame(Followup_time1, Followup_time2, Region = Region)
  
  Surv.Obj <- Surv(Followup_time1, Followup_time2, type = 'interval2')
  fit = survfit(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat)
  #autoplot(fit)
  
  
  
  # parametric survival models
  fs1 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "exp")
  fs2 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "weibull", method="Nelder-Mead")
  fs3 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "gamma", method="Nelder-Mead")
  fs4 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "lnorm", method="Nelder-Mead")
  fs5 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "gompertz", method="Nelder-Mead")
  fs6 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "llogis", method="Nelder-Mead")
  fs7 <- flexsurvreg(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, dist = "gengamma", method="Nelder-Mead", inits = c(1,1,-1))
  
  # more flexible spline for comparison (expecting better fit to data but poorer ability to extrapolate)
  fs_spline <- flexsurvspline(Surv(Followup_time1, Followup_time2, type = 'interval2') ~ 1, data = surv_dat, scale="normal", k = 1, inits = c(-2, 0.5, -0.5))
  
  mod_compare = data.frame(Model = c("exp", "weibull", "gamma", "lnorm", "gompertz", "llogis", "gengamma", "spline"),
                           AICs = c(AIC(fs1), AIC(fs2), AIC(fs3), AIC(fs4), AIC(fs5), AIC(fs6), AIC(fs7), AIC(fs_spline)))
  #mod_compare = mod_compare[order(mod_compare$AICs), ]
  # collect model list
  mod_list = list(fs1, fs2, fs3, fs4, fs5, fs6, fs7, fs_spline)
  return(list(model_comparison = mod_compare, model = mod_list))
  #chosen_model = mod_list[[which.min(mod_compare$AICs)]]
}




# convert to annual incidence predictions
incid.find <- function(x){
  # calculate eligible at t - 1
  eligible = c(1, x[1:(length(x) - 1)])
  # infected
  infected = eligible - x
  # incidence
  incidence = infected / eligible
  # correct cases wehre 0 eligible
  incidence[!is.finite(incidence)] = 1
  return(incidence)
}

# function to generate predictions from national survival models
Surv.predict <- function(Survmod){
  
  Surv_preds = predict(Survmod, 
                       type = "survival",
                       newdata = data.frame(1),
                       times = 0:(2040 - invasion_date), 
                       conf.int = T)
  
  Surv_preds = Surv_preds$.pred[[1]]
  
  IncidMod_pred <- data.frame(Year = invasion_date:2040,
                              Incid = incid.find(Surv_preds$.pred_survival),
                              Incid_upper = incid.find(Surv_preds$.pred_upper),
                              Incid_lower = incid.find(Surv_preds$.pred_lower))
  
  # graph
  #g1 <- ggplot(IncidMod_pred, aes(x = Year, y = Incid)) +
  #  geom_line() +
  #  geom_line(aes(x = Year, y = Incid_lower), lty = 2, lwd= 0.25) +
  #  geom_line(aes(x = Year, y = Incid_upper), lty = 2, lwd= 0.25) +
  #  ylab("Probability of invasion in the next year")
  
  # return
  #return(list(table = IncidMod_pred,
  #            graph = g1))
  return(IncidMod_pred)
}

# function to generate predictions from multiple regions
Surv.region.predict <- function(Survmod, u_regions){
  
  # list of tables
  tab_list <- list()
  
  # loop through regions
  for(i in 1:length(u_regions)){
    Surv_preds = predict(chosen_model, 
                         type = "survival", 
                         newdata = data.frame(Region = u_regions[i]),
                         times = 0:(2040 - invasion_date), 
                         conf.int = T)
    
    Surv_preds = Surv_preds$.pred[[1]]
    
    IncidMod_pred <- data.frame(Year = invasion_date:2040,
                                Incid = incid.find(Surv_preds$.pred_survival),
                                Incid_upper = incid.find(Surv_preds$.pred_upper),
                                Incid_lower = incid.find(Surv_preds$.pred_lower),
                                Region = u_regions[i])
    
    tab_list[[i]] = IncidMod_pred
  }
  
  # compile
  IncidMod_pred = do.call("rbind", tab_list)
  
  # graph
  #g1 <- ggplot(IncidMod_pred, aes(x = Year, y = Incid, col = Region)) +
  #  geom_line() +
  #  geom_line(aes(x = Year, y = Incid_lower, col = Region), lty = 2, lwd= 0.25) +
  #  geom_line(aes(x = Year, y = Incid_upper, col = Region), lty = 2, lwd= 0.25) +
  #  ylab("Probability of invasion in the next year")
  
  # return
  #return(list(table = IncidMod_pred,
  #            graph = g1))
  return(IncidMod_pred)
  
}

### temporal CV of survival models

u_years = unique(dat$year)

# summary of best fitting models
chose_mod <- data.frame(Model = NA,
                        Rank_rmse = NA,
                        Rank_aic = NA,
                        Prediction_year = NA)
chose_mod = chose_mod[-1, ]

# list of best models for each fitting year
mod_year_list <- list()

# predictions of best model for each fitting year
preds_df <- data.frame(Year = NA,
                       Incid = NA,
                       Incid_upper = NA,
                       Incid_lower = NA,
                       Surv_mod_type = NA,
                       Prediction_year = NA)
preds_df = preds_df[-1, ]

# loop through years of data (need at least 3 years of fitting data)
for(i in 3:length(u_years)){
  # data for fitting and data for evaluation
  f_dat = dat[dat$year <= u_years[i], ]
  e_dat = dat[dat$year > u_years[i], ]
  # summarise fitting data
  fit_dat = cumsum(table(f_dat$year))
  fit_dat = data.frame(Year = as.numeric(names(fit_dat)),
                        Invaded = as.numeric(fit_dat))
  # add in years where 0s reported if Mexico
  if(country == "Mexico"){
    fit_dat = rbind(fit_dat,
                    data.frame(Year = c(1998, 1999),
                               Invaded = 19))
    fit_dat = fit_dat[order(fit_dat$Year), ]
  }
  
  # summarise evaluation data
  eval_dat = cumsum(table(e_dat$year)) + nrow(f_dat)
  eval_dat = data.frame(Year = as.numeric(names(eval_dat)),
                        Invaded = as.numeric(eval_dat))
  
  # fit survival models
  year_mods <- surv.mod.wrap(f_dat)
  
  # predict from survival models
  mod_rmse_table = data.frame(model = year_mods[[1]]$Model,
                              rmse = NA)
  # also collect the predictions
  mod_preds_list <- list()
  
  for(k in 1:length(year_mods$model)){
    # incidence predictions
    preds <- Surv.predict(year_mods$model[[k]])
    # number of invaded municipality predictions
    preds$num_invaded = NA
    preds$num_invaded[1] = round(nrow(spread_dat[[1]]) * preds$Incid[1], 0)
    for(r in 2:nrow(preds)){
      susceptible = nrow(spread_dat[[1]]) - sum(preds$num_invaded[1:(r - 1)])
      preds$num_invaded[r] = round(susceptible * preds$Incid[r], 0)
    }
    # and cumulative
    preds$cum_num_invaded = cumsum(preds$num_invaded)
    # add observed (evaluation)
    preds$observed = eval_dat$Invaded[match(preds$Year, eval_dat$Year)]
    # add observed (fitting)
    preds$observed[preds$Year %in% fit_dat$Year] = na.omit(fit_dat$Invaded[match(preds$Year, fit_dat$Year)])
    # add data type (fit or evaluation)
    preds$data_type = "Outside"
    preds$data_type[preds$Year %in% fit_dat$Year] = "Fit"
    preds$data_type[preds$Year %in% eval_dat$Year] = "Eval"
    mod_preds_list[[k]] = preds
    
    # evaluate predictive performance (RMSE)
    evaluation = preds[preds$data_type == "Eval", ]
    mod_rmse_table$rmse[k] = sqrt(mean((evaluation$observed - evaluation$cum_num_invaded)^2))
  }
  
  # add CV and predictions to the model evaluation table
  year_mods$model_comparison$rmse = mod_rmse_table$rmse
  year_mods$preds = mod_preds_list
  
  # summarise model performance
  chose_mod = rbind(chose_mod,
                    data.frame(Model = year_mods$model_comparison$Model,
                               Rank_rmse = rank(year_mods$model_comparison$rmse),
                               Rank_aic = rank(year_mods$model_comparison$AICs),
                               Prediction_year = u_years[i]))
  
  #select best model and its predictions
  # chose model with best RMSE
  cmod_index <- which.min(rank(year_mods$model_comparison$rmse))
  # if not enoguh fitting data then pre-specify model form after checking form_plot
  if(i > (length(u_years) - 3)){
    # log-logistic in Brazil
    if(country == "Brazil"){
      cmod_index <- which.max(year_mods$model_comparison$Model == "llogis")
    }
    # log-normal in Mexico
    if(country == "Mexico"){
      cmod_index <- which.max(year_mods$model_comparison$Model == "lnorm")
    }
  }
  
  mod_year_list[[i]] = year_mods$model[[cmod_index]]
  preds_df = rbind(preds_df,
                   data.frame(mod_preds_list[[cmod_index]],
                              Surv_mod_type = year_mods$model_comparison$Model[cmod_index],
                              Prediction_year = u_years[i]))
}


# examine model performance

# comparison between functional form
overall_perf = aggregate(Rank_rmse ~ Model, data = chose_mod, FUN = mean)
overall_perf[order(overall_perf$Rank_rmse), ]
# trim last 3 years as limtied fitting data makes them not useful for comparing between forms
chose_mod_plot = chose_mod[chose_mod$Prediction_year <= u_years[length(u_years) - 3], ]

form_plot <- ggplot(aes(x = Prediction_year, y = Rank_rmse, colour = Model), data = chose_mod_plot) +
  geom_line(linetype = "twodash") +
  xlab("Prediction year") +
  ylab("RMSE rank (Best = 1)")
form_plot
ggsave(form_plot, filename = paste0("Plots/Spread_CV_form", country, ".pdf"), height = 4, width = 7)

# ! not sued- more interested in predictive performance now, so focus on rmse
#overall_perf2 = aggregate(Rank_aic ~ Model, data = chose_mod, FUN = mean)
#overall_perf2[order(overall_perf2$Rank_aic), ]
#form_plot2 <- ggplot(aes(x = Prediction_year, y = Rank_aic, colour = Model), data = chose_mod) +
#  geom_line() +
#  xlab("Prediction year") +
#  ylab("AIC rank (Best = 1)")
#form_plot2

# assessing changing predictions over time
preds_df$Years_of_data = 3 + preds_df$Prediction_year - min(preds_df$Prediction_year)
# trim out predictions that are before prediction year
preds_df_plot = preds_df[preds_df$Year > preds_df$Prediction_year, ]
# but add them back in for the model fit with the most data
preds_df_plot = rbind(preds_df_plot, preds_df[preds_df$Years_of_data == max(preds_df$Years_of_data), ])
# only past 5 year would be used for future projection, so strip out the long term prediction fo all but the last five models
f_preds = seq(max(preds_df_plot$Prediction_year) - 4, max(preds_df_plot$Prediction_year))
preds_df_plot = preds_df_plot[!(!(preds_df_plot$Prediction_year %in% f_preds) & (preds_df_plot$Year > max(preds_df_plot$Prediction_year))), ]

# if Brazil trim to 1990 onwards
preds_df_plot = preds_df_plot[preds_df_plot$Year >= 1990, ]

predictions_plot <- ggplot(aes(x = Year, y = cum_num_invaded, colour = factor(Years_of_data)), data = preds_df_plot) +
  geom_line() +
  scale_y_continuous(name = "Predicted cumulative municipalities invaded") +
  guides(col=guide_legend("Years of training data"))
predictions_plot
ggsave(predictions_plot, filename = paste0("Plots/Spread_CV_predictions", country, ".pdf"), height = 8, width = 7)


# save objects 
#objFilepath = "/Users/Vinyas/OneDrive/08_Serotype_spread/Fitted_models/Incidence_models/"
#objFilepath = "/Users/eideobra/Dropbox/08_Serotype_spread/Fitted_models/Incidence_models/"

#save(preds_df, file = paste0(objFilepath, "CV_Survival_model_", country, ".RData"))
#ggsave(preds$graph, file = paste0("Plots/Survival_model_", country, ".pdf"))



# function for predicting annual incidence based on the optimal fitting parametric survival model
# returns a data frame with incidence predictions for supplied years for 50% 2.5% and 97.5% quantiles
# includes a sampel option for future spread prediction- samples 1 or the five fitting models from the past 5 years
# if mod_sample = FALSE (default) the model from the closest year to Current_year is chosen
# mode can be "CV" or "Best"- the former is in cross validation mode- ie. waht woudl ahve been the best prediction in e.g. 2005 and is
# to be used for model cross validation and future projection. "Best" is to be used for best estimate reconstruction of past trends

predict.Mun.Incid <- function(Years, Country, Current_year, mod_sample = FALSE, mod_mode = "CV",
                              objFilepath = "/Users/Vinyas/OneDrive/08_Serotype_spread/Fitted_models/Incidence_models/"){
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
  
  #if((Country == "Brazil") & !(Region %in% c("Northeast", "Southeast", "South", "Centralwest", "North"))){
  #  print("Region has to be one of: Northeast, Southeast, South, Centralwest or North")
  #}
  
  #if((Country == "Mexico") & !(Region %in% c("North", "West", "South", "East", "Center"))){
  #  print("Region has to be one of: North, West, South, East, Center")
  #}
  
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



# example usage:

# for time series cross validation prediction
predict.Mun.Incid(Years = 2006:2012,
                  Country = "Brazil",
                  Current_year = 2005,
                  objFilepath = objFilepath)

# for historical reconstruction:
predict.Mun.Incid(Years = 1983:2000,
                  Country = "Brazil",
                  Current_year = 2019,
                  mod_mode = "Best",
                  objFilepath = objFilepath)

# for future projection including uncertainty
pred.Mun.Incid.Posterior(Years = 2020:2040,
                         Country = "Mexico",
                         objFilepath = objFilepath)



