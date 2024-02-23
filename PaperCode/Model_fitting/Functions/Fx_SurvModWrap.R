# function to fit temporal survival models to nation-wide invasion counts
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
  #Region = dat$Region[match(GAUL_surv$GAUL, dat$GAUL_A2)]
  
  
  
  # create survival object and plot Kaplan Meier curve
  surv_dat <- data.frame(Followup_time1, Followup_time2)
  
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