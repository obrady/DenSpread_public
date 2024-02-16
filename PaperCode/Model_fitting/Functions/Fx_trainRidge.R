trainRidge <- function(fullDataset, randomSeed){
  
  # Train lambda with 5-fold CV 
  set.seed(randomSeed)
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  spreaddat_train <- training(train_test_split)
  spreaddat_test <- testing(train_test_split)
  spreaddat_test <- spreaddat_test %>% filter(TestElig == 1)
  
  #Use 2020 for all 5-fold CV 
  set.seed(2020)
  folds <- vfold_cv(spreaddat_train, v=5)
  
  # Create recipe where you exclude variables you do not want as predictors but keep in the dataset and normalize all features 
  spreaddat_rec <- recipe(Infected ~ ., data = spreaddat_train) %>%
                   update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                               Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
                   step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                                  EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                                  LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  # Specify a Ridge model
  ridge_spec_tuning <- logistic_reg(penalty = tune(), mixture = 0) %>%
                       set_engine("glmnet") %>% 
                       set_mode("classification")
  
  # Workflows combine model specifications and dataset recipes
  ridge_tuning_wflow <- workflow() %>% 
                        add_recipe(spreaddat_rec)
  
  #Tune lambda
  lambda_grid <- grid_max_entropy(penalty(), size = 25)
  
  doParallel::registerDoParallel()
  
  ridge_grid <- tune_grid(ridge_tuning_wflow %>% add_model(ridge_spec_tuning),
                          resamples = folds,
                          grid = lambda_grid)
  
  # Updated workflow with tuned lambda
  highest_auc <- ridge_grid %>% select_best("roc_auc")
  
  ridge_tuned_wflow <- finalize_workflow(ridge_tuning_wflow %>% 
                       add_model(ridge_spec_tuning),highest_auc) %>%
                       fit(data = spreaddat_train)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(ridge_tuned_wflow, spreaddat_train, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_ridge_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(ridge_tuned_wflow, spreaddat_test, probThres)
  
  return(metrics_ridge_Overall)
}