trainXGBoost <- function(fullDataset, randomSeed){

  set.seed(randomSeed)
  
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_XGB <- training(train_test_split)
  testset_XGB <- testing(train_test_split)
  testset_XGB <- testset_XGB %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_XGB, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset
  xgb_rec <- recipe(Infected ~ ., data = trainset_XGB) %>%
             update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                          Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
             step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                             EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                             LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig) 
  
  #Specify an XGBoost model
  xgb_spec_tuning <- boost_tree(min_n=tune(), 
                                tree_depth=tune(), 
                                learn_rate=tune(), 
                                loss_reduction=tune(), 
                                trees=tune(), 
                                mtry=tune()) %>%
                      set_engine("xgboost") %>% 
                      set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  xgb_tuning_wflow <- workflow() %>% 
                      add_recipe(xgb_rec) 
  
  #Define the grid of hyperparameters and tune
  xgboost_params <- parameters(min_n(),
                               tree_depth(),
                               learn_rate(),
                               loss_reduction(),
                               trees(),
                               finalize(mtry(),trainset_XGB))
  
  xgboost_grid <- grid_max_entropy(xgboost_params, size = 125)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(xgb_tuning_wflow %>% add_model(xgb_spec_tuning),
                             resamples = folds,
                             grid = xgboost_grid,
                             metrics = metric_set(roc_auc, mcc, sens),
                             control = control_grid(verbose = TRUE, event_level = "second"))
  
  #Updated workflow with tuned hyperparameters
  xgb_highest_auc <- tuningResults %>% select_best("roc_auc")
  
  xgb_tuned_wflow <- finalize_workflow(xgb_tuning_wflow %>% 
                                       add_model(xgb_spec_tuning),
                                       xgb_highest_auc) %>%
                     fit(data = trainset_XGB)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(xgb_tuned_wflow, trainset_XGB, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_xgb_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(xgb_tuned_wflow, testset_XGB, probThres)
  
  return(metrics_xgb_Overall)
  
}