trainMLP <- function(fullDataset, randomSeed){
  
  set.seed(randomSeed)
  
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_MLP <- training(train_test_split)
  testset_MLP <- testing(train_test_split)
  testset_MLP <- testset_MLP %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_MLP, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset and normalize all features 
  mlp_rec <- recipe(Infected ~ ., data = trainset_MLP) %>%
             update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                          Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
             step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                             EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                             LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  
  #Specify a decision tree model where we want to tune degree and cost
  mlp_spec_tuning <- mlp(hidden_units= tune(), 
                         dropout=tune(),
                         penalty = tune(),
                         epochs = 20) %>%
                     set_engine("keras") %>% 
                     set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  mlp_tuning_wflow <- workflow() %>% 
                      add_recipe(mlp_rec)
  
  #Tune over 125 candidate models 
  mlp_grid <- grid_regular(hidden_units(),
                           dropout(),
                           penalty(),
                           levels = 5)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(mlp_tuning_wflow %>% add_model(mlp_spec_tuning),
                             resamples = folds,
                             grid = mlp_grid)
  
  highest_auc <- tuningResults %>% select_best("roc_auc")
  
  mlp_tuned_wflow <- finalize_workflow(mlp_tuning_wflow %>% 
                     add_model(mlp_spec_tuning),highest_auc) %>% 
                     fit(data = trainset_MLP)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(mlp_tuned_wflow, trainset_MLP, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_mlp_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(mlp_tuned_wflow, testset_MLP, probThres)
}