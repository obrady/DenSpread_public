trainRandomForest <- function(fullDataset, randomSeed){
  
  #Use 5-fold cross validation to tune
  set.seed(randomSeed)
  
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_RF <- training(train_test_split)
  testset_RF <- testing(train_test_split)
  testset_RF <- testset_RF %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_RF, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset
  rf_rec <- recipe(Infected ~ ., data = trainset_RF) %>%
            update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                        Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
            step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                           EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                           LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  #Specify a random forest where we want to tune:
    ## the number of predictors randomly sampled at each split (mtry) 
    ## the min number of data points in a node for a further split to happen (min_n)
    ## and the trees
  rf_spec_tuning <- rand_forest(mtry = tune(), 
                                min_n=tune(), 
                                trees=tune()) %>%
                    set_engine("ranger", verbose=TRUE) %>% 
                    set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  rf_tuning_wflow <- workflow() %>% 
                     add_recipe(rf_rec)
  
  #Tune tree depth and complexity over 5x5x5 candidate models 
  forest_grid <- rf_grid <- grid_max_entropy(mtry() %>% range_set(c( 1,  16)),
                                             min_n(),
                                             trees(),
                                             size = 25)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(rf_tuning_wflow %>% add_model(rf_spec_tuning),
                             resamples = folds,
                             grid = forest_grid,
                             control=control_grid(parallel_over = "everything"))
  
  #Updated workflow with tuned tree_depth
  rf_highest_auc <- tuningResults %>% select_best("roc_auc")
  
  rf_tuned_wflow <- finalize_workflow(rf_tuning_wflow %>% 
                                      add_model(rf_spec_tuning),
                                      rf_highest_auc) %>%
                    fit(data = trainset_RF)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(rf_tuned_wflow, trainset_RF, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_rf_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(rf_tuned_wflow, testset_RF, probThres)
  
  return(metrics_rf_Overall)
  
} 