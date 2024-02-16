trainKNN <- function(fullDataset, randomSeed){
  
  # Train lambda with 5-fold CV 
  set.seed(randomSeed)
 
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_knn <- training(train_test_split)
  testset_knn <- testing(train_test_split)
  testset_knn <- testset_knn %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_knn, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset and normalize all features 
  knn_rec <- recipe(Infected ~ ., data = trainset_knn) %>%
             update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                        Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
             step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                           EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                           LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  #Specify a knn model where we want to tune knn
  knn_spec_tuning <- nearest_neighbor(neighbors = tune()) %>%
                     set_engine("kknn") %>% 
                     set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  knn_tuning_wflow <- workflow() %>% 
                      add_recipe(knn_rec)
  
  #Tune k and cost over 10 candidate models 
  knn_grid <- grid_regular(neighbors(),
                           levels = 10)
  
  knn_grid[1,] <- 3
  knn_grid[2,] <- 5
  knn_grid[3,] <- 7
  knn_grid[4,] <- 9
  knn_grid[5,] <- 17
  knn_grid[6,] <- 33
  knn_grid[7,] <- 69
  knn_grid[8,] <- 97
  knn_grid[9,] <- 183 # ~sqrt (trainset)
  knn_grid[10,] <- 211 # ~sqrt(spread_dat)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(knn_tuning_wflow %>% add_model(knn_spec_tuning),
                             resamples = folds,
                             grid = knn_grid)

  knn_highest_auc <- tuningResults %>% select_best("roc_auc")
  
  knn_tuned_wflow <- finalize_workflow(knn_tuning_wflow %>% 
                                       add_model(knn_spec_tuning),knn_highest_auc) %>% 
                      fit(data = trainset_knn)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(knn_tuned_wflow, trainset_knn, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_knn_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(knn_tuned_wflow, testset_knn, probThres)
  
  return(metrics_knn_Overall)
}