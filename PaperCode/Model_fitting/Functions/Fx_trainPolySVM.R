trainPolySVM <- function(fullDataset, randomSeed){
  
  # Train lambda with 5-fold CV 
  set.seed(randomSeed)
  
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_pSVM <- training(train_test_split)
  testset_pSVM <- testing(train_test_split)
  testset_pSVM <- testset_pSVM %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_pSVM, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset and normalize all features 
  polySVM_rec <- recipe(Infected ~ ., data = trainset_pSVM) %>%
                 update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                              Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
                 step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                                 EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                                 LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  #Specify an SVM where we want to tune degree and cost
  polySVM_spec_tuning <- svm_poly(degree = tune(), 
                                  cost=tune()) %>%
                         set_engine("kernlab") %>% 
                         set_mode("classification")
                        
  #Workflows combine model specifications and dataset recipes
  polySVM_tuning_wflow <- workflow() %>% 
                          add_recipe(polySVM_rec)
  
  #Tune tree depth and complexity over 25 candidate models 
  svm_grid <- grid_regular(degree(),
                           cost(),
                           levels = 1)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(polySVM_tuning_wflow %>% add_model(polySVM_spec_tuning),
                             resamples = folds,
                             grid = svm_grid)
  
  highest_auc <- tuningResults %>% select_best("roc_auc")
  
  polySVM_tuned_wflow <- finalize_workflow(polySVM_tuning_wflow %>% 
                                           add_model(polySVM_spec_tuning),highest_auc) %>% 
                         fit(data = trainset_pSVM)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(polySVM_tuned_wflow, testset_pSVM, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_pSVM_Overall <- computePerformanceMetrics(polySVM_tuned_wflow, testset_pSVM, probThres)
}