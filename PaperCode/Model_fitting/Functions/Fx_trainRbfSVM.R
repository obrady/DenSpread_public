trainRbfSVM <- function(fullDataset, randomSeed){
  
  set.seed(randomSeed)
  
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_rbfSVM <- training(train_test_split)
  testset_rbfSVM <- testing(train_test_split)
  testset_rbfSVM <- testset_rbfSVM %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_rbfSVM, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset and normalize all features 
  rbfSVM_rec <- recipe(Infected ~ ., data = trainset_rbfSVM) %>%
                update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                            Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
                step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                               EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                               LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  
  #Specify a decision tree model where we want to tune degree and cost
  rbfSVM_spec_tuning <- svm_rbf(rbf_sigma = tune(), 
                                cost=tune()) %>%
                        set_engine("kernlab") %>% 
                        set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  rbfSVM_tuning_wflow <- workflow() %>% 
                         add_recipe(rbfSVM_rec)
  
  #Tune rbfsigma(gamma) and cost over 25 candidate models 
  svm_grid <- grid_regular(rbf_sigma(),
                           cost(),
                           levels = 5)
  
  doParallel::registerDoParallel()
  
  set.seed(2020)
  tuningResults <- tune_grid(rbfSVM_tuning_wflow %>% add_model(rbfSVM_spec_tuning),
                             resamples = folds,
                             grid = svm_grid)
  
  highest_auc <- tuningResults %>% select_best("roc_auc")
  
  rbfSVM_tuned_wflow <- finalize_workflow(rbfSVM_tuning_wflow %>% 
                                          add_model(rbfSVM_spec_tuning),highest_auc) %>% 
                        fit(data = trainset_rbfSVM)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(rbfSVM_tuned_wflow, testset_rbfSVM, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_rbfSVM_Overall <- computePerformanceMetrics(rbfSVM_tuned_wflow, testset_rbfSVM, probThres)
}