trainDecisionTree <- function(fullDataset, randomSeed){
  
  set.seed(randomSeed)
  train_test_split <- initial_split(spread_dat,prop = 3/4, strata=Infected)
  trainset_DT <- training(train_test_split)
  testset_DT <- testing(train_test_split)
  testset_DT <- testset_DT %>%filter(TestElig==1)
  
  set.seed(2020)
  folds <- vfold_cv(trainset_DT, v=5)
  
  #Create recipe where you exclude variables you do not want as predictors but keep in the dataset
  dt_rec <- recipe(Infected ~ ., data = trainset_DT)%>%
            update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                        Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
            step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                           EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                           LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  #Specify a decision tree model where we want to tune tree depth
  dt_spec_tuning <- decision_tree(tree_depth = tune(), 
                                  cost_complexity=tune(),
                                  min_n = tune()) %>%
                    set_engine("rpart") %>% 
                    set_mode("classification")
  
  #Workflows combine model specifications and dataset recipes
  dt_tuning_wflow <- workflow() %>% 
                     add_recipe(dt_rec)
  
  #Tune tree depth and complexity over 125 candidate models 
  tree_grid <- grid_max_entropy(cost_complexity(),
                                tree_depth(),
                                min_n(),
                                size = 125)
  
  doParallel::registerDoParallel()
  
  tuningResults <- tune_grid(dt_tuning_wflow %>% add_model(dt_spec_tuning),
                             resamples = folds,
                             grid = tree_grid)
  
  # Updated workflow with tuned params
  highest_auc <- tuningResults %>% select_best("roc_auc")
  
  dt_tuned_wflow <- finalize_workflow(dt_tuning_wflow %>% 
                                      add_model(dt_spec_tuning),
                                      highest_auc) %>%
                    fit(data = trainset_DT)
  
  #Find probability threshold that minimizes classification distance
  thresholdRange <- seq(0, 0.99, by = 0.01)
  probThres <- minimizeClassificationDist(dt_tuned_wflow, trainset_DT, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_dt_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(dt_tuned_wflow, testset_DT, probThres)
  
  return(metrics_dt_Overall)
}