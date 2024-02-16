trainLogReg <- function(fullDataset, randomSeed){
  
  set.seed(randomSeed)
  
  train_test_split <- initial_split(fullDataset,prop = 3/4, strata=Infected)
  trainset_LR <- training(train_test_split)
  testset_LR <- testing(train_test_split)
  testset_LR <- testset_LR %>% filter(TestElig==1)
  
  #Create workflow and fit all in one go since no tuning needed
  logreg_rec <- recipe(Infected ~ ., data = trainset_LR) %>%
                update_role(ae_suit, y, TCW_mean, LOGimport_pres_Grav,LOGimport_pres_GC,
                            Year_end,GAUL, TestElig, Region, new_role = "ID") %>% 
                step_normalize(x,LST_day_mean,LST_day_stdDev,LST_night_mean,LST_night_stdDev,TCW_stdDev,
                               EVI_mean,EVI_stdDev,Landcover,TCB_mean,TCB_stdDev,
                               LOGimport_pres_Fric,LOGimport_pres_Rad,import_pres_Adjacency, LOGimport_pres_Air,LOGimport_pres_Mig)
  
  logreg_spec <- logistic_reg(mode = "classification") %>%
                 set_engine(engine = "glm") 
  
  logreg_wflow <- workflow() %>% add_recipe(logreg_rec) %>% add_model(logreg_spec) %>% fit(data = trainset_LR)
  
  #Find probability threshold that minimizes abs difference between SN and SP
  thresholdRange <- seq(0, 0.99, by = 0.01)
  logreg_probThres <- minimizeClassificationDist(logreg_wflow, trainset_LR, thresholdRange)
  
  #AUC/SP/SN overall, regionally, and yearly
  metrics_LR_AirMig_Overall <- computePerformanceMetrics_NonIncidenceBasedThresholding(logreg_wflow, testset_LR, logreg_probThres)
  return(metrics_LR_AirMig_Overall)
}