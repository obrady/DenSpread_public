naiveTSCV <- function(dataset, tunedWorkflow, randomSeed, country, thresholds){
  set.seed(randomSeed)
  train_test_split <- initial_split(dataset,prop = 3/4, strata=TestElig)
  trainset_XGB <- training(train_test_split)
  testset_XGB <- testing(train_test_split)
  testset_XGB_byYear <- group_split(testset_XGB, Year_end)
  
  #Workflows combine model specifications and dataset recipes
  xgb_pretuned_wflow <- tunedWorkflow %>%  
                        fit(data = trainset_XGB)
  
  #Find probability threshold
  if (country=="Mexico"){
    numYears <- 23
  }else if (country=="Brazil"){
    numYears <- 18
  }
  
  model_preds_toScore <- list() 
  
  for (year in 1:numYears){
    # Set threshold
    threshold <- thresholds[year]
    
    numVulnerable <- as.numeric(testset_XGB_byYear[[year]] %>% tally(TestElig ==1))
    testset <- testset_XGB_byYear[[year]] %>% dplyr::filter(TestElig==1)
    
    #Generate dataframe of probabilities and class predictions 
    model_probs <- predict(xgb_pretuned_wflow, testset, type = "prob")
    model_classes <- predict(xgb_pretuned_wflow, testset, type = "class")
    model_preds_initialThres <- bind_cols(testset$GAUL, testset$Infected, testset$TestElig, testset$Region,
                                          model_probs, model_classes) %>% 
                                dplyr::rename(GAUL = 1, Infected = 2, TestElig = 3, Region=4)
    
    #Apply the previously computed prediction to the probabilities to recast the classes
    numNewlyInfectedGAULs <- round(threshold * numVulnerable)
    
    model_preds_newThres <-  model_preds_initialThres %>% 
                             arrange(desc(.pred_1)) %>% 
                             mutate(.pred_class = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) 
    
    #Need to convert new predictions into a factor in order to work with the yardstick functions
    model_preds_newThres$.pred_class <- factor(model_preds_newThres$.pred_class, levels=c(0,1))
    if (country=="Mexico"){
      model_preds_newThres$Year <- year+1996
    }else if(country=="Brazil"){
      model_preds_newThres$Year <- year+2001
    }
    model_preds_toScore[[year]] <- model_preds_newThres
  }
  
  model_preds_toScore <- bind_rows(model_preds_toScore)
  multimetric <- metric_set(sens, spec)
  naive_metrics_Overall <-  rbind(multimetric(model_preds_toScore, truth = Infected, estimate = .pred_class, event_level = "second"),
                                  roc_auc(model_preds_toScore, truth =Infected, .pred_1, event_level = "second"))
  
  outputs_nTSCV <- list()
  outputs_nTSCV[[1]] <- naive_metrics_Overall
  outputs_nTSCV[[2]] <- model_preds_toScore
  
  #Regional 
  if (country == "Mexico"){
    naive_metrics_Regional <- tidyRegionalMetricsMex(model_preds_toScore)
  } else {
    naive_metrics_Regional <- tidyRegionalMetricsBra(model_preds_toScore)
  }
  outputs_nTSCV[[3]] <-naive_metrics_Regional

  #Yearly 
  if (country == "Mexico"){
    naive_metrics_Yearly <- tidyYearlyMetricsMex(model_preds_toScore)
  } else {
    naive_metrics_Yearly <- tidyYearlyMetricsBra(model_preds_toScore)
  }
  outputs_nTSCV[[4]] <-naive_metrics_Yearly
  
  return(outputs_nTSCV)
}