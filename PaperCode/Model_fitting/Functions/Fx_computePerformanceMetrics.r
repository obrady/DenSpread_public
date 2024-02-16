# ++++++++++++++++++++++++++++
# computePerformanceMetrics
# ++++++++++++++++++++++++++++
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in! 

computePerformanceMetrics <- function(workflow, testset, threshold, numVulnerable, infectedGAULs){
  #Generate dataframe of probabilities and class predictions 
  model_probs <- predict(workflow, testset, type = "prob")
  model_classes <- predict(workflow, testset, type = "class")
  model_preds_initialThres <- bind_cols(testset$GAUL, testset$Infected, testset$TestElig, model_probs, model_classes) %>% 
                              dplyr::rename(GAUL = 1, Infected = 2, TestElig = 3)
  
  #Apply the previously computed prediction to the probabilities to recast the classes
  numNewlyInfectedGAULs <- round(threshold * numVulnerable)
  oldPreds <- model_preds_initialThres %>% dplyr::filter(GAUL %in% infectedGAULs)
  model_preds_newThres <-  model_preds_initialThres %>% 
                           arrange(desc(.pred_1)) %>% 
                           dplyr::filter(!(GAUL %in% infectedGAULs)) %>%
                           mutate(.pred_class = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) 
  model_preds_newThres <- rbind(oldPreds, model_preds_newThres)
  
  # #Force newGAULPredictions to be 1 where they were previously infected
  model_preds_newThres <- model_preds_newThres %>% mutate (.pred_class = case_when(GAUL %in% infectedGAULs ~ 1,
                                                                                   TRUE ~ as.numeric(as.character(.pred_class))))
  
  #Need to convert new predictions into a factor in order to work with the yardstick functions
  model_preds_newThres$.pred_class <- factor(model_preds_newThres$.pred_class, levels=c(0,1))

  #Calculate AUC, SP, and SN
  model_preds_toScore <- model_preds_newThres %>% dplyr::filter(TestElig == 1)
  multimetric <- metric_set(sens, spec)
  metricBundle <-  rbind(multimetric(model_preds_toScore, truth = Infected, estimate = .pred_class, event_level = "second"),
                         roc_auc(model_preds_toScore, truth =Infected, .pred_1, event_level = "second"))
  return(metricBundle)
}