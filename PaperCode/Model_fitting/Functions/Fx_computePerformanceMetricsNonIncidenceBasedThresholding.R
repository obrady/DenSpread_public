computePerformanceMetrics_NonIncidenceBasedThresholding <- function(workflow, testset, threshold){
  #Generate dataframe of probabilities and class predictions 
  model_probs <- predict(workflow, testset, type = "prob")
  model_classes <- predict(workflow, testset, type = "class")
  model_preds_initialThres <- bind_cols(testset$Infected, model_probs, model_classes) %>% dplyr::rename(Infected = 1)

  #Apply the previously computed prediction to the probabilities to recast the classes 
  model_preds_newThres <- model_preds_initialThres %>% mutate(.pred_class = case_when(.pred_1 > threshold ~ 1,
                                                                                      TRUE ~ 0))
  
  #Need to convert new predictions into a factor in order to work with the yardstick functions
  model_preds_newThres$.pred_class <- factor(model_preds_newThres$.pred_class, levels=c(0,1))
  
  #Calculate AUC, SP, and SN
  multimetric <- metric_set(yardstick::sens, yardstick::spec)
  metricBundle <-  rbind(multimetric(model_preds_newThres, truth = Infected, estimate = .pred_class, event_level = "second"),
                         roc_auc(model_preds_newThres, truth =Infected, .pred_1, event_level = "second"))
  return(metricBundle)
} 