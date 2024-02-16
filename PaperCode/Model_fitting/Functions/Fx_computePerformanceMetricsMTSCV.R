# ++++++++++++++++++++++++++++
# computePerformanceMetricsMTSCV
# ++++++++++++++++++++++++++++
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in!  

computePerformanceMetricsMTSCV <- function(workflow, testset, threshold, infectedGAULS, i, maxClassProbs, numVulnerable){
  
  model_probs <- predict(workflow, testset, type = "prob")
  model_classes <- predict(workflow, testset, type = "class")
  model_preds_initialThres <- bind_cols(testset$GAUL, testset$Infected, testset$TestElig, model_probs, model_classes) %>% 
                              dplyr::rename(GAUL = `...1`, Label = `...2`, TestElig = `...3`)
  
  #Take the percentile ranked predictions based on the threshold from the incidence model 
  numNewlyInfectedGAULs <- round(threshold * numVulnerable)
  oldPreds <- model_preds_initialThres %>% dplyr::filter(GAUL %in% infectedGAULS)
  model_preds_newThres <-  model_preds_initialThres %>% 
                           arrange(desc(.pred_1)) %>% 
                           dplyr::filter(!(GAUL %in% infectedGAULS)) %>%
                           mutate(.pred_class = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) 
                           
  model_preds_newThres <- rbind(oldPreds, model_preds_newThres)
  
  # #Force newGAULPredictions to be 1 where they were previously infected
   model_preds_newThres <- model_preds_newThres %>% mutate(.pred_class = case_when(GAUL %in% infectedGAULS ~ 1,
                                                                                   TRUE ~ as.numeric(as.character(.pred_class))))
  
  # #Need to convert new predictions into a factor in order to work with the yardstick functions
  model_preds_newThres$.pred_class <- factor(model_preds_newThres$.pred_class, levels=c(0,1))
   
  #Update class predictions with previous maximum probability of the first class if it decreases, update max if it increases. 
  if (i==1){
    maxClassProbs <- bind_cols(testset$GAUL,model_probs) %>% dplyr::rename(GAUL = `...1`)
  }
  
  if (i > 1) {
    for (j in 1:nrow(model_preds_newThres)){
      #Update max if it goes up, only for the 1 class
      if (model_preds_newThres$.pred_class[j]==1){
        if (model_preds_newThres$.pred_1[j] > maxClassProbs$.pred_1[j]){
          maxClassProbs$.pred_1[j] <- model_preds_newThres$.pred_1[j]
          maxClassProbs$.pred_0[j] <- model_preds_newThres$.pred_0[j]}
        else{
          #Maintain max if it goes down
          model_preds_newThres$.pred_1[j] <- maxClassProbs$.pred_1[j] 
          model_preds_newThres$.pred_0[j] <- maxClassProbs$.pred_0[j] 
        }        
      }
    }
}

  #Compute performance metrics and return
  model_preds_toScore <- model_preds_newThres %>% dplyr::filter(TestElig == 1)
  multimetric <- metric_set(yardstick::sens, yardstick::spec)
  metricBundle <-  rbind(multimetric(model_preds_toScore, truth = Label, estimate = .pred_class, event_level = "second"),
                         roc_auc(model_preds_toScore, truth = Label, .pred_1, event_level = "second"))
  
  compProbMetMTSCVOutputs <- list(metricBundle, maxClassProbs)
  return(compProbMetMTSCVOutputs)
}