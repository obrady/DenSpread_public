returnConfusionMatrixGAULs <- function(workflow, testset, threshold, numVulnerable, infectedGAULs){
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
                           filter(!(GAUL %in% infectedGAULs)) %>%
                           mutate(.pred_class = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) 
  model_preds_newThres <- rbind(oldPreds, model_preds_newThres)
  
  # #Force newGAULPredictions to be 1 where they were previously infected
  model_preds_newThres <- model_preds_newThres %>% mutate (.pred_class = case_when(GAUL %in% infectedGAULs ~ 1,
                                                                                   TRUE ~ as.numeric(as.character(.pred_class))))
  
  #Need to convert new predictions into a factor in order to work with the yardstick functions
  model_preds_newThres$.pred_class <- factor(model_preds_newThres$.pred_class, levels=c(0,1))
  
  #Determine mispredictions
  model_preds_toScore <- model_preds_newThres %>% filter(TestElig == 1)
  truePos <- as.list(model_preds_toScore %>% filter(Infected==1 & .pred_class == 1) %>% dplyr::select(GAUL))
  trueNeg <- as.list(model_preds_toScore %>% filter(Infected==0 & .pred_class == 0) %>% dplyr::select(GAUL))
  falsePos <- as.list(model_preds_toScore %>% filter(Infected==0 & .pred_class == 1) %>% dplyr::select(GAUL))
  falseNeg <- as.list(model_preds_toScore %>% filter(Infected==1 & .pred_class == 0) %>% dplyr::select(GAUL))

  #Boxplot of probabilities 
  tp_predsDF <- model_preds_toScore %>% filter(GAUL %in% truePos$GAUL)
  tn_predsDF <- model_preds_toScore %>% filter(GAUL %in% trueNeg$GAUL)
  fp_predsDF <- model_preds_toScore %>% filter(GAUL %in% falsePos$GAUL)
  fn_predsDF <- model_preds_toScore %>% filter(GAUL %in% falseNeg$GAUL)
  
  boxplot(tp_predsDF$.pred_1,tn_predsDF$.pred_1, fp_predsDF$.pred_1, fn_predsDF$.pred_1, 
          names=c("TP", "TN","FP", "FN"))
  
  confMatrixGAULs <- list(truePos, trueNeg, falsePos,falseNeg)
  names(confMatrixGAULs) <- c("truePos", "trueNeg", "falsePos","falseNeg")
  return(confMatrixGAULs)
}