# ++++++++++++++++++++++++++++
# tidyRegionalMetrics
# ++++++++++++++++++++++++++++

tidyRegionalMetricsBra <- function(model_preds_toScore){
  
  region_list <- group_split(model_preds_toScore, Region)
  test_CW <- region_list[[1]]
  test_N <- region_list[[2]]
  test_NE <- region_list[[3]]
  test_S <- region_list[[4]]
  test_SE <- region_list[[5]]
  
  multimetric <- metric_set(sens, spec)
  
  metrics_CW <- rbind(multimetric(test_CW, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_CW, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_N <-  rbind(multimetric(test_N, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_N, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_NE <-  rbind(multimetric(test_NE, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_NE, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_S <-  rbind(multimetric(test_S, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_S, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_SE <-  rbind(multimetric(test_SE, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_SE, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_Regional <- data.frame(matrix(ncol=4,nrow=5, dimnames=list(NULL, c("Region", "AUC", "SN","SP"))))
  
  metrics_Regional$Region[1] <- "CentralWest"
  metrics_Regional$AUC[1] <- metrics_CW$.estimate[3]
  metrics_Regional$SN[1] <- metrics_CW$.estimate[1]
  metrics_Regional$SP[1] <- metrics_CW$.estimate[2]
  
  metrics_Regional$Region[2] <- "North"
  metrics_Regional$AUC[2] <- metrics_N$.estimate[3]
  metrics_Regional$SN[2] <- metrics_N$.estimate[1]
  metrics_Regional$SP[2] <- metrics_N$.estimate[2]
  
  metrics_Regional$Region[3] <- "Northeast"
  metrics_Regional$AUC[3] <- metrics_NE$.estimate[3]
  metrics_Regional$SN[3] <- metrics_NE$.estimate[1]
  metrics_Regional$SP[3] <- metrics_NE$.estimate[2]
  
  metrics_Regional$Region[4] <- "South"
  metrics_Regional$AUC[4] <- metrics_S$.estimate[3]
  metrics_Regional$SN[4] <- metrics_S$.estimate[1]
  metrics_Regional$SP[4] <- metrics_S$.estimate[2]
  
  metrics_Regional$Region[5] <- "Southeast"
  metrics_Regional$AUC[5] <- metrics_SE$.estimate[3]
  metrics_Regional$SN[5] <- metrics_SE$.estimate[1]
  metrics_Regional$SP[5] <- metrics_SE$.estimate[2]
  
  return(metrics_Regional)
}