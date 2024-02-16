# ++++++++++++++++++++++++++++
# tidyRegionalMetrics
# ++++++++++++++++++++++++++++

tidyRegionalMetricsMex <- function(model_preds_toScore){
  
  region_list <- group_split(model_preds_toScore, Region)
  test_C <- region_list[[1]]
  test_NE <- region_list[[2]]
  test_NW <- region_list[[3]]
  test_S <- region_list[[4]]
  test_SE <- region_list[[5]]
  test_W <- region_list[[6]]
  
  multimetric <- metric_set(sens, spec)
  
  metrics_C <- rbind(multimetric(test_C, truth = Infected, estimate = .pred_class, event_level = "second"),
                     roc_auc(test_C, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_NE <-  rbind(multimetric(test_NE, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_NE, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_NW <-  rbind(multimetric(test_NW, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_NW, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_S <-  rbind(multimetric(test_S, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_S, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_SE <-  rbind(multimetric(test_SE, truth = Infected, estimate = .pred_class, event_level = "second"),
                       roc_auc(test_SE, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_W <-  rbind(multimetric(test_W, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_W, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_Regional <- data.frame(matrix(ncol=4,nrow=6, dimnames=list(NULL, c("Region", "AUC", "SN","SP"))))
  
  metrics_Regional$Region[1] <- "Central"
  metrics_Regional$AUC[1] <- metrics_C$.estimate[3]
  metrics_Regional$SN[1] <- metrics_C$.estimate[1]
  metrics_Regional$SP[1] <- metrics_C$.estimate[2]
  
  metrics_Regional$Region[2] <- "Northeast"
  metrics_Regional$AUC[2] <- metrics_NE$.estimate[3]
  metrics_Regional$SN[2] <- metrics_NE$.estimate[1]
  metrics_Regional$SP[2] <- metrics_NE$.estimate[2]
  
  metrics_Regional$Region[3] <- "Northwest"
  metrics_Regional$AUC[3] <- metrics_NW$.estimate[3]
  metrics_Regional$SN[3] <- metrics_NW$.estimate[1]
  metrics_Regional$SP[3] <- metrics_NW$.estimate[2]
  
  metrics_Regional$Region[4] <- "South"
  metrics_Regional$AUC[4] <- metrics_S$.estimate[3]
  metrics_Regional$SN[4] <- metrics_S$.estimate[1]
  metrics_Regional$SP[4] <- metrics_S$.estimate[2]
  
  metrics_Regional$Region[5] <- "Southeast"
  metrics_Regional$AUC[5] <- metrics_SE$.estimate[3]
  metrics_Regional$SN[5] <- metrics_SE$.estimate[1]
  metrics_Regional$SP[5] <- metrics_SE$.estimate[2]
  
  metrics_Regional$Region[6] <- "West"
  metrics_Regional$AUC[6] <- metrics_W$.estimate[3]
  metrics_Regional$SN[6] <- metrics_W$.estimate[1]
  metrics_Regional$SP[6] <- metrics_W$.estimate[2]
  
  return(metrics_Regional)
}