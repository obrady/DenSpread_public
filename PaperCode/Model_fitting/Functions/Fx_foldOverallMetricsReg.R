# ++++++++++++++++++++++++++++
# foldOverallMetricsReg
# ++++++++++++++++++++++++++++


foldOverallMetrics <- function(metrics_LR_Overall, metrics_EN_Overall, metrics_GAM_Overall){
  metrics_Overall <- data.frame(matrix(ncol=3,nrow=9, dimnames=list(NULL, c("Model", "Metric", "Value"))))
  
  metrics_Overall$Model[1] <- "Log Reg"
  metrics_Overall$Model[2] <- "Log Reg"
  metrics_Overall$Model[3] <- "Log Reg"
  
  metrics_Overall$Model[4] <- "Elastic Net"
  metrics_Overall$Model[5] <- "Elastic Net"
  metrics_Overall$Model[6] <- "Elastic Net"
  
  metrics_Overall$Model[7] <- "GAM"
  metrics_Overall$Model[8] <- "GAM"
  metrics_Overall$Model[9] <- "GAM"
  
  metrics_Overall$Metric[1] <- "AUC"
  metrics_Overall$Metric[4] <- "AUC"
  metrics_Overall$Metric[7] <- "AUC"
  
  metrics_Overall$Metric[2] <- "SN"
  metrics_Overall$Metric[5] <- "SN"
  metrics_Overall$Metric[8] <- "SN"
  
  metrics_Overall$Metric[3] <- "SP"
  metrics_Overall$Metric[6] <- "SP"
  metrics_Overall$Metric[9] <- "SP"
  
  
  metrics_Overall$Value[1] <- metrics_LR_Overall$.estimate[3]
  metrics_Overall$Value[4] <- metrics_EN_Overall$.estimate[3]
  metrics_Overall$Value[7] <- metrics_GAM_Overall$AUC
  
  metrics_Overall$Value[2] <- metrics_LR_Overall$.estimate[1]
  metrics_Overall$Value[5] <- metrics_EN_Overall$.estimate[1]
  metrics_Overall$Value[8] <- metrics_GAM_Overall$SN
  
  metrics_Overall$Value[3] <- metrics_LR_Overall$.estimate[2]
  metrics_Overall$Value[6] <- metrics_EN_Overall$.estimate[2]
  metrics_Overall$Value[9] <- metrics_GAM_Overall$SP
  
  return(metrics_Overall)
}