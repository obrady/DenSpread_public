# ++++++++++++++++++++++++++++
# foldOverallMetricsTrees
# ++++++++++++++++++++++++++++

foldOverallMetricsGSCV <- function(metrics_B2M_Overall, metrics_M2B_Overall){
  metrics_NTSGSCV_Overall <- data.frame(matrix(ncol=3,nrow=6, dimnames=list(NULL, c("TrainCountry2TestCountry", "Metric", "Value"))))
  
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[1] <- "B2M"
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[2] <- "B2M"
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[3] <- "B2M"
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[4] <- "M2B"
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[5] <- "M2B"
  metrics_NTSGSCV_Overall$TrainCountry2TestCountry[6] <- "M2B"
  
  metrics_NTSGSCV_Overall$Metric[1] <- "AUC"
  metrics_NTSGSCV_Overall$Metric[2] <- "SP"
  metrics_NTSGSCV_Overall$Metric[3] <- "SN"
  metrics_NTSGSCV_Overall$Metric[4] <- "AUC"
  metrics_NTSGSCV_Overall$Metric[5] <- "SP"
  metrics_NTSGSCV_Overall$Metric[6] <- "SN"
  
  metrics_NTSGSCV_Overall$Value[1] <- metrics_B2M_Overall$.estimate[3]
  metrics_NTSGSCV_Overall$Value[2] <- metrics_B2M_Overall$.estimate[2]
  metrics_NTSGSCV_Overall$Value[3] <- metrics_B2M_Overall$.estimate[1]
  
  metrics_NTSGSCV_Overall$Value[4] <- metrics_M2B_Overall$.estimate[3]
  metrics_NTSGSCV_Overall$Value[5] <- metrics_M2B_Overall$.estimate[2]
  metrics_NTSGSCV_Overall$Value[6] <- metrics_M2B_Overall$.estimate[1]
  
  return(metrics_NTSGSCV_Overall)
}