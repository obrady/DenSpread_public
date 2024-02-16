# ++++++++++++++++++++++++++++
# foldOverallMetricsOther
# ++++++++++++++++++++++++++++


foldOverallMetrics <- function(metrics_polySVM_Overall, metrics_rbfSVM_Overall, metrics_KNN_Overall){
  metrics_Overall <- data.frame(matrix(ncol=3,nrow=9, dimnames=list(NULL, c("Model", "Metric", "Value"))))
  
  metrics_Overall$Model[1] <- "Poly Kernel SVM"
  metrics_Overall$Model[2] <- "Poly Kernel SVM"
  metrics_Overall$Model[3] <- "Poly Kernel SVM"
  
  metrics_Overall$Model[4] <- "RBF Kernel SVM"
  metrics_Overall$Model[5] <- "RBF Kernel SVM"
  metrics_Overall$Model[6] <- "RBF Kernel SVM"
  
  metrics_Overall$Model[7] <- "KNN"
  metrics_Overall$Model[8] <- "KNN"
  metrics_Overall$Model[9] <- "KNN"
  
  metrics_Overall$Metric[1] <- "AUC"
  metrics_Overall$Metric[4] <- "AUC"
  metrics_Overall$Metric[7] <- "AUC"
  
  metrics_Overall$Metric[2] <- "SN"
  metrics_Overall$Metric[5] <- "SN"
  metrics_Overall$Metric[8] <- "SN"
  
  metrics_Overall$Metric[3] <- "SP"
  metrics_Overall$Metric[6] <- "SP"
  metrics_Overall$Metric[9] <- "SP"
  
  
  metrics_Overall$Value[1] <- metrics_polySVM_Overall$.estimate[3]
  metrics_Overall$Value[4] <- metrics_rbfSVM_Overall$.estimate[3]
  metrics_Overall$Value[7] <- metrics_KNN_Overall$.estimate[3]
  
  metrics_Overall$Value[2] <- metrics_polySVM_Overall$.estimate[1]
  metrics_Overall$Value[5] <- metrics_rbfSVM_Overall$.estimate[1]
  metrics_Overall$Value[8] <- metrics_KNN_Overall$.estimate[1]
  
  metrics_Overall$Value[3] <- metrics_polySVM_Overall$.estimate[2]
  metrics_Overall$Value[6] <- metrics_rbfSVM_Overall$.estimate[2]
  metrics_Overall$Value[9] <- metrics_KNN_Overall$.estimate[2]
  
  return(metrics_Overall)
}


