# ++++++++++++++++++++++++++++
# gamComputeMetrics
# ++++++++++++++++++++++++++++
gamComputeMetrics <- function(gamModel, testset, threshold=0.5){

  predicted_values<-ifelse(predict(gamModel,newdata=testset, type="response")>threshold,1,0)
  actual_values<-testset$Infected
  conf_matrix<-table(predicted_values,actual_values)
  sn <- caret::sensitivity(conf_matrix)
  sp <- caret::specificity(conf_matrix)
  
  prob <- predict(gamModel,newdata=testset,type=c("response"))
  testset$prob <- prob
  auc <- as.numeric(roc(Infected ~ prob, data=testset)$auc)
  
  metrics_GAM <- data.frame(matrix(ncol=3,nrow=1, dimnames=list(NULL, c( "AUC", "SN","SP"))))
  metrics_GAM$AUC <- auc
  metrics_GAM$SN <- sn
  metrics_GAM$SP <- sp
  
  return(metrics_GAM)
}