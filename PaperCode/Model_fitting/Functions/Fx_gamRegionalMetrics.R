# ++++++++++++++++++++++++++++
# gamRegionalMetrics
# ++++++++++++++++++++++++++++

gamRegionalMetrics <- function(gamModel, testset){
  
  region_list <- group_split(testset, Region)
  test_CW <- region_list[[1]]
  test_N <- region_list[[2]]
  test_NE <- region_list[[3]]
  test_S <- region_list[[4]]
  test_SE <- region_list[[5]]
  
  metrics_CW <- gamComputeMetrics(gamModel, test_CW)
  metrics_N <- gamComputeMetrics(gamModel, test_N)
  metrics_NE <- gamComputeMetrics(gamModel, test_NE)
  metrics_S <- gamComputeMetrics(gamModel, test_S)
  metrics_SE <- gamComputeMetrics(gamModel, test_SE)
  
  metrics_CW$Region="CentralWest"
  metrics_N$Region="North"
  metrics_NE$Region="Northeast"
  metrics_S$Region="South"
  metrics_SE$Region="Southeast"
  
  regionalmetrics <- rbind(metrics_CW,metrics_N,metrics_NE,metrics_S,metrics_SE)
  col_order <- c("Region", "AUC", "SN","SP")
  regionalmetrics <- regionalmetrics[, col_order]
  
  return(regionalmetrics)
  
}