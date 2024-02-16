# ++++++++++++++++++++++++++++
# gamRegionalMetricsMex
# ++++++++++++++++++++++++++++

gamRegionalMetrics <- function(gamModel, testset){
  
  region_list <- group_split(testset, Region)
  test_C <- region_list[[1]]
  test_NE <- region_list[[2]]
  test_NW <- region_list[[3]]
  test_S <- region_list[[4]]
  test_SE <- region_list[[5]]
  test_W <- region_list[[6]]
  
  metrics_C <- gamComputeMetrics(gamModel, test_C)
  metrics_NE <- gamComputeMetrics(gamModel, test_NE)
  metrics_NW <- gamComputeMetrics(gamModel, test_NW)
  metrics_S <- gamComputeMetrics(gamModel, test_S)
  metrics_SE <- gamComputeMetrics(gamModel, test_SE)
  metrics_W <- gamComputeMetrics(gamModel, test_W)
  
  metrics_C$Region="Central"
  metrics_NE$Region="Northeast"
  metrics_NW$Region="Northwest"
  metrics_S$Region="South"
  metrics_SE$Region="Southeast"
  metrics_W$Region="West"
  
  regionalmetrics <- rbind(metrics_C,metrics_NE,metrics_NW,metrics_S,metrics_SE,metrics_W)
  col_order <- c("Region", "AUC", "SN","SP")
  regionalmetrics <- regionalmetrics[, col_order]
  
  return(regionalmetrics)
  
}