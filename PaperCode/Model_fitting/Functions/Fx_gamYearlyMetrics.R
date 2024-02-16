# ++++++++++++++++++++++++++++
# gamYearlyMetrics
# ++++++++++++++++++++++++++++

gamYearlyMetrics <- function(gamModel, testset){
  
  year_list <- group_split(testset, Year_end)
  test_02 <- year_list[[1]]
  test_03 <- year_list[[2]]
  test_04 <- year_list[[3]]
  test_05 <- year_list[[4]]
  test_06 <- year_list[[5]]
  test_07 <- year_list[[6]]
  test_08 <- year_list[[7]]
  test_09 <- year_list[[8]]
  test_10 <- year_list[[9]]
  test_11 <- year_list[[10]]
  test_12 <- year_list[[11]]
  test_13 <- year_list[[12]]
  test_14 <- year_list[[13]]
  test_15 <- year_list[[14]]
  test_16 <- year_list[[15]]
  test_17 <- year_list[[16]]
  test_18 <- year_list[[17]]
  test_19 <- year_list[[18]]
  
  metrics_02 <- gamComputeMetrics(gamModel, test_02)
  metrics_03 <- gamComputeMetrics(gamModel, test_03)
  metrics_04 <- gamComputeMetrics(gamModel, test_04)
  metrics_05 <- gamComputeMetrics(gamModel, test_05)
  metrics_06 <- gamComputeMetrics(gamModel, test_06)
  metrics_07 <- gamComputeMetrics(gamModel, test_07)
  metrics_08 <- gamComputeMetrics(gamModel, test_08)
  metrics_09 <- gamComputeMetrics(gamModel, test_09)
  metrics_10 <- gamComputeMetrics(gamModel, test_10)
  metrics_11 <- gamComputeMetrics(gamModel, test_11)
  metrics_12 <- gamComputeMetrics(gamModel, test_12)
  metrics_13 <- gamComputeMetrics(gamModel, test_13)
  metrics_14 <- gamComputeMetrics(gamModel, test_14)
  metrics_15 <- gamComputeMetrics(gamModel, test_15)
  metrics_16 <- gamComputeMetrics(gamModel, test_16)
  metrics_17 <- gamComputeMetrics(gamModel, test_17)
  metrics_18 <- gamComputeMetrics(gamModel, test_18)
  metrics_19 <- gamComputeMetrics(gamModel, test_19)
  
  metrics_02$Year <- 2002
  metrics_03$Year <- 2003
  metrics_04$Year <- 2004
  metrics_05$Year <- 2005
  metrics_06$Year <- 2006
  metrics_07$Year <- 2007
  metrics_08$Year <- 2008
  metrics_09$Year <- 2009
  metrics_10$Year <- 2010
  metrics_11$Year <- 2011
  metrics_12$Year <- 2012
  metrics_13$Year <- 2013
  metrics_14$Year <- 2014
  metrics_15$Year <- 2015
  metrics_16$Year <- 2016
  metrics_17$Year <- 2017
  metrics_18$Year <- 2018
  metrics_19$Year <- 2019
  
  yearlymetrics <- rbind(metrics_02, metrics_03, metrics_04, metrics_05, metrics_06, metrics_07, metrics_08,
                         metrics_09, metrics_10, metrics_11, metrics_12, metrics_13, metrics_14, metrics_15,
                         metrics_16, metrics_17, metrics_18, metrics_19)
  col_order <- c("Year", "AUC", "SN","SP")
  yearlymetrics <- yearlymetrics[, col_order]
  
  return(yearlymetrics)
}