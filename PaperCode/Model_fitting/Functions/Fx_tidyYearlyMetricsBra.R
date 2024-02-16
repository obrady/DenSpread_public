# ++++++++++++++++++++++++++++
# tidyYearlyMetrics
# ++++++++++++++++++++++++++++

tidyYearlyMetricsBra <- function(model_preds_toScore){
  year_list <- group_split(model_preds_toScore, Year)
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
  
  multimetric <- metric_set(sens, spec)
  
  metrics_02 <- rbind(multimetric(test_02, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_02, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_03 <- rbind(multimetric(test_03, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_03, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_04 <- rbind(multimetric(test_04, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_04, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_05 <- rbind(multimetric(test_05, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_05, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_06 <- rbind(multimetric(test_06, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_06, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_07 <- rbind(multimetric(test_07, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_07, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_08 <- rbind(multimetric(test_08, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_08, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_09 <- rbind(multimetric(test_09, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_09, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_10 <- rbind(multimetric(test_10, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_10, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_11 <- rbind(multimetric(test_11, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_11, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_12 <- rbind(multimetric(test_12, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_12, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_13 <- rbind(multimetric(test_13, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_13, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_14 <- rbind(multimetric(test_14, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_14, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_15 <- rbind(multimetric(test_15, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_15, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_16 <- rbind(multimetric(test_16, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_16, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_17 <- rbind(multimetric(test_17, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_17, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_18 <- rbind(multimetric(test_18, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_18, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_19 <- rbind(multimetric(test_19, truth = Infected, estimate = .pred_class, event_level = "second"),
                      roc_auc(test_19, truth =Infected, .pred_1, event_level = "second"))
  
  metrics_Yearly <- data.frame(matrix(ncol=4,nrow=18, dimnames=list(NULL, c("Year", "AUC", "SN","SP"))))
  

  metrics_Yearly$Year[1] <- 2002
  metrics_Yearly$AUC[1] <- metrics_02$.estimate[3]
  metrics_Yearly$SN[1] <- metrics_02$.estimate[1]
  metrics_Yearly$SP[1] <- metrics_02$.estimate[2]
  
  metrics_Yearly$Year[2] <- 2003
  metrics_Yearly$AUC[2] <- metrics_03$.estimate[3]
  metrics_Yearly$SN[2] <- metrics_03$.estimate[1]
  metrics_Yearly$SP[2] <- metrics_03$.estimate[2]
  
  metrics_Yearly$Year[3] <- 2004
  metrics_Yearly$AUC[3] <- metrics_04$.estimate[3]
  metrics_Yearly$SN[3] <- metrics_04$.estimate[1]
  metrics_Yearly$SP[3] <- metrics_04$.estimate[2]
  
  metrics_Yearly$Year[4] <- 2005
  metrics_Yearly$AUC[4] <- metrics_05$.estimate[3]
  metrics_Yearly$SN[4] <- metrics_05$.estimate[1]
  metrics_Yearly$SP[4] <- metrics_05$.estimate[2]
  
  metrics_Yearly$Year[5] <- 2006
  metrics_Yearly$AUC[5] <- metrics_06$.estimate[3]
  metrics_Yearly$SN[5] <- metrics_06$.estimate[1]
  metrics_Yearly$SP[5] <- metrics_06$.estimate[2]
  
  metrics_Yearly$Year[6] <- 2007
  metrics_Yearly$AUC[6] <- metrics_07$.estimate[3]
  metrics_Yearly$SN[6] <- metrics_07$.estimate[1]
  metrics_Yearly$SP[6] <- metrics_07$.estimate[2]
  
  metrics_Yearly$Year[7] <- 2008
  metrics_Yearly$AUC[7] <- metrics_08$.estimate[3]
  metrics_Yearly$SN[7] <- metrics_08$.estimate[1]
  metrics_Yearly$SP[7] <- metrics_08$.estimate[2]
  
  metrics_Yearly$Year[8] <- 2009
  metrics_Yearly$AUC[8] <- metrics_09$.estimate[3]
  metrics_Yearly$SN[8] <- metrics_09$.estimate[1]
  metrics_Yearly$SP[8] <- metrics_09$.estimate[2]
  
  metrics_Yearly$Year[9] <- 2010
  metrics_Yearly$AUC[9] <- metrics_10$.estimate[3]
  metrics_Yearly$SN[9] <- metrics_10$.estimate[1]
  metrics_Yearly$SP[9] <- metrics_10$.estimate[2]
  
  metrics_Yearly$Year[10] <- 2011
  metrics_Yearly$AUC[10] <- metrics_11$.estimate[3]
  metrics_Yearly$SN[10] <- metrics_11$.estimate[1]
  metrics_Yearly$SP[10] <- metrics_11$.estimate[2]
  
  metrics_Yearly$Year[11] <- 2012
  metrics_Yearly$AUC[11] <- metrics_12$.estimate[3]
  metrics_Yearly$SN[11] <- metrics_12$.estimate[1]
  metrics_Yearly$SP[11] <- metrics_12$.estimate[2]
  
  metrics_Yearly$Year[12] <- 2013
  metrics_Yearly$AUC[12] <- metrics_13$.estimate[3]
  metrics_Yearly$SN[12] <- metrics_13$.estimate[1]
  metrics_Yearly$SP[12] <- metrics_13$.estimate[2]
  
  metrics_Yearly$Year[13] <- 2014
  metrics_Yearly$AUC[13] <- metrics_14$.estimate[3]
  metrics_Yearly$SN[13] <- metrics_14$.estimate[1]
  metrics_Yearly$SP[13] <- metrics_14$.estimate[2]
  
  metrics_Yearly$Year[14] <- 2015
  metrics_Yearly$AUC[14] <- metrics_15$.estimate[3]
  metrics_Yearly$SN[14] <- metrics_15$.estimate[1]
  metrics_Yearly$SP[14] <- metrics_15$.estimate[2]
  
  metrics_Yearly$Year[15] <- 2016
  metrics_Yearly$AUC[15] <- metrics_16$.estimate[3]
  metrics_Yearly$SN[15] <- metrics_16$.estimate[1]
  metrics_Yearly$SP[15] <- metrics_16$.estimate[2]
  
  metrics_Yearly$Year[16] <- 2017
  metrics_Yearly$AUC[16] <- metrics_17$.estimate[3]
  metrics_Yearly$SN[16] <- metrics_17$.estimate[1]
  metrics_Yearly$SP[16] <- metrics_17$.estimate[2]
  
  metrics_Yearly$Year[17] <- 2018
  metrics_Yearly$AUC[17] <- metrics_18$.estimate[3]
  metrics_Yearly$SN[17] <- metrics_18$.estimate[1]
  metrics_Yearly$SP[17] <- metrics_18$.estimate[2]
  
  metrics_Yearly$Year[18] <- 2019
  metrics_Yearly$AUC[18] <- metrics_19$.estimate[3]
  metrics_Yearly$SN[18] <- metrics_19$.estimate[1]
  metrics_Yearly$SP[18] <- metrics_19$.estimate[2]
  
  return(metrics_Yearly)
  
}