# ++++++++++++++++++++++++++++
# timeseriesCV_short
# ++++++++++++++++++++++++++++

#INPUTS
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in! 

#OUTPUTS
#the metric set for a set of years

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

timeseriesCV_short <- function(workflow, dataset, initialConditions, startYear, numYears, numGAULs, confMatrixGAULs, thresholds){
  
  #Initialize needed variables
  year_list <- group_split(dataset, Year_end)
  thresholdsToMerge <- list()
  metricsToMerge <- list()
  predictionsToMerge <- list() 
  
  #Run through range of years
  for (i in (1:numYears)){
    #Generate training dataframe
    if (i == 1){
      train_data <- year_list[[i]]
    }else{
      dataToMerge <- list()
      for (j in 1:i){
        dataToMerge[[j]] <-  year_list[[j]] 
      }
      train_data <- bind_rows(dataToMerge)
    }
    
    #Find optimal threshold based on training data using incidence-based thresholding
    fittedwflow <-  workflow %>% fit(data = train_data)
    
    threshold <- thresholds[i]

    numVulnerable <- as.numeric(train_data %>% filter(Year_end==startYear-1+i) %>% tally(Infected==0))
    infectedGAULs <- train_data %>% filter(Year_end==startYear-1+i) %>% filter(Infected==1) %>% dplyr::select(GAUL)
    infectedGAULs <- infectedGAULs$GAUL
    
    #Get testing dataframe
    test_data <- year_list[[i+1]] 
      
    if (confMatrixGAULs==TRUE){
      confMatrixGAULs <- returnConfusionMatrixGAULs(fittedwflow, test_data, threshold, numVulnerable, infectedGAULs)
      return(confMatrixGAULs)
    }else if (confMatrixGAULs==FALSE){
      #Compute performance metrics and store predictions
      metrics <- computePerformanceMetrics(fittedwflow, test_data, threshold, numVulnerable, infectedGAULs)
      predictions <- generateSTSCVPredictions(fittedwflow, test_data, threshold, numVulnerable, infectedGAULs, startYear, i)
        
      #Store thresholds, metrics, and predictions 
      thresholdsToMerge[[i]] <- threshold
      metricsToMerge[[i]] <- metrics
      predictionsToMerge[[i]] <- predictions
      }
      
  }  # End loop running through years 
  
  #Tidy metrics/threshold and return 
  metrics_TSCV <- data.frame(matrix(ncol=5,nrow=numYears, dimnames=list(NULL, c("Year","ProbThres", "AUC", "SN","SP"))))
  for (i in (1:numYears)){
    metrics <- metricsToMerge[[i]]
    metrics_TSCV$ProbThres[i]<- thresholdsToMerge[[i]]
    metrics_TSCV$Year[i] <- startYear + i
    metrics_TSCV$AUC[i] <- metrics$.estimate[3]
    metrics_TSCV$SN[i] <- metrics$.estimate[1]
    metrics_TSCV$SP[i] <- metrics$.estimate[2] 
  }
  
  tscvOutputList <- list()
  tscvOutputList[[1]] <- metrics_TSCV
  tscvOutputList[[2]] <- predictionsToMerge
  
  return(tscvOutputList)
  
}
