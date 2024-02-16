# ++++++++++++++++++++++++++++
# timeseriesCV_medium
# ++++++++++++++++++++++++++++

#INPUTS
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in!
#initialConditions -> a vector of GAULS that will seed the model 
#threshold range -> range of thresholds for finding the optimal probability threshold for classification

#OUTPUTS
#the metric set for a set of years


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

timeseriesCV_mediumMex <- function(workflow, fullDataset, initialConditions, numYears, thresholds, regionLUT){

  #Initialize needed variables
  year_list <- group_split(fullDataset, Year_end)
  thresholdsToMerge <- list()
  metricsToMerge <- list()
  predictionsToMerge <- list() 
  probabilitiesToMerge <- list()
  maxClassProbs <- data.frame(.pred_0=integer(),
                              .pred_1=integer())

  #Initial conditions of infected regions in 2002 (from dat and not spread_dat) 
  allGAULS <- (admin2) 
  allGAULS <- allGAULS$GAUL_CODE
  infected <- initialConditions$GAUL
  
  #Train initially on all of the data 
  train_data <- fullDataset 
  fittedwflow <- workflow %>% fit(data = train_data)
  
  #Run through range of years
  for (i in (1:numYears)){
    #Assemble covariates for each year's testing dataframe

    #Evaluate performance everywhere so initial conditions is still used here
    vulnerable <- allGAULS[!(allGAULS %in% infected)]

    test_data <- addCovariates_Mex(GAULS_SOURCE = infected,
                                   GAULS_DEST = allGAULS,
                                   spreadYear = i + 1995, #First prediction year is 1996
                                   fullDataset = fullDataset,
                                   admin1 = admin1,
                                   admin2 = admin2,
                                   location_data = location_data,
                                   regionLUT = regionLUT) 
    test_data <- test_data[[1]] #Updated version returns list object, get first entry as dataframe
    
    #Denote test eligibility 
    testEligGAULs <- train_data %>% dplyr::filter(Year_end==1995+i) %>% dplyr::filter(TestElig==1) %>% dplyr::select(GAUL)
    test_data <- test_data %>% mutate(TestElig = case_when(GAUL %in% testEligGAULs$GAUL ~ 1,
                                                           TRUE ~ 0))

    threshold <- thresholds[i]
    
    numVulnerable <- length(vulnerable)
    
    #Generate values to merge
    compProbMetMTSCVOutputs <- computePerformanceMetricsMTSCV(fittedwflow, test_data, threshold, infected, i, maxClassProbs, numVulnerable)
    
    metrics <- compProbMetMTSCVOutputs[[1]]
    maxClassProbs <- compProbMetMTSCVOutputs[[2]]
    
    thresholdsToMerge[[i]] <- threshold
    metricsToMerge[[i]] <- metrics

    #Make predictions for all GAULS in test_data (2456 - # infected GAULS in initialConditions)
    modelPredictedProbs <- predict(fittedwflow, test_data, type = "prob")
    modelPredictedClasses <- predict(fittedwflow, test_data, type = "class")
    newGAULPredictionsDefaultThres <- bind_cols(test_data$GAUL, modelPredictedProbs, modelPredictedClasses)  %>% 
                                      dplyr::rename(GAUL = `...1`, OrigPred = .pred_class)
    oldPreds <- newGAULPredictionsDefaultThres %>% dplyr::filter(GAUL %in% infected) %>% rename(Infected = OrigPred)
    numNewlyInfectedGAULs <- round(threshold * numVulnerable)
    newGAULPredictions <-  newGAULPredictionsDefaultThres %>% 
                           arrange(desc(.pred_1)) %>% 
                           dplyr::filter(!(GAUL %in% infected)) %>%
                           mutate(Infected = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) %>%
                           dplyr::select(-OrigPred)
    
    newGAULPredictions <- rbind(oldPreds, newGAULPredictions)
    
    #Force newGAULPredictions to be 1 where they were previously infected
    newGAULPredictions <- newGAULPredictions %>% mutate(Infected = case_when(GAUL %in% infected ~ 1,
                                                                             TRUE ~ as.numeric(as.character(Infected))))
    
    #Update infected to new infections
    infected <- newGAULPredictions %>% dplyr::filter(Infected == 1) %>% dplyr::select(GAUL)
    infected <- infected$GAUL

    #Save model predictions
    predictionsToMerge[[i]] <- newGAULPredictions
    
    #Save probabilities for GAULs that are newly infected each year
    probsToSave <- newGAULPredictionsDefaultThres %>% 
                   arrange(desc(.pred_1)) %>% 
                   dplyr::filter(!(GAUL %in% infected))
    probsToSave <- probsToSave[1:numNewlyInfectedGAULs,]
    probabilitiesToMerge[[i]] <- probsToSave
    
   } #End loop

  #Tidy metrics/threshold and return
  metrics_TSCV <- data.frame(matrix(ncol=5,nrow=numYears, dimnames=list(NULL, c("Year","ProbThres", "AUC", "SN","SP"))))
  for (i in (1:numYears)){
    metrics <- metricsToMerge[[i]]
    metrics_TSCV$ProbThres[i]<- thresholdsToMerge[[i]]
    metrics_TSCV$Year[i] <- 1995 + i
    metrics_TSCV$AUC[i] <- metrics$.estimate[3]
    metrics_TSCV$SN[i] <- metrics$.estimate[1]
    metrics_TSCV$SP[i] <- metrics$.estimate[2]
  }
  
  tscvOutputList <- list()
  tscvOutputList[[1]] <- metrics_TSCV
  tscvOutputList[[2]] <- predictionsToMerge
  tscvOutputList[[3]] <- probabilitiesToMerge
  
  return(tscvOutputList)

}
