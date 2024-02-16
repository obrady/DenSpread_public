# ++++++++++++++++++++++++++++
# TSGSCV_medium
# ++++++++++++++++++++++++++++

#INPUTS

#OUTPUTS


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

GSCV <- function(workflow, trainDataset, testDataset, initialConditions, admin1Test, admin2Test, location_data, numYears, thresholds, testCountry, regionLUT){
  
  #Initialize needed variables
  year_list <- group_split(testDataset, Year_end)
  thresholdsToMerge <- list()
  metricsToMerge <- list()
  predictionsToMerge <- list() 
  probabilitiesToMerge <- list()
  maxClassProbs <- data.frame(.pred_0=integer(),
                              .pred_1=integer())
  
  #Train initially from 2001-20017  (all of it)
  fittedwflow <-  workflow %>% fit(data = trainDataset)
  fullTestDataset <- testDataset
  
  #Initial conditions of infected regions in 2002 (from dat and not spread_dat) 
  allGAULS <- admin2Test 
  allGAULS <- allGAULS$GAUL_CODE
  infected <- initialConditions$GAUL
  
  #Run through range of years
  for (i in (1:numYears)){
    #Assemble covariates for each year's testing dataframe
    
    #Evaluate performance everywhere so initial conditions is still used here
    vulnerable <- allGAULS[!(allGAULS %in% infected)]
    
    if (testCountry=="Mexico"){
      # select appropriate distance matrices and environmntal covariates
      dist_mat_Adjacency = Mexico_dist_mat_Adjacency
      dist_mat_Air = Mexico_dist_mat_Air
      dist_mat_Friction = Mexico_dist_mat_Friction
      dist_mat_GC = Mexico_dist_mat_GC
      dist_mat_Grav = Mexico_dist_mat_Grav
      dist_mat_Migration = Mexico_dist_mat_Migration
      dist_mat_Rad = Mexico_dist_mat_Rad
      
      EVI_sum = Mexico_EVI_sum
      LandCover_sum = Mexico_LandCover_sum
      LST_day_sum = Mexico_LST_day_sum
      LST_night_sum = Mexico_LST_night_sum
      TCB_sum = Mexico_TCB_sum
      TCW_sum = Mexico_TCW_sum
      
      EVI_m_FP = Mexico_EVI_m_FP
      EVI_sd_FP = Mexico_EVI_sd_FP
      LST_day_m_FP = Mexico_LST_day_m_FP
      LST_day_sd_FP = Mexico_LST_day_sd_FP
      LST_night_m_FP = Mexico_LST_night_m_FP
      LST_night_sd_FP = Mexico_LST_night_sd_FP
      TCW_sd_FP = Mexico_TCW_sd_FP
      Movement_FP = Mexico_Movement_FP
      
      test_data <- addCovariates_Mex(GAULS_SOURCE = infected,
                                     GAULS_DEST = allGAULS,
                                     spreadYear = i + 1995, #First prediction year is 1996
                                     fullDataset = testDataset,
                                     admin1 = admin1Test,
                                     admin2 = admin2Test,
                                     location_data = location_data,
                                     regionLUT = regionLUT) 
      
      test_data <- test_data[[1]] #Updated version returns list object, get first entry as dataframe
      
      #Denote test eligibility 
      testEligGAULs <- fullTestDataset %>% filter(Year_end==1995+i) %>% filter(TestElig==1) %>% dplyr::select(GAUL)
      test_data <- test_data %>% mutate(TestElig = case_when(GAUL %in% testEligGAULs$GAUL ~ 1,
                                                             TRUE ~ 0))}
    
    if (testCountry=="Brazil"){
      # select appropriate distance matrices and environmntal covariates
      dist_mat_Adjacency = Brazil_dist_mat_Adjacency
      dist_mat_Air = Brazil_dist_mat_Air
      dist_mat_Friction = Brazil_dist_mat_Friction
      dist_mat_GC = Brazil_dist_mat_GC
      dist_mat_Grav = Brazil_dist_mat_Grav
      dist_mat_Migration = Brazil_dist_mat_Migration
      dist_mat_Rad = Brazil_dist_mat_Rad
      
      EVI_sum = Brazil_EVI_sum
      LandCover_sum = Brazil_LandCover_sum
      LST_day_sum = Brazil_LST_day_sum
      LST_night_sum = Brazil_LST_night_sum
      TCB_sum = Brazil_TCB_sum
      TCW_sum = Brazil_TCW_sum
      
      EVI_m_FP = Brazil_EVI_m_FP
      EVI_sd_FP = Brazil_EVI_sd_FP
      LST_day_m_FP = Brazil_LST_day_m_FP
      LST_day_sd_FP = Brazil_LST_day_sd_FP
      LST_night_m_FP = Brazil_LST_night_m_FP
      LST_night_sd_FP = Brazil_LST_night_sd_FP
      TCW_sd_FP = Brazil_TCW_sd_FP
      Movement_FP = Brazil_Movement_FP
      
      test_data <- addCovariates_Bra(GAULS_SOURCE = infected,
                                     GAULS_DEST = allGAULS,
                                     spreadYear = i + 2001, #First prediction year is 2002
                                     fullDataset = testDataset,
                                     admin1 = admin1Test,
                                     admin2 = admin2Test,
                                     location_data = location_data,
                                     regionLUT = regionLUT) 
      
      test_data <- test_data[[1]] #Updated version returns list object, get first entry as dataframe
      
      #Denote test eligibility 
      testEligGAULs <- fullTestDataset %>% filter(Year_end==2001+i) %>% filter(TestElig==1) %>% dplyr::select(GAUL)
      test_data <- test_data %>% mutate(TestElig = case_when(GAUL %in% testEligGAULs$GAUL ~ 1,
                                                             TRUE ~ 0))}
    
    
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
                           filter(!(GAUL %in% infected)) %>%
                           mutate(Infected = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) %>%
                           dplyr::select(-OrigPred)
    
    newGAULPredictions <- rbind(oldPreds, newGAULPredictions)
    
    #Force newGAULPredictions to be 1 where they were previously infected
    newGAULPredictions <- newGAULPredictions %>% mutate (Infected = case_when(GAUL %in% infected ~ 1,
                                                                              TRUE ~ as.numeric(as.character(Infected))))
    
    #Update infected to new infections
    infected <- newGAULPredictions %>% filter( Infected == 1) %>% dplyr::select(GAUL)
    infected <- infected$GAUL
    
    #Save model predictions
    predictionsToMerge[[i]] <- newGAULPredictions
    
    #Save probabilities for GAULs that are newly infected each year
    probsToSave <- newGAULPredictionsDefaultThres %>% 
                   arrange(desc(.pred_1)) %>% 
                   filter(!(GAUL %in% infected))
    probsToSave <- probsToSave[1:numNewlyInfectedGAULs,]
    probabilitiesToMerge[[i]] <- probsToSave
    
  } #End loop
  
  #Tidy metrics/threshold and return
  metrics_TSCV <- data.frame(matrix(ncol=5,nrow=numYears, dimnames=list(NULL, c("Year","ProbThres", "AUC", "SN","SP"))))
  
  if (testCountry=="Brazil"){
    for (i in (1:numYears)){
      metrics <- metricsToMerge[[i]]
      metrics_TSCV$ProbThres[i]<- thresholdsToMerge[[i]]
      metrics_TSCV$Year[i] <- 2001 + i
      metrics_TSCV$AUC[i] <- metrics$.estimate[3]
      metrics_TSCV$SN[i] <- metrics$.estimate[1]
      metrics_TSCV$SP[i] <- metrics$.estimate[2]
    }
  }
  
  if (testCountry=="Mexico"){
    for (i in (1:numYears)){
      metrics <- metricsToMerge[[i]]
      metrics_TSCV$ProbThres[i]<- thresholdsToMerge[[i]]
      metrics_TSCV$Year[i] <- 1995 + i
      metrics_TSCV$AUC[i] <- metrics$.estimate[3]
      metrics_TSCV$SN[i] <- metrics$.estimate[1]
      metrics_TSCV$SP[i] <- metrics$.estimate[2]
    }
  }
  
  tscvOutputList <- list()
  tscvOutputList[[1]] <- metrics_TSCV
  tscvOutputList[[2]] <- predictionsToMerge
  tscvOutputList[[3]] <- probabilitiesToMerge
  return(tscvOutputList)
  
}
