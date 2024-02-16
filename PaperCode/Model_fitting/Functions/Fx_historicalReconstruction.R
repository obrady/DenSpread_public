# ++++++++++++++++++++++++++++
# Historical Reconstruction
# ++++++++++++++++++++++++++++

#INPUTS

#OUTPUTS

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

historicalReconstruction <- function(fittedWorkflow, country, fullDataset, startingYear, thresholds, seedingYears, seedingGAULs, numYears, regionLUT){
  
  #Initialize needed variables
  year_list <- group_split(fullDataset, Year_end)
  thresholdsToMerge <- list()
  metricsToMerge <- list()
  predictionsToMerge <- list() 
  probabilitiesToMerge <- list()
  
  if(7005 %in% seedingGAULs){scenario_seedFZ <- TRUE}else{scenario_seedFZ <- FALSE}
  if(9961 %in%seedingGAULs){scenario_seedRdJ <- TRUE}else{scenario_seedRdJ <- FALSE}
  if(11468 %in% seedingGAULs){scenario_seedRP <- TRUE}else{scenario_seedRP <- FALSE}
  if(6509 %in% seedingGAULs){scenario_seedMS <- TRUE}else{scenario_seedMS <- FALSE}
  if(10673 %in% seedingGAULs){scenario_seedBV <- TRUE}else{scenario_seedBV <- FALSE}
  
  #Use source to set up initial conditions
  allGAULS <- admin2
  allGAULS <- allGAULS$GAUL_CODE
  
  originSource <- seedingGAULs[which(seedingYears == min(seedingYears))]
  infected <- originSource
  
  #Loop through years up until target date
  for (i in 1:numYears){
    #Identify vulnerable GAULS 
    vulnerable <- allGAULS[!(allGAULS %in% infected)]
    
    test_data <- addCovariates_Bra(GAULS_SOURCE = infected,
                                    GAULS_DEST = allGAULS,
                                    spreadYear =  startingYear+i,
                                    fullDataset = fullDataset,
                                    admin1 = admin1,
                                    admin2 = admin2,
                                    location_data = location_data,
                                   regionLUT = regionLUT) 
    
    test_data <- test_data[[1]] #Updated version returns list object, get first entry as dataframe
    test_data <- test_data %>% mutate(TestElig = 1)
    
    threshold <- thresholds[i]
    
    numVulnerable <- length(vulnerable)
    
    #Make predictions for all GAULS
    modelPredictedProbs <- predict(fittedWorkflow, test_data, type = "prob")
    modelPredictedClasses <- predict(fittedWorkflow, test_data, type = "class")
    newGAULPredictionsDefaultThres <- bind_cols(test_data$GAUL, modelPredictedProbs, modelPredictedClasses)  %>% 
                                      dplyr::rename(GAUL = `...1`, OrigPred = .pred_class)
    oldPreds <- newGAULPredictionsDefaultThres %>% dplyr::filter(GAUL %in% infected) %>% rename(Infected = OrigPred)
    numNewlyInfectedGAULs <- round(abs(threshold) * numVulnerable)
    newGAULPredictions <-  newGAULPredictionsDefaultThres %>% 
                           arrange(desc(.pred_1)) %>% 
                           filter(!(GAUL %in% infected)) %>%
                           mutate(Infected = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) %>%
                           dplyr::select(-OrigPred)
    
    newGAULPredictions <- rbind(oldPreds, newGAULPredictions)
    
    #Force newGAULPredictions to be 1 where they were previously infected
    newGAULPredictions <- newGAULPredictions %>% mutate(Infected = case_when(GAUL %in% infected ~ 1,
                                                                             TRUE ~ as.numeric(as.character(Infected))))
    
    newGAULPredictions <- newGAULPredictions %>% mutate(Year=startingYear+i)
    
    #Update infected to new infections
    newInfections <- newGAULPredictions %>% filter(Infected == 1) %>% select(GAUL)
    newInfections <- newInfections$GAUL
    infected <- unique(c(infected, newInfections))
    
    #Check if scenario has certain known infections, but only keep each GAUL once in case the model suggests its already invaded
    if (isTRUE(scenario_seedFZ) & (startingYear+i) == 1986){ 
      infected <- c(infected, 7005)
      infected <- unique(infected)
    }
    
    if (isTRUE(scenario_seedRdJ) & (startingYear+i) == 1989){ 
      infected <- c(infected, 9961)
      infected <- unique(infected)
    }
    
    if (isTRUE(scenario_seedRP) & (startingYear+i) == 1990){ 
      infected <- c(infected, 11468)
      infected <- unique(infected)
    }
    
    if (isTRUE(scenario_seedMS) & (startingYear+i) == 1996){ 
      infected <- c(infected, 6509)
      infected <- unique(infected)
    }
    
    if (isTRUE(scenario_seedBV) & (startingYear+i) == 1998){ 
      infected <- c(infected, 10673)
      infected <- unique(infected)
    }
    
    #Save model predictions and probabilities
    predictionsToMerge[[i]] <- newGAULPredictions
    
    probsToSave <- newGAULPredictionsDefaultThres %>% 
                   arrange(desc(.pred_1)) %>% 
                   filter(!(GAUL %in% infected))
    probsToSave <- probsToSave[1:numNewlyInfectedGAULs,]
    probabilitiesToMerge[[i]] <- probsToSave
  } #End loop through years
  
  #Tidy returns 
  tscvOutputList <- list()
  tscvOutputList[[1]] <- predictionsToMerge
  tscvOutputList[[2]] <- probabilitiesToMerge

  return(tscvOutputList)

} #End historical projection function
  