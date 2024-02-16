# ++++++++++++++++++++++++++++
# futureProjections
# ++++++++++++++++++++++++++++

#INPUTS

#OUTPUTS

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

futureProjections <- function(fittedWorkflow, country, fullDataset, thresholds, initialConditions, numYears, regionLUT, big_cities = NULL){
  #Initialize needed variables
  year_list <- group_split(fullDataset, Year_end)
  thresholdsToMerge <- list()
  predictionsToMerge <- list()
  probabilitiesToMerge <- list()
  bigCitiesMatricesToMerge <- list()
  
  #Initial conditions of infected regions in 2002 (from dat and not spread_dat) 
  allGAULS <- (admin2) 
  allGAULS <- allGAULS$GAUL_CODE
  infected <- initialConditions$GAUL
  
  #Run through range of years
  for (i in (1:numYears)){
    #Assemble covariates for each year's testing dataframe
    vulnerable <- allGAULS[!(allGAULS %in% infected)]
    spread_year <- i + 2019 #First prediction year is 2020
    
    updatedAnnualSpreadData <- addCovariates_Bra(GAULS_SOURCE = infected,
                                                 GAULS_DEST = allGAULS,
                                                 spreadYear = spread_year, 
                                                 fullDataset = fullDataset,
                                                 admin1 = admin1,
                                                 admin2 = admin2,
                                                 location_data = location_data,
                                                 regionLUT = regionLUT,
                                                 big_cities = big_cities)
    
    test_data <- updatedAnnualSpreadData[[1]] #Updated version returns list object, get first entry as dataframe

    
    #Denote test eligibility since the workflow needs this variable
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
                           dplyr::filter(!(GAUL %in% infected)) %>%
                           mutate(Infected = ifelse(row_number() <= (numNewlyInfectedGAULs), 1, 0)) %>%
                           dplyr::select(-OrigPred)
                        
    newGAULPredictions <- rbind(oldPreds, newGAULPredictions)
    
    #Force newGAULPredictions to be 1 where they were previously infected
    newGAULPredictions <- newGAULPredictions %>% mutate (Infected = case_when(GAUL %in% infected ~ 1,
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
    
    #Save mobility matrix
    if(spread_year %in% big_cities$YearInfected){
      bigCitiesMatrix <- updatedAnnualSpreadData[[2]]
      bigCitiesMatricesToMerge[[i]] <- bigCitiesMatrix
    }
    
    
  } #End loop
  
  futureProjectionsOutputList <- list()
  futureProjectionsOutputList[[1]] <- thresholds
  futureProjectionsOutputList[[2]] <- predictionsToMerge
  futureProjectionsOutputList[[3]] <- probabilitiesToMerge
  futureProjectionsOutputList[[4]] <- bigCitiesMatricesToMerge
  
  return(futureProjectionsOutputList)
}