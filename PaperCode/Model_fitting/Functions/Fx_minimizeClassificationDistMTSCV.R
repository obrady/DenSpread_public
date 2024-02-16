# ++++++++++++++++++++++++++++
# minimizeClassificationDistMTSCV
# ++++++++++++++++++++++++++++

#INPUTS
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in! 

#OUTPUTS
#the threshold that maximizes predictive performance, where dis is the abs(SN-SP)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

minimizeClassificationDistMTSCV <- function(workflow, testset, thresholdRange, infectedGAULS){
  
  #Generate an initial set of predictions to work from
  probabilities <- predict(workflow, testset, type = "prob")
  classes <- predict(workflow, testset, type = "class")
  predictions <- bind_cols(testset$GAUL ,testset$Infected, probabilities, classes) %>% dplyr::rename(GAUL = `...1`, Label = `...2`)
  
  #Force predictions to be 1 where they were previously infected
  predictions <- predictions %>% mutate (.pred_class = case_when(GAUL %in% infectedGAULS ~ 1,
                                                                 TRUE ~ as.numeric(as.character(.pred_class))))
  
  #Find probability cutoff to minimize the absolute value of SN-SP
  #Note:  As of yardstick version 0.06... the global option yardstick.event_first will be used to determine which level is the event of interest. 
          # For more details, see the Relevant level section of yardstick::sens().
          # Our factor is set so that the SECOND level is the event of interest (1, infected) so we need the option statement up top
  # UPDATE: The global option, yardstick.event_first, has been deprecated in favor of the new explicit argument, event_level as of v0.07 
          # All metric functions that previously supported changing the “event” level have gained this new argument... 
          # Not sure if is passed onto probably::threshold_perf but have this for now
  #options(yardstick.event_first = FALSE)
  distances <- predictions %>% threshold_perf(Label, .pred_1, thresholdRange, event_level="second") %>% 
               dplyr::filter(.metric!="j_index" & .metric!="distance") %>% 
               arrange(.threshold) %>%
               pivot_wider(names_from = .metric, values_from = .estimate) %>%
               mutate(dist = abs(sens - spec)) %>% 
               pivot_longer(cols =c(sens, spec, dist), names_to = ".metric", values_to = ".estimate") %>%
               dplyr::filter(.metric=="dist") 
  
  thresholdAtMinDist <- distances[which.min(distances$.estimate),]$.threshold
  
  return(thresholdAtMinDist)
}