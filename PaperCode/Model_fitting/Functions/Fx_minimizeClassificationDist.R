# ++++++++++++++++++++++++++++
# minimizeClassificationDist
# ++++++++++++++++++++++++++++

#INPUTS
#model_wflow -> tidymodel fitted worklow
#testset -> whatever you pass in! 

#OUTPUTS
#the threshold that maximizes predictive performance, where dis is the abs(SN-SP)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

minimizeClassificationDist <- function(workflow, testset, thresholdRange){
  
  #Generate an initial set of predictions to work from
  probabilities <- predict(workflow, testset, type = "prob")
  classes <- predict(workflow, testset, type = "class")
  predictions <- bind_cols(testset$Infected, probabilities, classes) %>% dplyr::rename(Infected = 1)
  
  #Find probability cutoff to minimize the absolute value of SN-SP
  #Note:  As of yardstick version 0.06... the global option yardstick.event_first will be used to determine which level is the event of interest. 
          # For more details, see the Relevant level section of yardstick::sens().
          # Our factor is set so that the SECOND level is the event of interest (1, infected) so we need the option statement up top
  # UPDATE: The global option, yardstick.event_first, has been deprecated in favor of the new explicit argument, event_level as of v0.07 
          # All metric functions that previously supported changing the “event” level have gained this new argument... 
          #Not sure if is passed onto probably::threshold_perf but have this for now
  #options(yardstick.event_first = FALSE)
  distances <- predictions %>% probably::threshold_perf(truth=Infected, estimate = .pred_1, thresholds = thresholdRange, event_level="second") %>% 
                               dplyr::filter(.metric!="j_index" & .metric!="distance") %>% 
                               arrange(.threshold) %>%
                               pivot_wider(names_from = .metric, values_from = .estimate) %>%
                               mutate(dist = abs(sensitivity - specificity)) %>% 
                               pivot_longer(cols =c(sensitivity, specificity, dist), names_to = ".metric", values_to = ".estimate") %>%
                               dplyr::filter(.metric=="dist") 
  
  thresholdAtMinDist <- distances[which.min(distances$.estimate),]$.threshold
  
  return(thresholdAtMinDist)
}