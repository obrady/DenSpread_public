# ++++++++++++++++++++++++++++
# maximizeJIndex
# ++++++++++++++++++++++++++++

#INPUTS
  #model_wflow -> tidymodel fitted worklow
  #testset -> whatever you pass in! 

#OUTPUTS
  #the threshold that maximizes predictive performance, where Youden's J Index is SN+SP-1 

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

maximizeJIndex <- function(workflow, testset, thresholdRange){
  
  #Generate an initial set of predictions to work from
  probabilities <- predict(workflow, testset, type = "prob")
  classes <- predict(workflow, testset, type = "class")
  predictions <- bind_cols(testset$Infected, probabilities, classes) %>% rename(Infected = 1)
  
  #Find probability cutoff to maximize Youden's J Index
  j_indices <- predictions %>% threshold_perf( Infected, .pred_0, thresholdRange) %>% dplyr::filter(.metric=="j_index") 
  thresholdAtMaxJ <- j_indices[which.max(j_indices$.estimate),]$.threshold 
  return(thresholdAtMaxJ)
}