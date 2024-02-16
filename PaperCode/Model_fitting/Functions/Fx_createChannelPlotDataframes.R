createChannelPlotDataframes <- function(inputDataframe){
  
  channelDataframes <- list()
  
  year_sum <- aggregate(YearInfected ~ YearPredicted + Region, FUN = quantile,
                        probs = c(0.025, 0.25, 0.5, 0.75, 0.975), data = inputDataframe)
  
  # create individual dataframes for each element of the plot
  line_element <- data.frame(x = year_sum$YearPredicted, y = year_sum$YearInfected[, 3], Region = year_sum$Region)
  u_regions = unique(year_sum$Region)
  
  for(i in 1:length(u_regions)){
    # subset to specific region
    f_year_sum = year_sum[year_sum$Region == u_regions[i], ]
    if(i == 1){
      CI75_element <- data.frame(x = c(f_year_sum$YearPredicted, rev(f_year_sum$YearPredicted)),
                                 y = c(f_year_sum$YearInfected[, 2], rev(f_year_sum$YearInfected[, 4])),
                                 Region = u_regions[i])
      CI95_element <- data.frame(x = c(f_year_sum$YearPredicted, rev(f_year_sum$YearPredicted)),
                                 y = c(f_year_sum$YearInfected[, 1], rev(f_year_sum$YearInfected[, 5])),
                                 Region = u_regions[i])
    }else{
      h1 <- data.frame(x = c(f_year_sum$YearPredicted, rev(f_year_sum$YearPredicted)),
                       y = c(f_year_sum$YearInfected[, 2], rev(f_year_sum$YearInfected[, 4])),
                       Region = u_regions[i])
      h2 <- data.frame(x = c(f_year_sum$YearPredicted, rev(f_year_sum$YearPredicted)),
                       y = c(f_year_sum$YearInfected[, 1], rev(f_year_sum$YearInfected[, 5])),
                       Region = u_regions[i])
      
      CI75_element = rbind(CI75_element, h1)
      CI95_element = rbind(CI95_element, h2)
    }
  }
  
  channelDataframes[[1]] <- line_element
  channelDataframes[[2]] <- CI75_element
  channelDataframes[[3]] <- CI95_element
  
  return(channelDataframes)
}