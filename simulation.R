#************************************* SIMULATION ************************************#
#                                                                                     #
#                                                                                     #
# This code generates the simulated ride requests for a 24-hour period.               #
#*************************************************************************************#

#simulation function
simulate_day <- function(estimation) {
  
  #intialize list to store simulated trips
  all_results_list <- list() 
  
  #identify routes from unique start-end station pairs. will loop for each route
  unique_routes <- estimation %>%
    select(start_station, end_station) %>%
    distinct()
  
  #start loop for each unique route
  for (i in 1:nrow(unique_routes)) {
    
    #extract current route's start and end station
    start_s <- unique_routes$start_station[i]
    end_s <- unique_routes$end_station[i]
    
    #filter estimation data to get parameters for current route
    route_data <- estimation %>%
      filter(start_station == start_s & end_station == end_s) %>%
      arrange(hour)
    
    #extract maximum rate
    lambda_max <- route_data$lambda_max[1] 
    
    #extract hourly rate
    rates <- route_data$mu_hat

    #KEY STEP: run utility function to run the Non-Homogeneous Poisson 
    #Process (NHPP) Thinning Algorithm and generate the specific arrival times
    accepted_arrivals <- simulate_route_arrivals(rates, lambda_max)

    #storing results for current route
    route_results <- data.frame(
      start_station = rep(start_s, length(accepted_arrivals)),
      end_station = rep(end_s, length(accepted_arrivals)),
      arrival_time = accepted_arrivals
    )
    
    #add current route's trip to master list
    all_results_list[[i]] <- route_results
  }
  
  #combine individual route trips into master data frame, arranged by arrival time
  logged_trips <- bind_rows(all_results_list) %>%
    arrange(arrival_time) 
  
  #return time-ordered list of all trip requests for simulated 24-hour period
  return(logged_trips)
}
