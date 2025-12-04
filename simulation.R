library(tidyverse)

# TODO: write documentation
simulate_day <- function(estimation) {
  
  all_results_list <- list() 
  
  # finds all unique routes in our data (routes not present have assumed
  # 0 hourly trips for all hours)
  unique_routes <- estimation %>%
    select(start_station, end_station) %>%
    distinct()
  
  for (i in 1:nrow(unique_routes)) {
    
    start_s <- unique_routes$start_station[i]
    end_s <- unique_routes$end_station[i]
    
    route_data <- estimation %>%
      filter(start_station == start_s & end_station == end_s) %>%
      arrange(hour)
    
    lambda_max <- route_data$lambda_max[1] 
    
    rates <- route_data$mu_hat

    accepted_arrivals <- simulate_route_arrivals(rates, lambda_max)

    #storing      
    route_results <- data.frame(
      start_station = rep(start_s, length(accepted_arrivals)),
      end_station = rep(end_s, length(accepted_arrivals)),
      arrival_time = accepted_arrivals
    )
    
    all_results_list[[i]] <- route_results
  }
  
  logged_trips <- bind_rows(all_results_list) %>%
    arrange(arrival_time) 
  
  return(logged_trips)
}
