simulate_day <- function(estimation) {
  
  all_results_list <- list() 
  
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

    
    time <- 0
    accepted_arrivals <- c()
    end_time_horizon <- 24
  
#distribution 
    while(time < end_time_horizon) {
      
      sample_time <- rexp(1, rate = lambda_max)
      time <- time + sample_time
      
      if (time >= end_time_horizon) {
        break
      }
      
#thinning
      current_hour_index <- floor(time) + 1 #0:23 to 1:24
      current_mu_hat <- rates[current_hour_index]
      
      acceptance_prob <- current_mu_hat / lambda_max
      U <- runif(1)
      
      if (U <= acceptance_prob) {
        accepted_arrivals <- c(accepted_arrivals, time)
      }
    }

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
