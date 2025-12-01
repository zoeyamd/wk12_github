simulate_route_arrivals <- function(rates, lambda_max, end_time_horizon = 24) {
  time <- 0
  accepted_arrivals <- c()
  
  # Run the thinning process for one route
  while (time < end_time_horizon) {
    
    # 1. Distribution (Proposing an arrival time based on lambda_max)
    sample_time <- rexp(1, rate = lambda_max)
    time <- time + sample_time
    
    if (time >= end_time_horizon) {
      break
    }
    
    # 2. Thinning/Rejection (Accepting the time based on the true rate mu_hat)
    current_hour_index <- floor(time) + 1 # Converts time (0-23.99) to index (1-24)
    current_mu_hat <- rates[current_hour_index]
    
    acceptance_prob <- current_mu_hat / lambda_max
    U <- runif(1)
    
    if (U <= acceptance_prob) {
      accepted_arrivals <- c(accepted_arrivals, time)
    }
  }
  return(accepted_arrivals)
}

adjust_for_rounding <- function(placement_plan, fleet_size) {
  total_placed <- sum(placement_plan$placement_bikes)
  difference <- fleet_size - total_placed
  
  if (difference != 0) {
    largest_station <- placement_plan %>%
      arrange(desc(prop)) %>% # Assuming 'prop' is in placement_plan
      head(1) %>%
      pull(start_station)
    
    placement_plan <- placement_plan %>%
      mutate(placement_bikes = ifelse(start_station == largest_station, 
                                      placement_bikes + difference, 
                                      placement_bikes))
  }
  return(placement_plan)
}

initialize_inventory <- function(initial_inventory_df, simulated_trips, estimation_df) {
  
  # 1. Identify ALL stations (from placement, trips, and original list)
  all_stations_id <- unique(c(
    as.character(initial_inventory_df$station),
    as.character(simulated_trips$start_station),
    as.character(simulated_trips$end_station)
  ))
  
  # 2. Create master zero-initialized data frame
  master_inventory_df <- data.frame(
    station = all_stations_id,
    bike_count = 0L, 
    stringsAsFactors = FALSE)
  
  # 3. Merge initial counts and convert to named vector
  master_inventory_with_placement <- master_inventory_df %>%
    dplyr::left_join(initial_inventory_df, by = "station") %>%
    dplyr::mutate(bike_count = dplyr::coalesce(initial_count, bike_count)) %>%
    dplyr::select(station, bike_count)
  
  current_inventory <- setNames(
    master_inventory_with_placement$bike_count, 
    master_inventory_with_placement$station
  )
  return(current_inventory)
}
