#************************************* UTILITIES *************************************#
#                                                                                     #
#                                                                                     #
# This code holds helper functions for main simulation and optimization code.         #
#*************************************************************************************#


#' Simulate Trip Arrivals via Non-Homogeneous Poisson Process (NHPP)
#'
#' @description generates vector of continuous, fractional arrival times for a 
#' single route over 24 hours using the NHPP Thinning Algorithm
#' 
#' 
#' @param rates numeric vector - vector of 24 hourly rate estimates (mu_hat) 
#' for a specific route.
#' @param lambda_max numeric - peak trip rate (maximum mu_hat) for that route, 
#' used as the constant majorizing rate in the thinning process.
#' 
#' @return numeric vector containing the chronologically ordered, 
#' fractional arrival times for the simulated trips on that route.

simulate_route_arrivals <- function(rates, lambda_max, end_time_horizon = 24) {
  time <- 0
  accepted_arrivals <- c()
  
  #run the thinning process for one route
  while (time < end_time_horizon) {
    
    #exponential distribution of arrival times based based on lambda_max
    sample_time <- rexp(1, rate = lambda_max)
    time <- time + sample_time
    
    if (time >= end_time_horizon) {
      break
    }
    
    #thinning arrival times based on the true rate mu_hat
    current_hour_index <- floor(time) + 1 #converts time (0-23.99) to index (1-24)
    current_mu_hat <- rates[current_hour_index]
    
    acceptance_prob <- current_mu_hat / lambda_max
    U <- runif(1)
    
    if (U <= acceptance_prob) {
      accepted_arrivals <- c(accepted_arrivals, time)
    }
  }
  return(accepted_arrivals)
}


#' Adjust Bike Counts for Rounding Errors
#'
#' @description ensures sum of proposed bike placements exactly matches 
#' the total available fleet size by compensating for integer rounding errors. 
#' adjustment (adding or subtracting the difference) is made to the station 
#' that had the largest initial proportional allocation.
#' 
#' @param placement_plan tibble - data frame containing at least 'start_station', 
#' 'prop' (the initial proportion), and 'placement_bikes' (the rounded count).
#' @param fleet_size numeric -target total number of bikes available for deployment.
#' 
#' @return tibble identical to the input 'placement_plan', but with one station's 
#' 'placement_bikes' count adjusted so that the total sum equals 'fleet_size'.

adjust_for_rounding <- function(placement_plan, fleet_size) {
  
  #calculate initial total placed
  total_placed <- sum(placement_plan$placement_bikes)
  
  #calculate difference
  difference <- fleet_size - total_placed
  
  #adjust if total placed doesn't equal fleet size
  if (difference != 0) {
    
    #identify station with largest allocation
    largest_station <- placement_plan %>%
      arrange(desc(prop)) %>% #sorts by proportion in descending order
      head(1) %>%
      pull(start_station) #extracts station id of top candidate
    
    #apply correction to identified largest station
    placement_plan <- placement_plan %>%
      mutate(placement_bikes = ifelse(start_station == largest_station, 
                                      placement_bikes + difference, 
                                      placement_bikes))
  }
  #return adjusted plan, where total placed is guaranteed to match fleet size
  return(placement_plan)
}


#' Initialize Bike Inventory State
#'
#' @description creates complete, named vector representing the current bike 
#' count for every station relevant to the simulation. ensures that all stations, 
#' including those not receiving initial placement, are present and set to zero.
#' 
#' @param initial_inventory_df tibble - data frame detailing planned bike counts 
#' for stations receiving initial placement
#' @param simulated_trips tibble - full data frame of all simulated trips, used 
#' to identify all unique stations (origins and destinations) in the system.
#' @param estimation tibble - full estimation data, used as a fallback to ensure 
#' all stations from the historical data are included.
#' 
#' @return named numeric vector where names are station IDs and values are the 
#' initial bike counts, ensuring zero counts for unplaced stations.

initialize_inventory <- function(initial_inventory_df, simulated_trips, estimation_df){
  
  #identify all stations (from placement, trips, and original list)
  all_stations_id <- unique(c(
    as.character(initial_inventory_df$station),
    as.character(simulated_trips$start_station),
    as.character(simulated_trips$end_station)
  ))
  
  #create master zero-initialized data frame
  master_inventory_df <- data.frame(
    station = all_stations_id,
    bike_count = 0L, 
    stringsAsFactors = FALSE)
  
  #merge initial counts and convert to named vector
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
