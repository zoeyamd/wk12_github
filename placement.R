#************************************* PLACEMENT ************************************ #
#                                                                                     #
#                                                                                     #
# This code generates the initial placement of bikes for optimal rider happiness.     #
#*************************************************************************************#

#' Bike Placement Optimization and Simulation (Probability Method)
#'
#' @description calculates the optimal initial bike placement based on the 
#' likelihood of trip starts at hour 0.runs a full 24-hour simulation 
#' using the generated demand queue to evaluate the placement's effectiveness.
#' 
#' @param estimation tibble - estimated arrival rates (mu_hat) and peak rates 
#' (lambda_max) for all station-to-station routes across 24 hours.
#' @param fleet_size numeric - total number of bikes available for initial 
#' placement.
#' @param seed numeric - seed value used to ensure the simulation is 
#' reproducible.
#' 
#' @return A list containing the full simulation results:
#' {initial_inventory}:}proposed initial bike placement at each station.
#' {final_inventory}:} bike counts at each station after 24 hours of 
#' simulated activity.
#' {happy_riders}:} total count of successful trip requests.
#' {unhappy_riders}:} total count of failed trip requests.
#' {system_happiness_rate}:} performance metric

optimization_prop <- function(estimation, fleet_size, seed){
  
#simulation set up
  #set seed for reproducibility
  set.seed(seed)
  #generate the time-ordered queue of all trip requests for the day.
  simulated_trips <- simulate_day(estimation)

#initial placement calculation
  #calculate the proportion of bikes to place at each station 
  #based on trip starts at hour 0.
  placement_props <- estimation %>%
    filter(hour == 0) %>%
    group_by(start_station) %>%
    summarize(count= n(), .groups = 'drop') %>%
    
    #calculate proportion of trips starting at each station
    mutate(prop= count/sum(count))
 
  #translate proportion into bike counts based on fleet size                                 
  placement_plan <- placement_props %>%
    mutate(placement_bikes = round(prop * fleet_size)) %>%
    select(start_station, prop, placement_bikes)

  #adjust counts to ensure sum equals total fleet (rounding errors)                                  
  placement_plan <- adjust_for_rounding(placement_plan, fleet_size)
  
  #finalize initial inventory
  initial_inventory <- placement_plan %>%
    select(station = start_station, initial_count = placement_bikes) %>%
    distinct(station, .keep_all = TRUE)

  #intialize current inventory vector for loop
  #ensures all stations are accounted for, even if they received 0 bikes initially
  current_inventory <- initialize_inventory(initial_inventory, simulated_trips,
                                         estimation)
  
#bike movement + happiness simulation loop
  
  #counters for tracking simulation performance
  happy_riders <- 0 
  unhappy_riders <- 0

  #loop through each trip request
  for (i in 1:nrow(simulated_trips)) {
  
  #extract trip details for current trip
  trip <- simulated_trips[i, ]
  start_station <- as.character(trip$start_station) 
  end_station <- as.character(trip$end_station)  
  
  #check if bike is available at current start station
  if (current_inventory[start_station] > 0) {
    
    #success: rider found a bike
    happy_riders <- happy_riders + 1
    
    #update inventories: bike leaves start station...
    current_inventory[start_station] <- current_inventory[start_station] - 1
    #...and arrives at end station
    current_inventory[end_station] <- current_inventory[end_station] + 1
    
  } else {
    #failure: station is empty (bike request is denied)
    unhappy_riders <- unhappy_riders + 1
    }
  }

#final result calculation
  
  #calculate total riders
  total_riders <- happy_riders + unhappy_riders
  #calculate primary metric: the percentage of demand met/happy riders
  system_happiness_rate <- happy_riders / total_riders
  
  #convert final vector into data frame
  final_inventory <- data.frame(
    station = as.character(names(current_inventory)),
    bike_count = as.vector(current_inventory)
  )
  
  #return key results for analysis and visualization
  return(list(
    initial_inventory = initial_inventory,
    final_inventory = final_inventory,
    happy_riders = happy_riders,
    unhappy_riders = unhappy_riders,
    system_happiness_rate = system_happiness_rate
  ))
}


