library(tidyverse)

optimization_prop <- function(estimation, fleet_size, seed){
#simulation
  set.seed(seed)
  simulated_trips <- simulate_day(estimation)

#initial placement
  placement_props <- estimation %>%
    filter(hour == 0) %>%
    group_by(start_station) %>%
    summarize(count= n(), .groups = 'drop') %>%
    mutate(prop= count/sum(count))
                                  
  placement_plan <- placement_props %>%
    mutate(placement_bikes = round(prop * fleet_size)) %>%
    select(start_station, prop, placement_bikes)
                                  
  placement_plan <- adjust_for_rounding(placement_plan, fleet_size)

  initial_inventory <- placement_plan %>%
    select(station = start_station, initial_count = placement_bikes) %>%
    distinct(station, .keep_all = TRUE)
  
  current_inventory <- initialize_inventory(initial_inventory, simulated_trips,
                                         estimation)
  
#bike movement + happiness    
  happy_riders <- 0 
  unhappy_riders <- 0

  for (i in 1:nrow(simulated_trips)) {
  
  trip <- simulated_trips[i, ]
  start_station <- as.character(trip$start_station) 
  end_station <- as.character(trip$end_station)  
  
  if (current_inventory[start_station] > 0) {
    #sucess
    happy_riders <- happy_riders + 1
    current_inventory[start_station] <- current_inventory[start_station] - 1
    current_inventory[end_station] <- current_inventory[end_station] + 1
  } else {
    #failure
    unhappy_riders <- unhappy_riders + 1
    }
  }
  
  total_riders <- happy_riders + unhappy_riders
  system_happiness_rate <- happy_riders / total_riders
  
  final_inventory <- data.frame(
    station = as.character(names(current_inventory)),
    bike_count = as.vector(current_inventory)
  )
  
  return(list(
    initial_inventory = initial_inventory,
    final_inventory = final_inventory,
    happy_riders = happy_riders,
    unhappy_riders = unhappy_riders,
    system_happiness_rate = system_happiness_rate
  ))
}

print(optimization_prop(complete_estimated_arrivals, 200, 123))

