optimization_prop <- function(estimation, fleet_size, seed){

#simulation
  set.seed(seed)
  simulated_trips <- simulate_day(estimation)

#initial placement
  placement_props <- estimation %>%
    filter(hour == 0) %>%
    group_by(start_station) %>%
    summarize(count= n()) %>%
    mutate(prop= count/sum(count))
                                  
  placement_plan <- placement_props %>%
    mutate(placement_bikes = round(prop * fleet_size)) %>%
    select(start_station, prop, placement_bikes)
                                  
  total_placed <- sum(placement_plan$placement_bikes)
  difference <- fleet_size - total_placed
                                  
  if (difference != 0) {
    largest_station <- placement_props %>%
      arrange(desc(count)) %>%
      head(1) %>%
      pull(start_station)
    
    placement_plan <- placement_plan %>%
      mutate(placement_bikes = ifelse(start_station == largest_station, 
                                      placement_bikes + difference, 
                                      placement_bikes))
    }

  initial_inventory <- placement_plan %>%
    select(station = start_station, initial_inventory = placement_bikes)
  
#organizing inventory
  current_inventory <- setNames(initial_inventory$initial_inventory, 
                                initial_inventory$station)
  
  all_stations_involved <- unique(c(
    simulated_trips$start_station, 
    simulated_trips$end_station,
    names(current_inventory)
  ))
  
  inventory_extended <- current_inventory[all_stations_involved]
  current_inventory <- tidyr::replace_na(inventory_extended, 0)
  
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
    station_id = names(current_inventory),
    bike_count = as.vector(current_inventory)
  )
  
  return(list(
    initial_inventory = initial_inventory,
    final_inventory = final_inventory,
    happy_riders = happy_riders,
    unhappy_riders = unhappy_riders,
    system_happiness_rate = system_happiness_rate))
}

print(optimization_prop(complete_estimated_arrivals, 300, 123))




optimization_flow <- function(estimation, fleet_size, seed){
  set.seed(seed)
  
#simulate trips
  simulated_trips <- simulate_day(estimation)

#track departures/arrivals for initial placement  
    departures <- simulated_trips %>%
      mutate(station_id = as.character(start_station)) %>% 
      group_by(station_id) %>%
      summarize(outflow = n(), .groups = 'drop')
    
    arrivals <- simulated_trips %>%
      mutate(station_id = as.character(end_station)) %>% # FIX 1: Standardize type
      group_by(station_id) %>%
      summarize(inflow = n(), .groups = 'drop')
    
    net_flow <- full_join(departures, arrivals, by = "station_id") %>%
      mutate(
        outflow = tidyr::replace_na(outflow, 0),
        inflow = tidyr::replace_na(inflow, 0),
        net_change = inflow - outflow
      )
    
#initial placement
    starving_stations_flow <- net_flow %>%
      filter(net_change < 0) %>% 
      mutate(abs_loss = abs(net_change))
    
      placement_plan <- starving_stations_flow %>%
        mutate(
          prop = abs_loss / sum(abs_loss), 
          placement_bikes = round(prop * fleet_size)
        ) %>%
        select(start_station = station_id, prop, placement_bikes)
      
      total_placed <- sum(placement_plan$placement_bikes)
      difference <- fleet_size - total_placed
      
      if (difference != 0) {
        largest_station <- placement_plan %>%
          arrange(desc(prop)) %>%
          head(1) %>%
          pull(start_station)
        
        placement_plan <- placement_plan %>%
          mutate(placement_bikes = ifelse(start_station == largest_station, 
                                          placement_bikes + difference, 
                                          placement_bikes))
      }      
      
    all_original_stations <- estimation %>% distinct(start_station) %>% pull(start_station)
    
    initial_inventory <- data.frame(start_station = all_original_stations) %>%
      left_join(placement_plan, by = "start_station") %>%
      mutate(placement_bikes = tidyr::replace_na(placement_bikes, 0))
  
#organizing inventory
  current_inventory <- setNames(initial_inventory$placement_bikes, 
                                initial_inventory$start_station)
  
  all_stations_involved <- unique(c(
    simulated_trips$start_station, 
    simulated_trips$end_station,
    names(current_inventory)
  ))
  
  inventory_extended <- current_inventory[all_stations_involved]
  current_inventory <- tidyr::replace_na(inventory_extended, 0)
  
#bike movement + happiness
  
  happy_riders <- 0
  unhappy_riders <- 0
  
  for (i in 1:nrow(simulated_trips)) {
    
    trip <- simulated_trips[i, ]
    start_station <- as.character(trip$start_station) 
    end_station <- as.character(trip$end_station)    
    
    if (current_inventory[start_station] > 0) {
      
      #success
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
    station_id = names(current_inventory),
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

print(optimization_flow(complete_estimated_arrivals, 300, 123))
