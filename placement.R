mock <- data.frame(hour = 0, start_station =c("A","B","A","C","B","C"),
                   end_station = c("B","A","C","A","C","B"))
                   
placement <- data.frame()

optimization <- function(mock, fleet_size){

simulation(mock)  
  
#initial placement
placement_props <-  mock %>%
    filter(hour == 0) %>%
    group_by(start_station) %>%
    summarize(count = n()) %>%
    mutate(prop = count/sum(count))

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

#bike movement
initial_inventory <- placement_plan %>%
  select(station = start_station, initial_inventory = placement_bikes)

  
#test happiness
# Ensure the inventory is a named vector for quick lookups and updates
current_inventory <- setNames(initial_inventory$initial_inventory, initial_inventory$station)

# Global counters
happy_riders <- 0
unhappy_riders <- 0


for (i in 1:nrow(mock)) {
  
  trip <- mock[i, ]
  start_station <- trip$start_station
  end_station <- trip$end_station
  
  if (current_inventory[start_station] > 0) {
    
    # SUCCESS: Rider gets a bike
    happy_riders <- happy_riders + 1
    
    current_inventory[start_station] <- current_inventory[start_station] - 1

    current_inventory[end_station] <- current_inventory[end_station] + 1
  } else {
    
    unhappy_riders <- unhappy_riders + 1
    
  }
}

total_riders <- happy_riders + unhappy_riders
system_happiness_rate <- happy_riders / total_riders

# Format final inventory back into a data frame
final_inventory_df <- data.frame(
  station_id = names(current_inventory),
  bike_count = as.vector(current_inventory)
)

return(list(
  final_inventory = final_inventory_df,
  happy_riders = happy_riders,
  unhappy_riders = unhappy_riders,
  system_happiness_rate = system_happiness_rate
))
  




}

print(optimization(mock, 10))

