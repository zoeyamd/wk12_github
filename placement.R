mock <- data.frame(hour = 0, start_station =c("A","B","A","C","B","C"),
                   end_station = c("B","A","C","A","C","B"))
                   
placement <- data.frame()

optimization <- function(mock, fleet_size){

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
initial_bike_inventory <- placement_plan %>%
  select(station = start_station, initial_inventory = placement_bikes)

flow_bike_inventory <- placement_plan %>%
  

  
  

}

optimization(mock, 10)
