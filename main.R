library(tidyverse)

## 1) DATA PREP

# load bike rides data
bike_data <- read_csv("data/sample_bike.csv")

# drop all rows with NA stations
bike_data <- bike_data %>%
  drop_na(start_station, end_station)

## 2) ESTIMATION

# estimate arrival rates end/start/hour trios present in the data
arrival_rates <- estimate_arrival_rates(bike_data)

# complete the data to include all possible end/start/hour trios
# mu for trios not present in the data is 0
# TODO: NOT SURE ABOUT THIS
arrival_rates <- complete_arrival_rates(arrival_rates)

# compute the largest hourly arrival rate for every station pair
lambda_max <- find_lambda_max(arrival_rates)

## 3) SIMULATION
