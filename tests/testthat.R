#************************************* TESTTHAT **************************************#
# Contains unit tests for core functions                                              #
#*************************************************************************************#

library(testthat)
library(tidyverse)

# load functions
source("scripts/estimation.R")
source("scripts/simulation.R")
source("scripts/utilities.R")
source("scripts/placement.R")

# load some toy ridership data for testing
toydata <- read_csv("data/toy_data.csv")
toydata$start_station <- as.character(toydata$start_station)
toydata$end_station <- as.character(toydata$end_station)

# estimate_arrival_rates unit test (makes sure it is correct value)
arrival_rates <- estimate_arrival_rates(toydata)
expect_equal(arrival_rates$avg_trips[1], 1.5)

# find_lambda_max unit test (makes sure it is correct value)
lambda_max <- find_lambda_max(arrival_rates)
expect_equal(lambda_max$lambda_max[1], 1.5)

# simulate_day + simulate_route_arrivals unit test (times ought to be sorted)
simulation <- simulate_day(lambda_max)
expect_equal(simulation$arrival_time, sort(simulation$arrival_time))

# initialize_inventory (makes sure it correctly aggregates results)
initial_inventory_df <- data.frame(
  station = c("2", "4"),
  initial_count = c(5, 3)
)

init_inventory <- initialize_inventory(initial_inventory_df, simulation, lambda_max)
expect_equal(init_inventory["2"], c("2" = 5))
expect_equal(init_inventory["4"], c("4" = 3))

# optimization_prop (makes sure number of happy + unhappy riders equals total)
set.seed(100)
results <- optimization_prop(lambda_max, 50, 100)
total_riders <- nrow(simulate_day(lambda_max))
expect_equal(results$happy_riders + results$unhappy_riders, total_riders)