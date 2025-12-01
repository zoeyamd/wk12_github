#************************************* ANALYSIS **************************************#
#                                                                                     #
#                                                                                     #
# This code runs optimization pipeline across multiple fleet sizes.                   #
#*************************************************************************************#


#load dependencies and funcions
library(dplyr)
library(tidyr)

#replace path to where your functions are defined
source("/Users/zoeybug/Documents/GitHub/wk12_github/scripts/estimation.R")
source("/Users/zoeybug/Documents/GitHub/wk12_github/scripts/utilities.R")
source("/Users/zoeybug/Documents/GitHub/wk12_github/scripts/simulation.R")
source("/Users/zoeybug/Documents/GitHub/wk12_github/scripts/placement.R")

#load input data
#replace 'path/to/your/data.csv' and 'read.csv' with your actual data loading command.
bike_data <- read_csv("~/Downloads/1560/Data/sample_bike.csv")

#run estimation functions to get estimation data
estimated_arrivals <- estimate_arrival_rates(bike_data)
complete_estimated_arrivals <- find_lambda_max(estimated_arrivals)
complete_estimated_arrivals <- complete_estimated_arrivals %>%
  filter(start_station != "15")

#define fleet sizes and set seed
fleet_sizes_to_test <- c(10, 50, 300) 
seed <- 42

#wrapper function for running and summarizing results
simulate_and_analyze <- function(fleet_size, estimation_data, seed) {
  
  cat("Running simulation for Fleet Size:", fleet_size, "\n")
  
  #run optimization and simulation function
  results <- optimization_prop(
    estimation = estimation_data, 
    fleet_size = fleet_size, 
    seed = seed)
  
  #extract top placement station (for insight)
  top_placement <- results$initial_inventory %>%
    arrange(desc(initial_count)) %>%
    head(1)
  
  #extract and format key indicators
  summary_df <- data.frame(
    fleet_size = fleet_size,
    happiness_rate = results$system_happiness_rate,
    happy_riders = results$happy_riders,
    unhappy_riders = results$unhappy_riders,
    top_placement_station = top_placement$station,
    top_placement_count = top_placement$initial_count)
  
  #return the concise summary data frame
  return(summary_df)
}

#run wrapper for fleet size [1]
fleet_1 <- simulate_and_analyze(fleet_sizes_to_test[1], complete_estimated_arrivals, seed)

#run wrapper for fleet size [2]
fleet_2 <- simulate_and_analyze(fleet_sizes_to_test[2], complete_estimated_arrivals, seed)

#run wrapper for fleet size [3]
fleet_3 <- simulate_and_analyze(fleet_sizes_to_test[3], complete_estimated_arrivals, seed)

#combine resulting data frame into single comparison table
comparison_table <- bind_rows(fleet_1, fleet_2, fleet_3)
print(comparison_table)

#save comparison table as csv for results folder
write.csv(comparison_table, file = paste0("results", "/fleet_size_comparison_table.csv"), 
  row.names = FALSE)

#generate plot of happiness rate vs fleet size
happiness_plot <- ggplot(comparison_table, aes(x = fleet_size, y = happiness_rate)) +
  geom_point(size = 4, color = "steelblue") +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_text(aes(label = paste0(round(happiness_rate * 100, 1), "%")), 
            vjust = -1.5, color = "black") +
  labs(
    title = "System Performance vs. Fleet Size (Prop. Placement)",
    x = "Total Fleet Size",
    y = "System Happiness Rate (Demand Met)"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal(base_size = 14)

# Save the plot as a PNG file
ggsave(filename = "fleet_size_happiness_plot.png", 
  plot = happiness_plot, 
  path = "results", 
  width = 8, 
  height = 5, 
  units = "in")


