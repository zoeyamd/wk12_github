#load input data
#replace 'path/to/your/data.csv' and 'read.csv' with your actual data loading command.
bike_data <- read_csv("~/Downloads/1560/Data/sample_bike.csv")

#source work from run analysis file, which has all functions and fleet tests
source("/Users/zoeybug/Documents/GitHub/wk12_github/scripts/run_analysis.R")


#generate plot of demand distribution from estimation data
#aggregate data by hour
aggregated_estimated_arrivals <- complete_estimated_arrivals %>%
  group_by(hour) %>%
  summarize(total_est_rate = sum(mu_hat, na.rm = TRUE), .groups = 'drop')

#plot
demand_plot <- ggplot(aggregated_estimated_arrivals, aes(x=hour, y=total_est_rate)) +
  #trend of demand
  geom_line(color="steelblue") +
  #hourly points
  geom_point(color="steelblue") +
  #define rush points
  geom_text(data = aggregated_estimated_arrivals %>% 
              filter(hour %in% c(9, 16)), #highlight 9 AM and 4 PM
            aes(label = round(total_est_rate, 0)),
            vjust = -1.5,
            color = "#dc3912",
            fontface = "bold") +
  labs(
    title = "System-Wide Estimated Arrival Rate by Hour",
    subtitle = "This represents the statistically estimated unconstrained demand",
    x = "Hour of Day (0 = 12 AM)",
    y = "Total Estimated Arrival Rate (Trips/Hour)") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theme_minimal()

#generate plot of happiness rate vs fleet size
#change to vector for geom point
comparison_table$fleet_size <- as.vector(comparison_table$fleet_size)

#plot
happiness_plot <- ggplot(comparison_table, aes(x = fleet_size, y = happiness_rate)) +
  geom_point(size = 4, color = "pink") +
  geom_line(color="pink") +
  labs(
    title = "System Performance versus Fleet Size with Proportional Initial Placement",
    x = "Total Fleet Size",
    y = "System Performance (Happiness Rate)") +
  theme_minimal()


#generate plot of inventory imbalance (start vs end) for fleet size 50
#pulling inventory data from optimization function
fleet_2_results <- optimization_prop(complete_estimated_arrivals, 200, 42)
inventory_comparison <- bind_rows(
  #process initial inventory
  fleet_2_results$initial_inventory %>%
    #rename to match final inventory
    rename(bike_count = initial_count) %>% 
    mutate(inventory_type = "Starting Inventory"),
  
  #process final inventory
  fleet_2_results$final_inventory %>%
    mutate(inventory_type = "Ending Inventory")) %>%
  
  #ensure ordering is correct for the plot
  mutate(inventory_type = factor(inventory_type,
                                 levels = c("Starting Inventory",
                                            "Ending Inventory")),
         station = factor(station),
         station = reorder(station, if_else(inventory_type == "Ending Inventory", 
                                            bike_count, NA_real_))) 
#plot
inventory_imbalance_plot <- 
  ggplot(inventory_comparison, aes(x= station, y= bike_count, fill=inventory_type,
                                 position="dodge")) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ inventory_type, scales = "free_x") +
  labs(
    title = "Bike Inventory Imbalance",
    subtitle = "Visualizing the spatial impact of one day's demand (Fleet Size 200)",
    x = "Station ID (Ordered by Final Bike Count)",
    y = "Number of Bikes")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.title.position = "plot")

#save plots to results folder
#demand plot
ggsave(filename = "demand_distribution_plot.png",
       plot = demand_plot, 
       path = "results", 
       width = 10, 
       height = 6, 
       units = "in")
#happiness plot
ggsave(filename = "fleet_size_happiness_plot.png", 
       plot = happiness_plot, 
       path = "results", 
       width = 8, 
       height = 5, 
       units = "in")

#inventory imbalance plot
ggsave(filename = "inventory_imbalance_plot.png", 
  plot = inventory_imbalance_plot, 
  path = "results", 
  width = 12,        
  height = 6, 
  units = "in")
