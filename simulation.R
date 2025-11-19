
simulate_day <- function(arrival_rates) {
  # we have our arrival data (assume it is complete over stations and hours)
  logged_trips <- as.data.frame(start_station = c(), end_station = c(), start_time = c(), end_time())
  for (start in unique(arrival_rates$start_station)) {
    for (end in unique(arrival_rates$end_station)) {
      accumulated_time = 0
      lambda_max = arrival_rates[arrival_rates$start_station == start & arrival_rates$end_station == end]$lambda_max[0]
      
      while (accumulated_time < 24):
        sampled_time = rexp(1, rate = lambda_max)
        accumulated_time = accumulated_time + sampled_time
        
        thinning_parameter <- 
      
    }
  }
}