library(tidyverse)

#' Estimate arrival rates
#'
#' @description uses an unbiased estimator to estimate the arrival rate
#' of every start_station, end_station, hour trio in the data 
#' 
#' @param data data frame/tibble - contains bike ridership data
#' 
#' @return a tibble that contains the desired estimated arrival rates, along
#' with the intermediate involved in calculation
estimate_arrival_rates <- function(data) {
  # converts times to POSIXct format
  data <- data %>%
    mutate(start_time = as.POSIXct(start_time, format = "%Y-%m-%d %H:%M:%S"),
           end_time   = as.POSIXct(end_time,   format = "%Y-%m-%d %H:%M:%S"))
  
  # compute the average number of trips per hour between each pair
  x_hat <- data %>%
    mutate(hour = hour(start_time)) %>%
    filter(start_station != "R", end_station != "R") %>%
    group_by(start_station, end_station, hour) %>%
    summarise(avg_trips = n() / n_distinct(as_date(start_time)), 
              .groups = "drop") 
  
  # pivot longer to get change in count 
  data$end_station <- as.character(data$end_station)
  trips_long <- data %>%
    pivot_longer(cols = c("start_station", "start_time", 
                          "end_station", "end_time"),
                 names_to = c("type", ".value"),   
                 names_pattern = "(start|end)_(.*)") %>%
    mutate(change = ifelse(type == "start", -1, 1),
           hour = hour(time)) %>%
    select(station, time, hour, change)
  
  # add hour markers so we can get cumulative time
  dates <- unique(as_date(trips_long$time))
  hours <- c(seq(0,23,1), seq(0,23,1) + 0.9999999)
  stations <- unique(trips_long$station)
  hr_pts <- expand.grid(time = dates, hour = hours, 
                        station = stations) %>%
    mutate(time = as.POSIXct(time) + hour*60*60,
           hour = hour(time))
  hr_pts$change <- 0
  trips_long <- rbind(trips_long, hr_pts)
  
  # find average availability 
  alpha_hat <- trips_long %>%
    group_by(station) %>%
    filter(station != "R") %>%
    arrange(time) %>% 
    mutate(count = cumsum(change),
           date = as_date(time)) %>%
    group_by(station, hour, date) %>%
    summarize(time_avail = 
                sum(difftime(time, lag(time), units="hours")*(count > 0), 
                    na.rm = TRUE)) %>%
    summarize(avg_avail = mean(time_avail)) %>%
    mutate(avg_avail = round(as.numeric(avg_avail), digits = 4)) %>%
    ungroup()
  
  # join the data and compute arrival rates
  mu_hat <- x_hat %>%
    left_join(alpha_hat, by = c("start_station" = "station", "hour")) %>%
    mutate(mu_hat = ifelse(avg_avail > 0, avg_trips / avg_avail, NA))
  
  return(mu_hat)
}

#' Finds the max hourly arrival rate between stations
#'
#' @description for every start, end station pair in the data, determines the 
#' maximum estimated hourly arrival rate
#' 
#' @param arrival_rates tibble - contains estimated arrival rates
#' 
#' @return a tibble with the max hourly estimated arrival rate for every 
#' start, end station pair present in the data
find_lambda_max <- function(arrival_rates) {
  lambda_maxes <- arrival_rates %>%
    group_by(start_station, end_station) %>%
    summarize(lambda_max = max(mu_hat))
  return(lambda_maxes)
}
