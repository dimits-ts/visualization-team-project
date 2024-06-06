library(dplyr)
library(psych)

# ======== PREPROCESSING ========

trip_df = read.csv("../data/tripdata.csv")

# Add time delta
str(trip_df)

trip_df$started_at <- as.POSIXct(as.character(trip_df$started_at), format="%Y-%m-%d %H:%M:%S")
trip_df$ended_at <- as.POSIXct(as.character(trip_df$ended_at), format="%Y-%m-%d %H:%M:%S")
trip_df <- trip_df %>%
  mutate(ride_duration = ended_at - started_at)
# some ride seems to have end dates preceding start dates. We remove these false datapoints.
trip_df <- trip_df[trip_df$ride_duration > 0, ]

# Add pricing

# Create a function to calculate the price for each trip
calculate_price <- function(rideable_type, member_casual, ride_duration) {
  duration_minutes <- as.numeric(ride_duration, units = "mins")
  price <- 0
  
  if (member_casual == "casual") {
    if (rideable_type == "classic_bike") {
      price <- 1 + 0.05 * duration_minutes
    } else if (rideable_type == "electric_bike") {
      price <- 1 + 0.15 * duration_minutes
    }
  } else if (member_casual == "member") {
    if (rideable_type == "classic_bike") {
      if (duration_minutes <= 45) {
        price <- 0
      } else {
        price <- 0.05 * (duration_minutes - 45)
      }
    } else if (rideable_type == "electric_bike") {
      price <- 0.10 * duration_minutes
    }
  }
  
  return(price)
}

trip_df <- trip_df %>%
  mutate(price = mapply(calculate_price, rideable_type, member_casual, ride_duration))

head(trip_df)
describe(trip_df$price)


