library(dplyr)

trip_df = read.csv("../data/tripdata.csv")

# adjust column types
str(trip_df)

trip_df$started_at <- as.POSIXct(as.character(trip_df$started_at), format="%Y-%m-%d %H:%M:%S")
trip_df$ended_at <- as.POSIXct(as.character(trip_df$ended_at), format="%Y-%m-%d %H:%M:%S")
trip_df <- trip_df %>%
  mutate(ride_duration = ended_at - started_at)
