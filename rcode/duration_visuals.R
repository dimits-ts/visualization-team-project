rm(list = ls())
getwd()
# Change to your Directory
setwd("C:/Users/vassi/OneDrive/Documents_OneDrive/MSc in Data Science/Spring Semester/Data visualization and communication/Assignments/Project2")

library(ggplot2)
library(maps)
library(dplyr)
library(mapview) 
library(sf)
library(lubridate)

tripdata <- read.csv("tripdata.csv")
View(tripdata)

# ------------ MAP PLOT OF THE STATIONS ------------

# Assuming df is your dataframe
stations_loc <- tripdata %>%
  group_by(start_station_name) %>%
  summarise(longitude = first(start_lng),
            latitude = first(start_lat))

stations_loc <- na.omit(stations_loc)
View(stations_loc)

points_sdf = st_as_sf(stations_loc,  
                      coords = c("longitude", "latitude"), crs = 4326) 

mapview(points_sdf,zcol = NULL, legend = FALSE, popup = NULL)

# --------------------------------------------------

tripdata <- tripdata %>%
  mutate(started_at = ymd_hms(started_at),
         ended_at = ymd_hms(ended_at))

# Calculate duration and create a new column
tripdata <- tripdata %>%
  mutate(duration = as.duration(ended_at - started_at))

# Print the data frame
View(tripdata)

