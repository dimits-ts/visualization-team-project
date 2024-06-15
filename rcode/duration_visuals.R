rm(list = ls())
library(here)

setwd(here::here())
getwd()

library(ggplot2)
library(dplyr)
library(lubridate)

tripdata <- read.csv("data/tripdata.csv")

tripdata <- tripdata %>%
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at)
  )
tripdata <- tripdata %>%
  mutate(duration = as.numeric(difftime(ended_at, started_at, units = "secs")))
View(tripdata)

# ------- Distribution of Ride Durations -------

ggplot(tripdata, aes(x = duration)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +  # Display frequency labels as regular numbers
  labs(x = "Ride Duration (seconds)", y = "Frequency", title = "Distribution of Ride Durations",subtitle = "Distribution of Bike Ride Durations: Majority Between 5-20 Minutes") +
  xlim(0,2500) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("output/ride_duration_distribution.png")
# ------- Ride Duration over time -------

tripdata$started_at <- as.POSIXct(tripdata$started_at, format = "%Y-%m-%d %H:%M:%S")
tripdata$month <- format(tripdata$started_at, "%m")
tripdata$year <- format(tripdata$started_at, "%Y")
tripdata$hour <- format(tripdata$started_at, "%H")

# Calculate average ride duration per month and year
avg_duration_month_year <- aggregate(duration ~ month + year, data = tripdata, FUN = median)

# Plot 1: Average Ride Duration Over Time
ggplot(avg_duration_month_year, aes(x = month, y = duration, group = year, color = year)) +
  geom_line(size=1.5) +
  labs(x = "Month", y = "Median Ride Duration",color="Year", title = "Average Ride Duration Over Time",subtitle = "Decreasing Trend in Ride Durations: Yearly Decline and Seasonal Dip from May to August") +
  scale_x_discrete(labels = c("May","June","July","August")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("output/ride_duration_per_year_month.png")

# Calculate average ride duration per hour
avg_duration_hour <- aggregate(duration ~ hour+month, data = tripdata, FUN = median)

# Plot 1: Average Ride Duration Over Time
ggplot(avg_duration_hour, aes(x = hour, y = duration, group = month, color = month)) +
  geom_line(size=1.5) +
  labs(x = "Hour of the Day", y = "Median Ride Duration",color="Month", title = "Median Ride Duration",subtitle = "Ride Duration Patterns: Decrease from 4-8 AM and Increase from 11 AM to 5 PM") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("output/ride_duration_per_hour_month.png")

# ------- Duration by Rideable Type -------

tripdata_clean <- tripdata %>%
  filter(!is.na(year) & !is.na(duration))

rideable_colors <- c("electric_bike" = "darkblue",    # blue
                     "docked_bike" = "lightblue",      # green
                     "classic_bike" = "darkgreen")  # red

ggplot(tripdata_clean, aes(x = year, y = duration, fill = rideable_type)) +
  geom_boxplot() +
  ylim(0, 2500) +
  labs(title = "Ride Duration by Rideable Type and Year",
       x = "Year",
       y = "Duration (seconds)",color="Rideable Type",subtitle = "Decrease in Ride Durations for all types except Docked") +
  facet_wrap(~ rideable_type) +
  scale_fill_manual(values = rideable_colors) +  # Set custom fill colors
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("output/ride_duration_per_biketype_year.png")

# ------- Duration by Member Type -------

color_palette <- c("member" = "blue",  # blue
                   "casual" = "orange" # red
)

# Plot with custom colors, title, and subtitle
ggplot(tripdata, aes(x = duration, colour = member_casual)) +
  geom_density(size = 1.5) +
  scale_color_manual(values = color_palette) +  # Set custom line colors
  xlim(0, 2500) +
  labs(
    title = "Ride Duration Density by Member Type",
    subtitle = "Higher Density for Members on low Duration Rides, Opposite for Casuals",
    x = "Duration",
    y = "Density", color="Member Type"
  ) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),axis.text.y = element_blank())
ggsave("output/ride_duration_per_membertype.png")

