rm(list = ls())

library(ggplot2)
library(dplyr)
library(lubridate)

tripdata <- read.csv("../data/tripdata.csv")
names(tripdata)
View(tripdata)

# ------- Distribution of Ride Durations -------

ggplot(tripdata, aes(x = duration)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +  # Display frequency labels as regular numbers
  labs(x = "Ride Duration (minutes)", y = "Frequency", title = "Distribution of Ride Durations") +
  xlim(0,2500) +
  theme_minimal()+
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("../output/ride_duration_distribution.png")
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
  labs(x = "Month", y = "Median Ride Duration", title = "Average Ride Duration Over Time") +
  scale_x_discrete(labels = c("May","June","July","August")) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("../output/ride_duration_per_year_month.png")

# Calculate average ride duration per hour
avg_duration_hour <- aggregate(duration ~ hour+month, data = tripdata, FUN = median)

# Plot 1: Average Ride Duration Over Time
ggplot(avg_duration_hour, aes(x = hour, y = duration, group = month, color = month)) +
  geom_line(size=1.5) +
  labs(x = "Hour of the Day", y = "Median Ride Duration", title = "Median Ride Duration") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("../output/ride_duration_per_hour_month.png")

# ------- Duration by Rideable Type -------

tripdata_clean <- tripdata %>%
  filter(!is.na(year) & !is.na(duration))

ggplot(tripdata_clean, aes(x = year, y = duration)) +
  geom_boxplot(fill = "lightblue") +
  ylim(0, 2500) +
  labs(title = "Ride Duration by Rideable Type and Year",
       x = "Year",
       y = "Duration (seconds)") +
  facet_wrap(~ rideable_type) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("../output/ride_duration_per_biketype_year.png")

# ------- Duration by Member Type -------

ggplot(tripdata, aes(x = duration, colour = member_casual)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlim(0,2500) +
  geom_density(size=1.5) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave("../output/ride_duration_per_membertype.png")

