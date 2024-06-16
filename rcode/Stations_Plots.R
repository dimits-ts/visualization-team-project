rm(list = ls())
getwd()

# Change to your Directory
setwd("C:\\Users\\maria\\OneDrive\\Υπολογιστής\\Visualization Project\\Data")

library(dplyr)

# Read the CSV file
tripdata <- read.csv("tripdata.csv")


################################################################################

#Clean the data

# Remove rows with missing values in start_station_name or end_station_name
tripdata_clean <- tripdata %>%
  na.omit(select = c(start_station_name, end_station_name))

# Count number of unique start stations
num_start_stations <- tripdata_clean %>%
  distinct(start_station_name) %>%
  n_distinct()

# Count number of unique end stations
num_end_stations <- tripdata_clean %>%
  distinct(end_station_name) %>%
  n_distinct()

# Print the results
cat("Number of unique start stations:", num_start_stations, "\n")
cat("Number of unique end stations:", num_end_stations, "\n")

##################     Plots    ############################################

library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid) 
library(ggtext)


# Top 10 most popular Start stations
top_start_stations <- tripdata_clean %>%
  group_by(start_station_name) %>%
  summarise(trips = n()) %>%
  arrange(desc(trips)) %>%
  head(10) %>%
  rename(station_name = start_station_name)

# Top 10 most popular End stations
top_end_stations <- tripdata_clean %>%
  group_by(end_station_name) %>%
  summarise(trips = n()) %>%
  arrange(desc(trips)) %>%
  head(10) %>%
  rename(station_name = end_station_name)

# Create the plot for top start stations
plot_start <- ggplot(top_start_stations, aes(x = reorder(station_name, -trips), y = trips)) +
  geom_bar(stat = "identity", fill = "blue",  width = 0.7) +
  coord_flip() +
  labs(title = "", x = "Start Stations", y = "Number of Trips") +
  theme_minimal()

# Create the plot for top end stations
plot_end <- ggplot(top_end_stations, aes(x = reorder(station_name, -trips), y = trips)) +
  geom_bar(stat = "identity", fill = "green",  width = 0.7) +
  coord_flip() +
  labs(title = "", x = "End Stations", y = "Number of Trips") +
  theme_minimal()

# Combine the two plots side by side
combined_plot <- grid.arrange(plot_start, plot_end, ncol = 2)

# Add a title to the combined plot at the top left
title <- textGrob("Top 10 Most Popular Start and End Stations \n May-June-July-Aug 2020, 2021, 2022, 2023",
                  gp = gpar(fontsize = 15), 
                  x = unit(0, "npc") + unit(1, "mm"), 
                  hjust = 0)

# Arrange the title and the combined plot
final_plot <- grid.arrange(combined_plot, top = title)

############################################################################

#Most popular start station for each month by year

# Group by year and month, find the most frequent start station
top_monthly_stations <- tripdata_clean %>%
  group_by(year, month, start_station_name) %>%
  summarise(trips = n()) %>%
  arrange(year, month, desc(trips)) %>%
  slice(1) %>%
  ungroup()

# Convert year and month to factor for correct ordering in plots
top_monthly_stations$year <- factor(top_monthly_stations$year)
top_monthly_stations$month <- factor(top_monthly_stations$month, levels = month.name)

# Plot the data
ggplot(top_monthly_stations, aes(x = month, y = trips, fill = start_station_name)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ year, scales = "free_x", nrow = 1) +
  labs(title = "Most Popular Start Station Each Month by Year",
       x = "", y = "Number of Trips", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        legend.position = "bottom")



####################################################################

#Most popular end station for each month by year

# Group by year and month, find the most frequent end station
top_monthly_end_stations <- tripdata_clean %>%
  group_by(year, month, end_station_name) %>%
  summarise(trips = n()) %>%
  arrange(year, month, desc(trips)) %>%
  slice(1) %>%
  ungroup()

# Convert year and month to factor for correct ordering in plots
top_monthly_end_stations$year <- factor(top_monthly_end_stations$year)
top_monthly_end_stations$month <- factor(top_monthly_end_stations$month, levels = month.name)

# Plot the data
ggplot(top_monthly_end_stations, aes(x = month, y = trips, fill = end_station_name)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ year, scales = "free_x", nrow = 1) +
  labs(title = "Most Popular End Station Each Month by Year",
       x = "", y = "Number of Trips", fill = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(size = 10),
        legend.position = "bottom")

############################ about tableau dataset  #######################

# Read the CSV file
tripdata <- read.csv("tripdata.csv")

#Clean the data
# Remove rows with missing values in start_station_name 
tripdata_new <- tripdata %>%
  na.omit(select = c(start_station_name))


# Calculate the frequency of each start station
station_freq <- tripdata_new %>%
  group_by(start_station_name) %>%
  summarise(
    frequency = n(),  # Number of rides starting at this station
    start_lat = first(start_lat),  # Latitude of the first occurrence
    start_lng = first(start_lng)   # Longitude of the first occurrence
  )

# Write to CSV
write.csv(station_freq, "stations_freq.csv", row.names = FALSE)










