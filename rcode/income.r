library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(scales)
library(gtable)

OUTPUT_PATH = "output"

MEMBER_COLOR <- "blue"
CASUAL_COLOR <- "orange"
ALL_COLOR <- "grey"

# Utility function to get a relative file path (including file extension)
# from a file name.
filepath_png <- function(name) {
  return(file.path(OUTPUT_PATH, paste(name, ".png", sep = "")))
}

# ======== PREPROCESSING ========

trip_df <- read.csv("./data/tripdata.csv")
trip_df <- trip_df[, c("month", "year", "rideable_type", "member_casual", "started_at", "ended_at")]
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


# ======== PLOTS ========


income_summary <- trip_df %>%
  mutate(year_month = format(started_at, "%Y-%m")) %>%
  group_by(year_month, member_casual) %>%
  summarise(total_income = sum(price)) %>%
  ungroup()

total_income_all <- income_summary %>%
  group_by(year_month) %>%
  summarise(total_income = sum(total_income)) %>%
  mutate(member_casual = "All")

income_summary <- bind_rows(income_summary, total_income_all)

#TODO: I cant make the x axis work with anything, I have been trying for over an hour at this point
ggplot(income_summary, aes(x = year_month, 
                           y = total_income / 1e6, 
                           color = member_casual, 
                           group = member_casual, 
                           fill = member_casual)) +
  geom_ribbon(aes(ymin = 0, ymax = total_income / 1e6), alpha = 0.2) +
  geom_line() +
  labs(title = "Income by Member Type and Month",
       x = "Month",
       y = "Income (Millions)") +
  scale_color_manual(values = c("member" = MEMBER_COLOR, 
                                "All" = ALL_COLOR, 
                                "casual" = CASUAL_COLOR), guide=FALSE) +
  scale_fill_manual(values = c("member" = MEMBER_COLOR, 
                               "All" = ALL_COLOR, 
                               "casual" = CASUAL_COLOR),
                    guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  #scale_x_date(date_labels = "%b-%Y")
  
ggsave(filename=filepath_png("income_by_member_month"))
