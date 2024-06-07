library(dplyr)
library(psych)
library(ggplot2)
library(lubridate)
library(scales)
library(gtable)
library(gridExtra)

OUTPUT_PATH = "output"

MEMBER_COLOR <- "blue"
CASUAL_COLOR <- "orange"
ALL_MEMBERS_COLOR <- "grey"

CLASSIC_BIKE_COLOR = "darkgreen"
ELECTRIC_BIKE_COLOR = "darkblue"

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

# what the fuck is a "docked_bike"
# Im assuming docked bikes are just classic bikes
# TODO: investigate
levels(trip_df$rideable_type)
trip_df[trip_df$rideable_type == "docked_bike", "rideable_type"] <- "classic_bike"
sum(trip_df$rideable_type=="docked_bike")

# Add pricing

# Create a function to calculate the price for each trip
calculate_price <- function(rideable_type, member_casual, ride_duration) {
  duration_minutes <- as.numeric(ride_duration, units = "mins")
  price <- 0
  
  if (member_casual == "casual") {
    # include the upfront 1$ cost for unlocking the bike
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

# statistics and exploratory plots
head(trip_df)
describe(trip_df$price)
ggplot(trip_df, aes(x = price)) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trip Prices",
       x = "Price",
       y = "Frequency") +
  theme_minimal()

describe(as.numeric(trip_df$ride_duration, units="mins"))
ggplot(trip_df, aes(x = as.numeric(ride_duration, units = "mins"))) +
  geom_density(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Trip Duration",
       x = "Duration (minutes)",
       y = "Frequency") +
  theme_minimal()

# ======== PLOTS ========

# Sum income plot
income_summary <- trip_df %>%
  mutate(year_month = format(started_at, "%Y-%m")) %>%
  group_by(year_month, member_casual) %>%
  summarise(total_income = sum(price), .groups = 'drop')

total_income_all <- income_summary %>%
  group_by(year_month) %>%
  summarise(total_income = sum(total_income)) %>%
  mutate(member_casual = "All")

income_summary <- bind_rows(income_summary, total_income_all)

plot1 <- ggplot(income_summary, aes(x = year_month, 
                                    y = total_income / 1e6, 
                                    color = member_casual, 
                                    group = member_casual, 
                                    fill = member_casual)) +
  geom_ribbon(aes(ymin = 0, ymax = total_income / 1e6), alpha = 0.2) +
  geom_line() +
  labs(title="Membership revenues increasingly more important each summer",
       subtitle = "Summer revenue by member type",
       x = "Month",
       y = "Income ($ Millions)",
       fill="Member Type") +
  scale_color_manual(values = c("member" = MEMBER_COLOR, 
                                "All" = ALL_MEMBERS_COLOR, 
                                "casual" = CASUAL_COLOR), guide=FALSE) +
  scale_fill_manual(values = c("member" = MEMBER_COLOR, 
                               "All" = ALL_MEMBERS_COLOR, 
                               "casual" = CASUAL_COLOR),
                    guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Percentage income plot
trip_df$month_number <- match(trip_df$month, month.name)

income_by_member <- trip_df %>%
  group_by(year, month_number, member_casual) %>%
  summarise(total_income = sum(price), .groups = 'drop') %>%
  group_by(year, month_number) %>%
  mutate(percentage_income = (total_income / sum(total_income)) * 100) %>%
  mutate(year_month = factor(paste(year, month_number, sep = "-"), 
                             levels = unique(paste(year, month_number, sep = "-"))))

plot2 <- ggplot(income_by_member, aes(x = year_month, y = percentage_income, fill = member_casual)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  labs(title="", # correctly offset title
       subtitle = "Revenue share by member type",
       x = "Month",
       y = "Revenue (%)", 
       fill="Member Type") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("member" = MEMBER_COLOR, 
                               "casual" = CASUAL_COLOR),
                    guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Combine plots
combined_plot <- grid.arrange(plot1, plot2, ncol = 2)
ggsave(filename=filepath_png("income_member"), plot = combined_plot)


# Bike type plot

revenue_by_month_year <- trip_df %>%
  mutate(year_month = format(started_at, "%Y-%m")) %>%
  group_by(year_month, rideable_type) %>%
  summarize(total_revenue = sum(price))

ggplot(revenue_by_month_year, aes(x = year_month, 
                                  y = total_revenue/10e6, 
                                  color = rideable_type, 
                                  fill = rideable_type, 
                                  group = rideable_type)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = total_revenue/10e6), alpha = 0.2) +
  labs(title="Electric bikes increasingly profitable each summer",
       subtitle="Revenue by bike type", 
       x = "Date", 
       y = "Total Revenue ($ Millions)",
       fill = "Rideable Type") +
  scale_color_manual(values = c("classic_bike" = CLASSIC_BIKE_COLOR,
                                "electric_bike" = ELECTRIC_BIKE_COLOR), guide=FALSE) +
  scale_fill_manual(values = c("classic_bike" = CLASSIC_BIKE_COLOR,
                               "electric_bike" = ELECTRIC_BIKE_COLOR), 
                    guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename=filepath_png("income_bike_type"))

# ================ A ===============


income_by_rideable <- trip_df %>%
  group_by(year, month_number, rideable_type) %>%
  summarise(total_income = sum(price), .groups = 'drop') %>%
  group_by(year, month_number) %>%
  mutate(percentage_income = (total_income / sum(total_income)) * 100) %>%
  mutate(year_month = factor(paste(year, month_number, sep = "-"), 
                             levels = unique(paste(year, month_number, sep = "-"))))

ggplot(income_by_rideable, aes(x = year_month, y = percentage_income, fill = rideable_type)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
  labs(title="", # correctly offset title
       subtitle = "Revenue share by member type",
       x = "Month",
       y = "Revenue (%)", 
       fill="Member Type") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = c("classic_bike" = CLASSIC_BIKE_COLOR, 
                               "electric_bike" = ELECTRIC_BIKE_COLOR),
                    guide = guide_legend(override.aes = list(shape = 16, size = 4))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
