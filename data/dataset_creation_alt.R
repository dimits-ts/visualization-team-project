# Load necessary library
library(dplyr)

# this needs to be specified apparently
setwd("/home/dimits/Documents/university/masters/visualization/project/data/")
target_dir = "raw"

# Get and unzip all zip files in the current directory
zip_files <- list.files(pattern = "*.zip", full.names = T)

# if this throws warnings, it's because it is not overwriting already unzipped files
# aka ignore them
lapply(zip_files, function(zip_file) {
  unzip(zip_file, exdir = target_dir, overwrite=FALSE)
})

# Get all CSV files in the target directory
csv_files <- list.files(path = target_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csv_files) == 0) {
  stop("No CSV files found after unzipping.")
}

# Function to extract month and year from the file name
extract_month_year <- function(file_name) {
  # Extract the date part from the file name
  date_part <- gsub(".*(\\d{6})-capitalbikeshare-tripdata.csv", "\\1", file_name)
  year <- substr(date_part, 1, 4)
  month_num <- substr(date_part, 5, 6)
  month <- switch(month_num,
                  "01" = "January", "02" = "February", "03" = "March",
                  "04" = "April", "05" = "May", "06" = "June",
                  "07" = "July", "08" = "August", "09" = "September",
                  "10" = "October", "11" = "November", "12" = "December")
  return(list(month = month, year = as.numeric(year)))
}

# Read all CSV files and add month and year columns
tripdata_list <- lapply(csv_files, function(csv_file) {
  data <- read.csv(csv_file)
  month_year <- extract_month_year(csv_file)
  data <- data %>% mutate(month = month_year$month, year = month_year$year)
  return(data)
})


tripdata <- bind_rows(tripdata_list)
write.csv(tripdata, "tripdata.csv", row.names = FALSE)
