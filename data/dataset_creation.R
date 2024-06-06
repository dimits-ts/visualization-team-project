rm(list = ls())
getwd()
# Change to your Directory
setwd("/home/dimits/Documents/university/masters/visualization/project/data")

library(dplyr)

unzip("/202005-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202006-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202007-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202008-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202105-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202106-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202107-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202108-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202205-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202206-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202207-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202208-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202305-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202306-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202307-capitalbikeshare-tripdata.zip", exdir = "/")
unzip("/202308-capitalbikeshare-tripdata.zip", exdir = "/")

may2020 <- read.csv("/202005-capitalbikeshare-tripdata.csv")
jun2020 <- read.csv("/202006-capitalbikeshare-tripdata.csv")
jul2020 <- read.csv("/202007-capitalbikeshare-tripdata.csv")
aug2020 <- read.csv("/202008-capitalbikeshare-tripdata.csv")
may2021 <- read.csv("/202105-capitalbikeshare-tripdata.csv")
jun2021 <- read.csv("/202106-capitalbikeshare-tripdata.csv")
jul2021 <- read.csv("/202107-capitalbikeshare-tripdata.csv")
aug2021 <- read.csv("/202108-capitalbikeshare-tripdata.csv")
may2022 <- read.csv("/202205-capitalbikeshare-tripdata.csv")
jun2022 <- read.csv("/202206-capitalbikeshare-tripdata.csv")
jul2022 <- read.csv("/202207-capitalbikeshare-tripdata.csv")
aug2022 <- read.csv("/202208-capitalbikeshare-tripdata.csv")
may2023 <- read.csv("/202305-capitalbikeshare-tripdata.csv")
jun2023 <- read.csv("/202306-capitalbikeshare-tripdata.csv")
jul2023 <- read.csv("/202307-capitalbikeshare-tripdata.csv")
aug2023 <- read.csv("/202308-capitalbikeshare-tripdata.csv")

View(aug2023)


may2020 <- may2020 %>% mutate(month = "May", year = 2020)
jun2020 <- jun2020 %>% mutate(month = "June", year = 2020)
jul2020 <- jul2020 %>% mutate(month = "July", year = 2020)
aug2020 <- aug2020 %>% mutate(month = "August", year = 2020)

may2021 <- may2021 %>% mutate(month = "May", year = 2021)
jun2021 <- jun2021 %>% mutate(month = "June", year = 2021)
jul2021 <- jul2021 %>% mutate(month = "July", year = 2021)
aug2021 <- aug2021 %>% mutate(month = "August", year = 2021)

may2022 <- may2022 %>% mutate(month = "May", year = 2022)
jun2022 <- jun2022 %>% mutate(month = "June", year = 2022)
jul2022 <- jul2022 %>% mutate(month = "July", year = 2022)
aug2022 <- aug2022 %>% mutate(month = "August", year = 2022)

may2023 <- may2023 %>% mutate(month = "May", year = 2023)
jun2023 <- jun2023 %>% mutate(month = "June", year = 2023)
jul2023 <- jul2023 %>% mutate(month = "July", year = 2023)
aug2023 <- aug2023 %>% mutate(month = "August", year = 2023)

# Concatenate the data frames
tripdata <- bind_rows(may2020, jun2020, jul2020, aug2020,
                      may2021, jun2021, jul2021, aug2021,
                      may2022, jun2022, jul2022, aug2022,
                      may2023, jun2023, jul2023, aug2023)

# Display the combined data frame
View(tripdata)

write.csv(tripdata, "tripdata.csv", row.names=FALSE)