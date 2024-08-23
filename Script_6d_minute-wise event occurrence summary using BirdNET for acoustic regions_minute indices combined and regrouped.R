# Load required libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(stringr)

# Set file paths
csv_file <- "D:/Acoustic Region Analysis 2023/Results/minute_frequency_time_summaries_acoustic_regions.csv"
xlsx_file <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/MInute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_GROUPED_OCCURRENCE.xlsx"
output_file <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/MInute__INDICES_EVENTS_COMBINED_REGROUPED____ALL_REGIONS.csv"

# Read the files
csv_data <- read_csv(csv_file)
xlsx_data <- read_excel(xlsx_file)

# Rename the Acoustic_Region column in the xlsx file to acoustic_region
xlsx_data <- xlsx_data %>% rename(acoustic_region = Acoustic_Region)

# Convert the Date column in the xlsx file to a date type
xlsx_data <- xlsx_data %>% mutate(Date = as.Date(Date, format = "%d-%m-%Y"))

# Convert the time_period to a format that matches the Minute column in the xlsx file
csv_data <- csv_data %>% mutate(time_period = hms(time_period))

# Convert the Minute column in the xlsx file to a Period type
xlsx_data <- xlsx_data %>% mutate(Minute = hms(Minute))

# Rename the Acoustic_Region column in the xlsx file to acoustic_region
xlsx_data <- xlsx_data %>% rename(acoustic_region = Acoustic_Region)

# Convert the time_period to a format that matches the Minute column in the xlsx file
csv_data <- csv_data %>% mutate(time_period = hms(time_period))

# Merge the two data frames
merged_data <- inner_join(csv_data, xlsx_data, 
                          by = c("acoustic_region" = "acoustic_region", "season" = "Season", "site" = "Site", "date" = "Date", "time_period" = "Minute"))

# Select the desired columns
reshaped_data <- merged_data %>% 
  select(acoustic_region, season, site, date, Minute, index, Scientific.name, tot_occurrence, minute_sum, minute_average)

# Save resulting data frame to CSV file
write_csv(reshaped_data, output_file)

# Load the feather library
library(feather)

# Write the data frame to a .feather file
write_feather(reshaped_data, "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/MInute__INDICES_EVENTS_COMBINED_REGROUPED____ALL_REGIONS.feather")

# Read the .feather file into R
reshaped_data <- read_feather("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/MInute__INDICES_EVENTS_COMBINED_REGROUPED____ALL_REGIONS.feather")
