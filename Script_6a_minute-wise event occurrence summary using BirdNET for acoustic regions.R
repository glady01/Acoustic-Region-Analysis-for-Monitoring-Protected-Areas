#-------------------------------------------------------------------------------
# Title: Minute-wise Event Occurrence Summary using BirdNET for Acoustic Regions
#-------------------------------------------------------------------------------
# Author: [Name]
# Date: [Current Date]
# Description: This script performs an analysis of minute-wise event detections based on BirdNET data for different acoustic regions.
#The data is grouped by various factors such as scientific name, site, season, date, and minute.
#The script identifies exclusive and common species for different sites and calculates the total incidence
#for each group based on different confidence levels.
#-------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(lubridate)
library(openxlsx)

# Set the working directory
setwd("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output2")

# Get a list of all CSV files in the working directory that match the specified pattern
input_files <- list.files(pattern = "^(Mid-high-frequency|Mid-low-frequency|Low-frequency|High-frequency)_(Pre-Dawn|Dawn|Day|Dusk|Night)_(Olakara|Munipara)_(Olakara|Munipara)_(May|March)_BirdNET\\.csv$")

# Loop over all input files
for (input_file in input_files) {
  # Derive the region name, time, sites, and month from the input file name
  input_file_parts <- strsplit(input_file, "_")[[1]]
  acoustic_region <- input_file_parts[1]
  time <- input_file_parts[2]
  site1 <- input_file_parts[3]
  site2 <- input_file_parts[4]
  month <- input_file_parts[5]
  
  # Read in the data
  data <- read.csv(input_file)
  
  # Add the acoustic_region to the data
  data$acoustic_region <- acoustic_region
  
  # Format the scientific name and common name
  data$Scientific.name <- gsub("\\.", " ", data$Scientific.name)
  data$Common.name <- gsub("\\.", " ", data$Common.name)
  
  # Format the date_time column and ensure hours come before minutes
  data$Date_Time <- strptime(data$Date_Time, "%Y%m%d_%H%M%S")  # Ensure correct format conversion
  
  # Extract the minute portion from Date_Time in "%H:%M:%S" format
  data <- data %>%
    mutate(Minute = format(Date_Time, "%H:%M:%S"))
  
  # Extract Date and Time separately
  data <- data %>%
    mutate(Date = date(Date_Time),
           Time = format(Date_Time, "%H:%M:%S"))
  
  # Filter data based on Confidence
  filtered_data <- data %>%
    filter(Confidence >= 0.7)
  
  # Group by species, Site, Date, and Minute
  grouped_data <- filtered_data %>%
    group_by(Scientific.name, Site, Season, Date, Minute) %>%
    summarize(total_incidence = n())
  
  # Print the summarized data
  print(grouped_data)
  
  # Filter data based on Confidence and calculate total_incidence for each
  filtered_data_07 <- data %>%
    filter(Confidence >= 0.7) %>%
    mutate(Minute = format(Date_Time, "%H:%M:%S")) %>%
    mutate(Date = date(Date_Time),
           Time = format(Date_Time, "%H:%M:%S")) %>%
    group_by(acoustic_region, Scientific.name, Common.name, Site, Season, Date, Minute) %>%
    summarize(total_incidence_07 = n())
  
  filtered_data_03 <- data %>%
    filter(Confidence >= 0.3) %>%
    mutate(Minute = format(Date_Time, "%H:%M:%S")) %>%
    mutate(Date = date(Date_Time),
           Time = format(Date_Time, "%H:%M:%S")) %>%
    group_by(acoustic_region, Scientific.name, Common.name, Site, Season, Date, Minute) %>%
    summarize(total_incidence_03 = n())
  
  filtered_data_01 <- data %>%
    filter(Confidence >= 0.1) %>%
    mutate(Minute = format(Date_Time, "%H:%M:%S")) %>%
    mutate(Date = date(Date_Time),
           Time = format(Date_Time, "%H:%M:%S")) %>%
    group_by(acoustic_region, Scientific.name, Common.name, Site, Season, Date, Minute) %>%
    summarize(total_incidence_01 = n())
  
  # Merge the data frames
  merged_data <- full_join(filtered_data_07, filtered_data_03, by = c("acoustic_region", "Scientific.name", "Common.name", "Site", "Season", "Date", "Minute"))
  merged_data <- full_join(merged_data, filtered_data_01, by = c("acoustic_region", "Scientific.name", "Common.name", "Site", "Season", "Date", "Minute"))
  
  # Find exclusive and common species for both sites
  munipara_data <- data %>%
    filter(Site == "Munipara")
  olakara_data <- data %>%
    filter(Site == "Olakara")
  
  munipara_exclusive_species <- setdiff(munipara_data$Scientific.name, olakara_data$Scientific.name)
  olakara_exclusive_species <- setdiff(olakara_data$Scientific.name, munipara_data$Scientific.name)
  common_species <- intersect(munipara_data$Scientific.name, olakara_data$Scientific.name)
  
  # Add Status column to the merged data
  merged_data <- merged_data %>%
    mutate(Status = case_when(
      Scientific.name %in% munipara_exclusive_species ~ "Munipara Exclusive",
      Scientific.name %in% olakara_exclusive_species ~ "Olakara Exclusive",
      Scientific.name %in% common_species ~ "Common",
      TRUE ~ "Other"
    ))
  
  # Add a new column that calculates the highest confidence for each species for both sites
  max_confidence_data <- data %>%
    group_by(acoustic_region, Scientific.name, Common.name, Site, Season, Date, Minute) %>%
    summarize(
      max_confidence = max(Confidence),
      .groups = "drop"
    )
  
  # Merge max_confidence_data with merged_data
  merged_data <- left_join(merged_data, max_confidence_data, by = c("acoustic_region", "Scientific.name", "Common.name", "Site", "Season", "Date", "Minute"))
  
  # Convert column names to lowercase
  names(merged_data) <- tolower(names(merged_data))
  
  # Save the summary data to an Excel workbook
  output_file <- paste0("Minute_", acoustic_region,"_",time,"_",site1,"_",site2,"_",month,"_Events_SUMMARY.xlsx")
  write.xlsx(merged_data, output_file)
}