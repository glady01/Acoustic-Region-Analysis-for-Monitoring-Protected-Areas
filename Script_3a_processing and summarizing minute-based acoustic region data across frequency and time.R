# ==============================================================================
# Title: Processing and Summarizing Minute-Based Acoustic Region Data Across Frequency and Time
# Author: [A.P. Zaibin]
# Date Created: 2024-02-03
# ==============================================================================

# Description:
# This script analyzes acoustic region data on a minute-by-minute basis, calculating row sums and averages
#across frequency and time for defined regions. It processes normalized daily data from CSV files containing
#separate indices for sites and seasons, generating summarized data with additional columns in output CSV and XLSX files.

# Input:
# - CSV files containing normalized daily acoustic data for each index, grouped by site and season

# Output:
# - CSV and XLSX files containing summarized data with additional columns, including minute-based time periods, 
#region ranges, row sums, and row averages.

#Duration: approximately 18hrs

# ==============================================================================

library(openxlsx)

# Set the working directory
setwd("D:/Acoustic Region Analysis 2023/Acoustic Region Data_Day-wise/Without_first_column_Daily Acoustic Region Data_Normalised")

# Get a list of all CSV files in the directory
files <- list.files(pattern = "\\.csv$")

# Modify the results data frame to include the new columns
results <- data.frame(site = character(),
                      season = character(),
                      index = character(),
                      file_name= character(),
                      date = character(),
                      region = numeric(),
                      region_range = character(),
                      region_renamed2 = character(),
                      row_range = character(),
                      col_range = character(),
                      minute_sum = numeric(),
                      minute_average = numeric(),
                      stringsAsFactors = FALSE)

# Create a sequence of 5-minute time periods
time_periods <- format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                           to = as.POSIXct("23:55:00", format = "%H:%M:%S"), 
                           by = "5 min"), 
                       format = "%H:%M:%S")

# Create a vector of time period ranges
time_period_ranges <- paste(time_periods, format(as.POSIXct(time_periods, format = "%H:%M:%S") + 4*60, format = "%H:%M:%S"), sep = " - ")


# Loop over the files and read them in
for (file in files) {
  # Print out debugging information
  cat("Processing file:", file, "\n")
  
  # Extract site information
  site <- gsub(".*(Munipara|Olakara).*", "\\1", file)
  
  # Extract season information
  season <- gsub(".*(March|May).*", "\\1", file)
  
  # Extract index information
  index <- gsub(".*_(ACI|PMN|CVR|OSC|ENT|EVN|RNG|RPS|RVT|SPT|RHZ|BGN).*", "\\1", file)
  
  # Extract file name information
  file_name <- gsub(".*_(\\d{4}-\\d{2}-\\d{2}).*\\.csv$", "\\1", file)
  
  # Extract day information
  date <- substr(unlist(strsplit(file, "_"))[1], 1, 10)
  
  # Read in the data
  data <- read.csv(file, header = TRUE)
  
  # Define the acoustic regions
  acoustic_regions_day1 <- data.frame(
    region = 1:20,
    row_start = c(2, 2, 2, 2, 68, 68, 68, 68, 110, 110, 110, 110, 212, 212, 212, 212, 254, 254, 254, 254),
    row_end = c(67, 67, 67, 67, 109, 109, 109, 109, 211, 211, 211, 211, 253, 253, 253, 253, 289, 289, 289, 289),
    col_start = c(1, 24, 85, 185, 1, 24, 85, 185, 1, 24, 85, 185, 1, 24, 85, 185, 1, 24, 85, 185),
    col_end = c(23, 84, 184, 256, 23, 84, 184, 256, 23, 84, 184, 256, 23, 84, 184, 256, 23, 84, 184, 256)
  )
  
  #Assigning region ranges to acoustic regions
  acoustic_regions_day1$region_range <- paste0("row ", formatC(acoustic_regions_day1$row_start, width = 2, flag = "0"), 
                                            "-", formatC(acoustic_regions_day1$row_end, width = 2, flag = "0"), 
                                            ", col ", formatC(acoustic_regions_day1$col_start, width = 2, flag = "0"),
                                            "-", formatC(acoustic_regions_day1$col_end, width = 2, flag = "0"))
  
  # Define the new acoustic region names
  acoustic_region <- c("Low-frequency_Pre-Dawn", "Mid-low-frequency_Pre-Dawn", "Mid-high-frequency_Pre-Dawn", "High-frequency_Pre-Dawn",
                       "Low-frequency_Dawn", "Mid-low-frequency_Dawn", "Mid-high-frequency_Dawn", "High-frequency_Dawn",
                       "Low-frequency_Day", "Mid-low-frequency_Day", "Mid-high-frequency_Day", "High-frequency_Day",
                       "Low-frequency_Dusk", "Mid-low-frequency_Dusk", "Mid-high-frequency_Dusk", "High-frequency_Dusk",
                       "Low-frequency_Night", "Mid-low-frequency_Night", "Mid-high-frequency_Night", "High-frequency_Night")
  
  # Add the new region names to the data frame
  acoustic_regions_day1$acoustic_region <- acoustic_region
  
  # Extract data for each region and calculate the row sum and row average
  for (i in 1:nrow(acoustic_regions_day1)) {
    # Extract the data for the current region
    region_data <- data[(acoustic_regions_day1$row_start[i] - 1):(acoustic_regions_day1$row_end[i] - 1), 
                        acoustic_regions_day1$col_start[i]:acoustic_regions_day1$col_end[i]]
    
    # Calculate the row sum and row average for each row (which now represents 1 minute)
    for (j in 1:nrow(region_data)) {
      sub_region_data <- unlist(region_data[j, ])  # Convert to numeric vector
      
      # Calculate row sum
      minute_sum <- sum(sub_region_data, na.rm = TRUE)
      
      # Calculate row average
      minute_average <- mean(sub_region_data, na.rm = TRUE)
      
      # Create a single-minute time period for the current row
      time_period <- time_periods[(acoustic_regions_day1$row_start[i] - 1) + j - 1]
      
      # Inside the loop, add the row and column ranges to the results
      results <- rbind(results,
                       data.frame(file_name= file_name,
                                  date = date,
                                  site = site,
                                  season = season,
                                  index = index,
                                  region = i,
                                  acoustic_region = acoustic_regions_day1$acoustic_region[i],
                                  time_period = time_period,  # Use single-minute time period
                                  region_range = acoustic_regions_day1$region_range[i],
                                  row_range = paste(j, j, sep = "-"),
                                  col_range = paste(acoustic_regions_day1$col_start[i], acoustic_regions_day1$col_end[i], sep = "-"),
                                  minute_sum = minute_sum,
                                  minute_average = minute_average))
    }
  }
}

# Save the results as both CSV and XLSX files
write.csv(results, "d:/acoustic region analysis 2023/results/minute_frequency_time_summaries_acoustic_regions.csv", row.names = FALSE)
write.xlsx(results, "d:/acoustic region analysis 2023/results/minute_frequency_time_summaries_acoustic_regions.xlsx")
