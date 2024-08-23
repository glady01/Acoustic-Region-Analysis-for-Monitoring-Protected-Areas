# ==============================================================================
# Title: Processing and Summarizing Acoustic Region Data Across Frequency and Time
# Author: [Zaibin]
# Date Created: 2024-02-01
# ==============================================================================

# Description:
# This script processes and summarizes acoustic region data across frequency and time, calculating grand sums and grand averages for defined regions and half-hourly time periods.

# Input:
# - CSV files containing normalized daily acoustic data for each index separately for sites and seasons in a specific format

# Output:
# - CSV and XLSX files containing summarized data with additional columns

# ==============================================================================

library(openxlsx)

# Set the working directory
setwd("D:/Acoustic Region Analysis 2023/Acoustic Region Data_Day-wise/finer_Without_first_column_Daily Acoustic Region Data_Normalised")

# Get a list of all CSV files in the directory
files <- list.files(pattern = "\\.csv$")

# Modify the results data frame to include the new columns
results <- data.frame(site = character(),
                      season = character(),
                      index = character(),
                      file_name= character(),
                      date = character(),
                      region = numeric(),
                      time_zone = character(),
                      region_renamed2 = character(),
                      row_range = character(),
                      col_range = character(),
                      grand_sum = numeric(),
                      grand_average = numeric(),
                      stringsAsFactors = FALSE)

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
 
  #Assigning half-hourly time zones to acoustic regions
  acoustic_regions_day1$time_zone <- paste0("row ", formatC(acoustic_regions_day1$row_start, width = 2, flag = "0"), 
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
  
  ## Create a sequence of 30-minute time periods
  time_periods <- format(seq(from = as.POSIXct("00:00:00", format = "%H:%M:%S"), 
                             to = as.POSIXct("23:30:00", format = "%H:%M:%S"), 
                             by = "30 min"), 
                         format = "%H:%M:%S")
  
  # Create a vector of time period ranges
  time_period_ranges <- paste(time_periods, format(as.POSIXct(time_periods, format = "%H:%M:%S") + 25*60, format = "%H:%M:%S"), sep = " - ")
  
  
  # Extract data for each region and calculate the grand sum and grand average
  for (i in 1:nrow(acoustic_regions_day1)) {
    # Extract the data for the current region
    region_data <- data[(acoustic_regions_day1$row_start[i] - 1):(acoustic_regions_day1$row_end[i] - 1), 
                        acoustic_regions_day1$col_start[i]:acoustic_regions_day1$col_end[i]]
    
    # Calculate the grand sum, grand average, and average of averages for each 6 consecutive rows
    for (j in seq(1, nrow(region_data), by = 6)) {
      sub_region_data <- region_data[j:min(j+5, nrow(region_data)), ]
      
      # Calculate row sums and grand sum
      row_sums <- rowSums(sub_region_data, na.rm = TRUE)
      grand_sum <- sum(row_sums)
      
      # Print the row sums and grand sum
      #print(paste("Row sums for region", i, "and time period", time_period_ranges[(j-1)/6 + 1]))
      #print(row_sums)
      #print(paste("Grand sum for region", i, "and time period", time_period_ranges[(j-1)/6 + 1]))
      #print(grand_sum)
      
      # Calculate grand average as the mean of row sums
      grand_average <- mean(row_sums)
      
      # Print the grand average
      #print(paste("Grand average for region", i, "and time period", time_period_ranges[(j-1)/6 + 1]))
      #print(grand_average)
      
      # Inside the loop, add the row and column ranges to the results
      results <- rbind(results,
                       data.frame(file_name= file_name,
                                  date = date,
                                  site = site,
                                  season = season,
                                  index = index,
                                  region = i,
                                  acoustic_region = acoustic_regions_day1$acoustic_region[i],
                                  time_period = time_period_ranges[(j-1)/6 + 1],  # Check indexing
                                  time_zone = acoustic_regions_day1$time_zone[i],
                                  row_range = as.character(paste(j, min(j+5, nrow(region_data)), sep = "-")),
                                  col_range = as.character(paste(acoustic_regions_day1$col_start[i], acoustic_regions_day1$col_end[i], sep = "-")),
                                  grand_sum = grand_sum,
                                  grand_average = grand_average))
    }
  }
}
# Save the results as both CSV and XLSX files
write.csv(results, "d:/acoustic region analysis 2023/results/half-hourly_frequency_time_summaries_acoustic_regions.csv", row.names = FALSE)
write.xlsx(results, "d:/acoustic region analysis 2023/results/half-hourly_frequency_time_summaries_acoustic_regions.xlsx")
