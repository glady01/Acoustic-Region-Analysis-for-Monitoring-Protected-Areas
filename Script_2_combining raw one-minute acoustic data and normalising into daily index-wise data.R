# ==============================================================================
# Title: Combining and Normalizing Acoustic Data
# Author: [Name/Affiliation]
# Date Created: 2024-02-01
# ==============================================================================

# Description:
# This script automates the process of combining raw one-minute acoustic data into
# daily index data for sites and seasons based on file names. It then normalizes the combined data 
# using 1.5 and 98.5 percentile bounds (Towsey 2014). The script processes all CSV files within the
# specified directory and its subdirectories.

# Dependencies:
# - tidyverse
# - lubridate

# Input:
# - CSV files containing raw one minute index-wise data in a specified directory

# Output:
# - Normalized CSV files after combining them into daily index-wise data with the suffix "_normalised.csv" in a specified output directory

# ==============================================================================

# Load necessary libraries
library(tidyverse)
library(lubridate)

# Set the working directory
setwd("D:/Acoustic Region Analysis 2023/Acoustic Region Data_Day-wise/_Raw_Daily Acoustic Region Data")

# Get a list of all directories
dirs <- list.dirs(recursive = FALSE)

# Set the output directory for combined data
combined_dir <- "D:/Acoustic Region Analysis 2023/Acoustic Region Data_Day-wise/Combined_Daily_Acoustic_Region_Data"

# Loop through each directory
for (dir in dirs) {
  # Get the site and season from the directory name
  dir_name <- basename(dir)
  dir_split <- strsplit(dir_name, "_")[[1]]
  site <- dir_split[1]
  season <- dir_split[2]
  
  # Get a list of all csv files in the directory
  csv_files <- list.files(dir, pattern = "*.csv", full.names = TRUE)
  
  # Loop through each csv file
  for (csv_file in csv_files) {
    # Get the index from the csv file name
    csv_file_name <- basename(csv_file)
    csv_file_split <- strsplit(csv_file_name, "_")[[1]]
    index <- csv_file_split[5]
    date <- ymd(csv_file_split[1])
    
    # Read in the csv file
    df <- read_csv(csv_file)
    
    # Check if a combined file already exists for this date and index
    combined_file_name <- paste(date, site, season, index, sep = "_")
    combined_file_path <- file.path(combined_dir, combined_file_name)
    if (file.exists(combined_file_path)) {
      # If the combined file already exists, append to it
      write_csv(df, combined_file_path, append = TRUE)
    } else {
      # If the combined file does not exist, create it
      write_csv(df, combined_file_path)
    }
  }
}

#==============================================================================

# Set the output directory for normalized data
output_dir <- "D:/Acoustic Region Analysis 2023/Acoustic Region Data_Day-wise/norm_Daily_Acoustic_Region_Data"

# Function to normalize data
normalise <- function (x, xmin, xmax) {
  y <- (x - xmin)/(xmax - xmin)
}

# Function to normalize data in a file and save to a new file
normalise_file <- function(file_path) {
  # Load and read summary indices
  indices_all <- read.csv(file_path, header = TRUE, sep = ",", quote = "")
  
  # Remove the first column
  indices_all <- indices_all[,-1]
  
  # Replace NA values with 0
  indices_all[is.na(indices_all)] <- 0
  
  # Create a normalized dataset between 1.5 and 98.5% bounds 
  indices_norm <- indices_all
  
  # Normalize values between 1.5 and 98.5 percentile bounds
  q1.values <- NULL
  q2.values <- NULL
  for (i in 1:ncol(indices_all)) {
    q1 <- unname(quantile(indices_all[,i], probs = 0.015, na.rm = TRUE))
    q2 <- unname(quantile(indices_all[,i], probs = 0.985, na.rm = TRUE))
    q1.values <- c(q1.values, q1)
    q2.values <- c(q2.values, q2)
    indices_norm[,i]  <- normalise(indices_all[,i], q1, q2)
  }
  
  # Adjust values greater than 1 or less than 0 to 1 and 0 respectively
  for (j in 1:ncol(indices_norm)) {
    a <- which(indices_norm[,j] > 1)
    indices_norm[a,j] = 1
    a <- which(indices_norm[,j] < 0)
    indices_norm[a,j] = 0
  }
  
  # Save normalized data to a new file
  new_file_name <- sub("\\.csv$", "_normalised.csv", basename(file_path))
  new_file_path <- file.path(output_dir, new_file_name)
  write.csv(indices_norm, new_file_path, row.names = FALSE)
}

# Recursively search for all .csv files in the combined directory and its subdirectories
files <- list.files(path = combined_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)

# Normalize data in each file
for (file in files) {
  normalise_file(file)
}
