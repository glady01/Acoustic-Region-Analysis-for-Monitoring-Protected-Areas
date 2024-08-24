# Load necessary libraries
library(dplyr)
library(readxl)
library(readr)
library(openxlsx)

# Define file path
file_path1 <- "D:/Acoustic Region Analysis 2023/Results/non_para_pairwise_results_minute_sum.xlsx"

# Read the data from the xlsx file
data1 <- read_excel(file_path1)

# Convert the acoustic_region column to character
data1$acoustic_region <- as.character(data1$acoustic_region)

# Add site column based on the sign in the ratio column
data1$site <- ifelse(data1$ratio < 0, 'Olakara', 'Munipara')

# Create a new column that combines acoustic_region, site, Season, and Index
data1$combined <- paste0("region_renamed", data1$acoustic_region, ":site", data1$site, ":season", data1$Season, ":index", data1$Index)

# Filter rows where Significant column contains '*'
data1_filtered <- data1 %>% 
  filter(Significant == "*")

# Extract the top 20 percent ratios
top_20_percent <- data1_filtered %>% 
  top_n(n = nrow(data1_filtered) * 0.2, wt = ratio_raw) # change the percentage here

# Define the output file path
output_file_path <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/For Renaming and Combining Features of Acoustic Region Ratio Results (top 20 percent).xlsx"

# Write top_10_percent to an Excel file
write.xlsx(top_20_percent, output_file_path)
