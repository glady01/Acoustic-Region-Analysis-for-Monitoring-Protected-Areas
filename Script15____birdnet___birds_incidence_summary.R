# Load necessary libraries
library(xlsx)
library(dplyr)
library(vegan)

# Define the output directory and filename
output_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"

# Define the complete path to the .xlsx file
file_path <- paste(output_dir, '__DAILY__GRAND_SUMMARY_BirdNET_Bird_Species_Selected_REGIONS_CRITERIA_APPLIED.xlsx', sep = "")

# Read the sheet named 'sheet1' from the .xlsx file
data <- read.xlsx(file_path, 'Sheet 1')

# Group by Acoustic_Region, Site, Common.name, and Scientific.name, and calculate the sum of tot_incidence
summary <- data %>%
  group_by(Acoustic_Region, Season, Site, Common.name, Scientific.name) %>%
  summarise(tot_incidence = sum(tot_incidence))

# Define the complete path to the output .csv file
output_file <- paste(output_dir, "bird_species_incidence_summary.csv", sep = "")

# Save the summary data to a CSV file
write.csv(summary, file = output_file)
