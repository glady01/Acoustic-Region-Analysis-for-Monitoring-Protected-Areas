# Load required library
library(openxlsx)
library(dplyr)

# Set working directory
setwd("D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024")

# Read data from xlsx file
data <- read.xlsx("__DAILY__GRAND_SUMMARY_BirdNET_Bird_Species_Selected_REGIONS.xlsx")

# Define the species that meet the criteria (0.9 confidence level)
selected_species <- unique(data[data$'0.9-1.(tot)' > 0, ]$Common.name)

# Define regions and seasons
regions <- c("High-frequency_Dawn", "High-frequency_Day", "High-frequency_Dusk", "High-frequency_Night", "High-frequency_Pre-Dawn",
             "Low-frequency_Dawn", "Low-frequency_Day", "Low-frequency_Dusk", "Low-frequency_Night", "Low-frequency_Pre-Dawn",
             "Mid-high-frequency_Dawn", "Mid-high-frequency_Day", "Mid-high-frequency_Dusk", "Mid-high-frequency_Night", "Mid-high-frequency_Pre-Dawn",
             "Mid-low-frequency_Dawn", "Mid-low-frequency_Day", "Mid-low-frequency_Dusk", "Mid-low-frequency_Night", "Mid-low-frequency_Pre-Dawn")


seasons <- c("March", "May")

# Initialize final data frame
final_data <- data.frame()

# Define sites
sites <- c("Olakara", "Munipara")

# Loop over regions, seasons and sites
for (region in regions) {
  for (season in seasons) {
    for (site in sites){
      # Filter data for selected species and 0.3 or higher abundance
      region_season_data <- data[data$Common.name %in% selected_species & data$Acoustic_Region == region & data$Season == season & data$`0.3_above_total_occurrance` >= 1, ]
      
      # Append filtered data to final data frame
      final_data <- rbind(final_data, region_season_data)
    }
  }
}

# Apply renaming to the data
final_data$Scientific.name <- final_data$Scientific.name

# Parse and format Day column using base R's as.Date() function
final_data$Day <- as.Date(final_data$Day, origin = "1899-12-30")

# Save the summary data to an Excel workbook
output_file <- paste0("0.3__DAILY__GRAND_SUMMARY_BirdNET_Bird_Species_Selected_REGIONS_CRITERIA_APPLIED.xlsx")
write.xlsx(final_data, output_file)
