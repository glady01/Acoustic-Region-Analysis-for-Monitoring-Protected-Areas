# Load required library
library(openxlsx)
library(dplyr)

# Set working directory
setwd("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output")

# Read data from xlsx file
data <- read.xlsx("Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS.xlsx")

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
    # Replace NA values with 0
    data$total_incidence_07[is.na(data$total_incidence_07)] <- 0
    # Initialize selected species vector
    selected_species <- c()
    for (site in sites){
      # Calculate total abundance of each species for current region, season and site
      total_abundance <- data %>%
        filter(Acoustic_Region == region, Season == season, Site == site) %>%
        group_by(Common.name) %>%
        summarize(total_abundance = sum(total_incidence_07), .groups = 'drop')
      
      # Find species that meet criteria in at least one site for current region and season
      selected_species_site <- total_abundance[total_abundance$total_abundance >= 20,]$Common.name
      
      # Append selected species from current site to selected species vector
      selected_species <- c(selected_species, selected_species_site)
    }
    
    # Filter data to include all rows for selected species in current region and season
    region_season_data <- data[data$Common.name %in% selected_species & data$Acoustic_Region == region & data$Season == season, ]
    
    
    # Append filtered data to final data frame
    final_data <- rbind(final_data, region_season_data)
  }
}

# Define renaming dictionary
renaming_dict <- c("Clicker Round-winged Katydid" = "Orthopterans",
                   "Unknown Cricket1" = "Orthopterans",
                   "Cicada2" = "Cicadas",
                   "Birds" = "Birds",
                   "Southern Chorus Frog" = "Anurans",
                   "Wind" = "Wind",
                   "Unknown Cricket2" = "Orthopterans",
                   "Cicada1" = "Cicadas",
                   "Texas Bush Katydid" = "Orthopterans",
                   "Logging" = "Logging",
                   "Common Virtuoso Katydid" = "Orthopterans",
                   "Rain" = "Rain",
                   "Stripe-faced Meadow Katydid" = "Orthopterans",
                   "HF Cricket" = "Orthopterans",
                   "Oblong-winged Katydid" = "Orthopterans",
                   "Dog" = "Dog",
                   "Plains" = "Plains",
                   "Thunder" = "Thunder",
                   "Rummagging" = "Rummaging",
                   "Four-spotted Tree Cricket" = "Orthopterans",
                   "Eastern Narrow-mouthed Toad" = "Anurans",
                   "Cicada3" = "Cicadas",
                   "Macaques" = "Mammals",
                   "Carolina Ground Cricket" = "Orthopterans",
                   "Fast-calling Tree Cricket" = "Orthopterans",
                   "Great Plains Toad" = "Anurans",
                   "Great Plains Narrow-mouthed Toad" = "Anurans",
                   "Malabar Giant Squirrel" = "Mammals",
                   "Black-horned Tree Cricket" = "Orthopterans",
                   "Houston Toad" = "Anurans",
                   "Flying Squirrel" = "Mammals")

# Apply renaming to the data
final_data$Scientific.name <- renaming_dict[final_data$Scientific.name]

# Parse and format Day column using base R's as.Date() function
final_data$Date <- as.Date(final_data$Date, origin = "1899-12-30")

# Save the summary data to an Excel workbook
output_file <- paste0("Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_CRITERIA_APPLIED.xlsx")
write.xlsx(final_data, output_file)

