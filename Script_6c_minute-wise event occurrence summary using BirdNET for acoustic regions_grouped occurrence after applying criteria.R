# Load required library
library(openxlsx)

# Set working directory
setwd("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output")

# Read data from xlsx file
data <- read.xlsx("Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_CRITERIA_APPLIED.xlsx")

# Define regions, seasons, and sites
regions <- unique(data$Acoustic_Region)
seasons <- unique(data$Season)
sites <- unique(data$Site)
date <- unique(data$Date)
minute <- unique(data$Minute)

# Initialize final data frame
final_data <- data.frame()

# Loop over regions, seasons, sites, and days
for (region in regions) {
  for (season in seasons) {
    for (site in sites) {
      for (date in unique(data$Date)) {
        for (minute in unique(data$Minute)) {
          # Filter data for current region, season, site, date and minute
          region_season_site_date_minute_data <- data[data$Acoustic_Region == region & data$Season == season & data$Site == site & data$Date == date & data$Minute == minute,]
          
          # Check if there are any rows to aggregate
          if (nrow(region_season_site_date_minute_data) > 0) {
            # Calculate total occurrence for each group
            tot_occurrence <- aggregate(cbind(total_incidence_07 = region_season_site_date_minute_data$total_incidence_07), by = list(region_season_site_date_minute_data$Scientific.name), sum)
            colnames(tot_occurrence) <- c("Scientific.name", "tot_occurrence")
            
            # Add rows for absent species with zero total occurrence
            all_species <- unique(data$Scientific.name)
            absent_species <- setdiff(all_species, tot_occurrence$Scientific.name)
            absent_species_data <- data.frame(Scientific.name = absent_species, tot_occurrence = 0)
            absent_species_data <- absent_species_data[!is.na(absent_species_data$Scientific.name), ]
            tot_occurrence <- rbind(tot_occurrence, absent_species_data)
            
            # Calculate relative occurrence for each group and round to two decimal places
            #tot_occurrence$rel_occurrence <- round(tot_occurrence$tot_occurrence / sum(tot_occurrence$tot_occurrence), 2)
            
            # Add region, season, site, and day columns
            tot_occurrence$Acoustic_Region <- region
            tot_occurrence$Season <- season
            tot_occurrence$Site <- site
            tot_occurrence$Date <- date
            tot_occurrence$Minute <- minute
            
            # Rearrange columns in desired order
            tot_occurrence <- tot_occurrence[, c("Acoustic_Region", "Season", "Site", "Date", "Minute", "Scientific.name", "tot_occurrence")]
            
            # Append data to final data frame
            final_data <- rbind(final_data, tot_occurrence)
          }
        }
      }
    }
  }
}

# Correctly format the Day column as a character string (Corrected line)
final_data$Date <- format(as.Date(final_data$Date, origin = "1899-12-30"), "%d-%m-%Y")  # Specify the origin

# Save the summary data to an Excel workbook
output_file <- paste0("Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_GROUPED_OCCURRENCE.xlsx")
write.xlsx(final_data, output_file)
