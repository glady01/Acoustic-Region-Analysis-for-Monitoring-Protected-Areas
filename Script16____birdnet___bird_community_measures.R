# Load necessary libraries
library(dplyr)
library(vegan)
library(readxl)
library(writexl)

# Define the output directory and filename
output_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"

# Define the filename with the correct extension
file_name <- "bird_species_incidence_summary.csv"

# Load data from the CSV file
data <- read.csv(file_name)

# Rename the tot_incidence column to Abundance
names(data)[names(data) == "tot_incidence"] <- "Abundance"

# False positive species removed
species_removed <- c("Green Sandpiper", "Jerdon's Nightjar", "Sri Lanka Frogmouth",
                       "Walker's Ground Cricket", "Western Crowned Warbler")

# Filter out unwanted Acoustic Regions (base R)
filtered_data <- data[data$Acoustic_Region != "High-frequency_Pre-Dawn" & data$Acoustic_Region != "Mid-high-frequency_Pre-Dawn", ]

# Filter out unwanted species from remaining regions
filtered_data <- filtered_data[!filtered_data$Common.name %in% species_removed, ]

# Calculate indices for each site, season
summary_site_season <- filtered_data %>%
  group_by(Site, Season) %>%
  summarise(
    Species_richness = n_distinct(Common.name),
    Total_abundance = sum(Abundance),
    Shannon_Diversity = diversity(Abundance, index = "shannon"),
    Dominance = max(Abundance) / sum(Abundance),
    Simpsons_Diversity = diversity(Abundance, index = "simpson"),
    Pielous_Evenness = Shannon_Diversity / log(exp(1)) * Species_richness,
    Equitability_J = Shannon_Diversity / log(exp(1)) * Species_richness,
    Exclusive_species = sum(!Common.name %in% data[data$Site != first(Site) & data$Season != first(Season),]$Common.name),
    .groups = 'drop'
  )

# Calculate indices for each acoustic region, season
summary_region_season <- filtered_data %>%
  group_by(Acoustic_Region, Season) %>%
  summarise(
    Species_richness = n_distinct(Common.name),
    Total_abundance = sum(Abundance),
    Shannon_Diversity = diversity(Abundance, index = "shannon"),
    Dominance = max(Abundance) / sum(Abundance),
    Simpsons_Diversity = diversity(Abundance, index = "simpson"),
    Pielous_Evenness = Shannon_Diversity / log(exp(1)) * Species_richness,
    Equitability_J = Shannon_Diversity / log(exp(1)) * Species_richness,
    .groups = 'drop'
  )

# Calculate indices for each combination of site, acoustic region, and season
summary_site_region_season <- filtered_data %>%
  group_by(Acoustic_Region, Site, Season) %>%
  summarise(
    Species_richness = n_distinct(Common.name),
    Total_abundance = sum(Abundance),
    Shannon_Diversity = diversity(Abundance, index = "shannon"),
    Dominance = max(Abundance) / sum(Abundance),
    Simpsons_Diversity = diversity(Abundance, index = "simpson"),
    Pielous_Evenness = Shannon_Diversity / log(exp(1)) * Species_richness,
    Equitability_J = Shannon_Diversity / log(exp(1)) * Species_richness,
    .groups = 'drop'
  )

# Calculate species list for each combination with separate cells (using reframe)
species_list <- filtered_data %>%
  group_by(Acoustic_Region, Site, Season) %>%
  mutate(id = row_number()) %>%  # Create a unique ID within each group
  mutate(Species = unique(Common.name), Scientific_name = unique(Scientific.name)) %>%  # Calculate unique species and their scientific names
  ungroup()  # Remove grouping


# Save the summaries and species lists to an Excel file in the output directory
write_xlsx(list(Site_Season_Summary = summary_site_season, Region_Season_Summary = summary_region_season, Site_Region_Season_Summary = summary_site_region_season, Species_List = species_list), path = paste0(output_dir, "bird_community_measures.xlsx"))
