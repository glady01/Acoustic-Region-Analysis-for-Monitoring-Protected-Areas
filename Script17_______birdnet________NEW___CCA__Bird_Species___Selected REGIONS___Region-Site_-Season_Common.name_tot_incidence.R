# Load libraries
library(tidyverse)
library(readxl)
library(vegan)
library(scatterplot3d)
library(factoextra)

# Set directories (adjust for your file paths)
input_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"
output_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"

# Read data with correct header names
data <- read_xlsx(file.path(input_dir, "__DAILY__GRAND_SUMMARY_BirdNET_Bird_Species_Selected_REGIONS_CRITERIA_APPLIED.xlsx"))

# Define the names you want to exclude
exclude_names <- c("Green Sandpiper", "Jerdon's Nightjar", "Sri Lanka Frogmouth",
                   "Walker's Ground Cricket", "Western Crowned Warbler")

# Filter out rows with certain Species
data <- data %>% filter(!(Common.name %in% exclude_names))

# Define the regions that need to be excluded
exclude_regions <- c("High-frequency_Pre-Dawn", "Mid-high-frequency_Pre-Dawn")

# Filter out rows with certain Acoustic Regions
data <- data %>% filter(!(Acoustic_Region %in% exclude_regions))

# Convert to numeric and handle warnings
data <- data %>% mutate(across(c(tot_incidence), as.numeric, .warn = FALSE))

# Filter and prepare data
data_cca <- data %>% select(Acoustic_Region, Site, Season, Common.name, tot_incidence)

# Summarize tot_incidence for each combination of Acoustic_Region, Site, Season, and Common.name
data_summarized <- data_cca %>%
  group_by(Acoustic_Region, Site, Season, Common.name) %>%
  summarise(tot_incidence = sum(tot_incidence, na.rm = TRUE))

# Reshape data to wide format for tot_incidence (Common.name)
data_wide_events <- data_summarized %>%
  pivot_wider(names_from = Common.name, values_from = tot_incidence, values_fill = list(numeric = 0))

# Ensure all columns are numeric
data_wide <- data_wide_events %>% mutate(across(everything(), as.numeric, .warn = FALSE))

# Replace NA values with 0
data_wide[is.na(data_wide)] <- 0

# Save reshaped data to a CSV file
write.csv(data_wide, file.path(output_dir, "CCA_data_reshaped.csv"))

# Get the column names for the events (excluding Season)
birds_cols <- names(data_wide_events)[!names(data_wide_events) %in% c("Acoustic_Region", "Site", "Season")]

# Define your events
birds <- unique(data$Common.name)

# Check which species exist in your data frame
existing_birds <- birds[birds %in% names(data_wide)]

# Make sure your existing species are included in the event_cols
birds_cols <- existing_birds

# Convert tibble to data frame before setting row names
data_wide <- as.data.frame(data_wide)

# Now you can set row names
data_wide$Site <- paste(data_wide$Site, data_wide$Acoustic_Region, data_wide$Season, sep = "_")
rownames(data_wide) <- data_wide$Site

# Perform CCA with conditioning variables, including Season
cca_result <- cca(data_wide[birds_cols], Condition = data_wide[, c("Acoustic_Region", "Site")])

png(filename="CCA_biplot.png", width=800, height=600)
plot(cca_result)
dev.off()


# Print the summary of the CCA result
print(summary(cca_result))

# Define the output file path
output_file <- file.path(output_dir, "CCA_result_summary.txt")

# Redirect output to the file
sink(output_file)

# Print the summary of the CCA result
print(summary(cca_result))

# Reset output to console
sink()

#############################

# Extract the site scores for all three axes
site_scores <- scores(cca_result, display = "sites", choices = 1:3)

# Extract the species scores for all three axes
species_scores <- scores(cca_result, display = "species", choices = 1:3)

# Define the output file path
output_file <- file.path(output_dir, "CCA_3D_all_axis_plot.png")

# Save the plot to a PNG file
png(output_file)
############################


# Create a 3D scatter plot of the site scores
s3d <- scatterplot3d(site_scores[,1], site_scores[,2], site_scores[,3], pch = 19, color = "blue", xlab = "CA1", ylab = "CA2", zlab = "CA3")

# Add labels to the points for sites
text(s3d$xyz.convert(site_scores[,1], site_scores[,2], site_scores[,3]), labels = rownames(site_scores), cex = 0.6, pos = 4)

# Add labels to the points for species
text(s3d$xyz.convert(species_scores[,1], species_scores[,2], species_scores[,3]), labels = rownames(species_scores), cex = 0.6, pos = 4, col = "red")


# Close the PNG device
dev.off()
