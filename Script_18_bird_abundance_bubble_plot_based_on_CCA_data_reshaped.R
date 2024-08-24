# Load the required library
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)

# Set directories (adjust for your file paths)
input_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"
output_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024/"

# Read data with correct header names
data <- read_csv(file.path(input_dir, "CCA_data_reshaped.csv"))

# Assuming 'data' is your dataframe
# Reshape the data to long format for ggplot
data_long <- data %>% 
  pivot_longer(cols = starts_with("Asian"):starts_with("Yellow-browed Bulbul"),
               names_to = "Species",
               values_to = "Value")

# Abbreviate the Acoustic_Region names
data_long$Acoustic_Region <- factor(data_long$Acoustic_Region,
                                    levels = c("Mid-high-frequency_Day", "Mid-high-frequency_Dawn", "Mid-low-frequency_Dawn"),
                                    labels = c("Mid-high/Day", "Mid-high/Dawn", "Mid-low/Dawn"))


# Create a new variable for the x-axis
data_long$Acoustic_Site_Season <- paste(data_long$Acoustic_Region, data_long$Site, data_long$Season, sep = "-")

# Create a new variable for the facets
data_long$Site_Season <- paste(data_long$Site, data_long$Season, sep = "-")

# Order the levels of Site_Season so that Olakara comes before Munipara
data_long$Site_Season <- factor(data_long$Site_Season, levels = c(sort(unique(data_long$Site_Season))))

# Order the Species variable based on the abundance in the Olakara March facet
species_order <- data_long %>%
  filter(Site == "Olakara", Season == "March") %>%
  group_by(Species) %>%
  summarise(Total = sum(Value, na.rm = TRUE)) %>%
  arrange(desc(Total)) %>%
  pull(Species)

data_long$Species <- factor(data_long$Species, levels = species_order)

# Create the bubble plot
bird_abundance_bubble_plot2 <- ggplot(data_long, aes(x = Acoustic_Region, y = Species, size = Value)) +
  geom_point(data = subset(data_long, Value > 0), alpha = 0.6) +
  scale_size(range = c(1, 10)) +
  facet_wrap(~ Site_Season, scales = "free_x", nrow = 1) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Acoustic Regions", y = "Bird Species", size = "Abundance")

# Print the plot to screen
print(bird_abundance_bubble_plot2)

# Define the output file path for the plot
plot_file <- file.path(output_dir, "bird_abundance_bubble_plot2.png")

# Save the plot to a file
ggsave(plot_file, bird_abundance_bubble_plot2, width = 10, height = 8.5, units = "in")
