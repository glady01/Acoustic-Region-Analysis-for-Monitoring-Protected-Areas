# Load necessary libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyverse)
library(writexl)

# Define the path to the Excel file
data_path <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute__GRAND_SUMMARY_BirdNET_EVENTS_ALL_REGIONS_GROUPED_OCCURRENCE.xlsx"

# Read the data directly from the Excel file
merged_data <- read_excel(data_path)

# Define the specified acoustic regions
specified_regions <- c("Low-frequency/Pre-Dawn", "Mid-low-frequency/Pre-Dawn", "Mid-high-frequency/Pre-Dawn", "High-frequency/Pre-Dawn", "Low-frequency/Dawn", "Mid-low-frequency/Dawn", "Mid-high-frequency/Dawn", "High-frequency/Dawn", "Low-frequency/Day", "Mid-low-frequency/Day", "Mid-high-frequency/Day", "High-frequency/Day", "Low-frequency/Dusk", "Mid-low-frequency/Dusk", "Mid-high-frequency/Dusk", "High-frequency/Dusk", "Low-frequency/Night", "Mid-high-frequency/Night", "Mid-low-frequency/Night", "High-frequency/Night")

# Replace underscores with slashes in the Acoustic_Region column
merged_data$Acoustic_Region <- str_replace_all(merged_data$Acoustic_Region, "_", "/")

# Filter the data to include only the specified Acoustic Regions and the ACI index
filtered_data <- merged_data %>%
  filter(`Acoustic_Region` %in% specified_regions)

# Group the filtered data by Acoustic Regions, Sites, Seasons, Events, Index, and Region, and summarise the count of each group
grouped_data <- filtered_data %>%
  group_by(`Acoustic_Region`, Site, Season, `Scientific.name`) %>%
  summarise(Count = sum(tot_occurrence))

# Filter the grouped data to include only the events with abundance greater than zero
grouped_data <- grouped_data %>%
  filter(Count > 0)

# Save the grouped_data to an xlsx file
write_xlsx(grouped_data, "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/minute_sum_above_0_7_with_Sites_all_acoustic_regions_ratios_abundance_bubble_plot_data.xlsx")

# Define the order of the events and acoustic regions
event_order <- c("Anurans", "Birds", "Cicadas",  "Dog",  "Logging", "Orthopterans", "Plains", "Rain", "Wind", "Thunder", "Mammals", "Rummaging")
#region_order <- c("High-frequency/Pre-Dawn","High-frequency/Dawn", "High-frequency/Day",  "High-frequency/Night", 
#"Mid-high-frequency/Night","Mid-high-frequency/Pre-Dawn", "Mid-high-frequency/Dawn", "Mid-high-frequency/Day", "Mid-low-frequency/Pre-Dawn", "Mid-low-frequency/Dusk",
#"Low-frequency/Dawn", "Low-frequency/Dusk", "Low-frequency/Night")

# Create a bubble plot
p <- ggplot(grouped_data, aes(x=factor(`Acoustic_Region`), y=factor(`Scientific.name`, levels=event_order), size=Count, color=Site)) +
  geom_point(alpha=0.6) +
  facet_wrap(~Season, ncol = 2) +  # Increase the number of columns for facets
  scale_size(range = c(1, 12)) +
  scale_color_manual(values = c("Olakara" = "#FFB6C1", "Munipara" = "green")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 13),
        legend.title = element_text(size = 14),
        plot.title = element_text(size = 13),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        strip.text = element_text(size = 13)) +
  labs(x="Acoustic Regions", y="Events", size="Count", color="Sites", title="Bubble plot of abundance of 12 events in all acoustic regions")

# Create a bubble plot with flipped axes
p_flipped <- p + coord_flip()

# Save the flipped plot to the output directory
ggsave(filename = "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/minute_sum_above_0_7_with_Sites_all_acoustic_regions_ratios_abundance_bubble_plot_flipped.png", plot = p_flipped, width = 10, height = 8)

# Save the original plot to the output directory
ggsave(filename = "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/minute_sum_above_0_7_with_Sites_all_acoustic_regions_ratios_abundance_bubble_plot_background.png", plot = p, width = 10, height = 8)

# Save the second flipped plot to the output directory
ggsave(filename = "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/minute_sum_above_0_7_with_Sites_all_acoustic_regions_ratios_abundance_bubble_plot_flipped2.png", plot = p_flipped, width = 10, height = 8)
