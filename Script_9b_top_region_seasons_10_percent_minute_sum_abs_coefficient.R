library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Read in data
data <- readxl::read_excel("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 10 percent).xlsx")

# Calculate the top 10% abs_Coefficient values
top_10_percent_abs_Coefficient <- data$abs_Coefficient # change here to calculate a different percentage

# Filter rows with top 10% abs_Coefficient values
data_top_10_percent_abs_Coefficient <- data %>% filter(abs_Coefficient >= top_10_percent_abs_Coefficient)

# Extract regions and seasons for top 10% abs_Coefficient values
regions_seasons_top_10_percent_abs_Coefficient <- data_top_10_percent_abs_Coefficient %>% dplyr::select(Acoustic_Region, Season) %>% filter(Season %in% c("May", "March"))

# Count occurrences of regions and seasons
region_season_counts <- regions_seasons_top_10_percent_abs_Coefficient %>% group_by(Acoustic_Region, Season) %>% summarise(Count = n())

# Spread data to wide format
region_season_counts_wide <- region_season_counts %>% spread(key = Season, value = Count)

# Replace NA values with 0
region_season_counts_wide[is.na(region_season_counts_wide)] <- 0

# Calculate total occurrence for each region
region_season_counts_wide$Total_Occurrence <- rowSums(region_season_counts_wide[,c("March", "May")])

# Rename columns
colnames(region_season_counts_wide) <- c("Region_top10%", "March", "May", "Total_Occurrence")

# Reorder columns
region_season_counts_wide <- region_season_counts_wide[,c("Region_top10%", "Total_Occurrence", "March", "May")]

# Order rows by Total_Occurrence in descending order
#region_season_counts_wide <- region_season_counts_wide[order(-region_season_counts_wide$Total_Occurrence),]

# Define the mapping of region labels
region_labels <- c("Low-frequency_Pre-Dawn", "Mid-low-frequency_Pre-Dawn", "Mid-high-frequency_Pre-Dawn", "High-frequency_Pre-Dawn", "Low-frequency_Dawn", "Mid-low-frequency_Dawn", "Mid-high-frequency_Dawn", "High-frequency_Dawn", "Low-frequency_Day", "Mid-low-frequency_Day", "Mid-high-frequency_Day", "High-frequency_Day", "Low-frequency_Dusk", "Mid-low-frequency_Dusk", "Mid-high-frequency_Dusk", "High-frequency_Dusk", "Low-frequency_Night", "Mid-high-frequency_Night", "Mid-low-frequency_Night", "High-frequency_Night")

# Add rows for regions with 0 occurrence
all_regions <- data.frame(Region_top10 = region_labels)
region_season_counts_wide_complete <- all_regions %>% 
  left_join(region_season_counts_wide, by = c("Region_top10" = "Region_top10%"))

# Replace NA values with 0
region_season_counts_wide_complete[is.na(region_season_counts_wide_complete)] <- 0

# Write result to csv file
write.csv(region_season_counts_wide_complete, file = "D:/Acoustic Region Analysis 2023/Results/top_region_occurrences_10_percent_with_seasons_minute_sum_abs_coefficient.csv", row.names = FALSE)

# Gather data to long format
region_season_counts_long <- region_season_counts_wide_complete %>% gather(key = "Season", value = "Count", -Region_top10, -Total_Occurrence)

# Convert Region_top10 to a factor and specify the levels
region_season_counts_long$Region_top10 <- factor(region_season_counts_long$Region_top10, levels = region_season_counts_wide_complete$Region_top10)

# Create stacked bar plot
p <- ggplot(region_season_counts_long, aes(x = Region_top10, y = Count, fill = Season)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  scale_x_discrete(name = "Acoustic Regions", labels = region_labels) +
  scale_y_continuous(name = "Occurrences in highest 10% coefficients") +
  scale_fill_manual(values = c("gray", "lightgray"), name = "Season") +
  labs(title = "Occurrence of acoustic regions within the 10% highest coefficients") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Make x-axis labels vertical and adjust size
        axis.text.y = element_text(size = 8),  # Adjust size of y-axis labels
        plot.title = element_text(size = 10, face = "bold"),  # Adjust size and style of title
        legend.title = element_text(size = 10),  # Adjust size of legend title
        legend.text = element_text(size = 8))  # Adjust size of legend text

plot(p)

# Save the plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/top_region_seasons_mean_ratio_10_percent_minute_sum_abs_coefficient.png", plot = p, width = 10/2, height = 11.69/2, dpi = 300, units = "in")

# Calculate mean, median, sd, and se of highest ratios for selected regions
data_top_10_percent_abs_Coefficient_summary <- data_top_10_percent_abs_Coefficient %>% group_by(Acoustic_Region, Season) %>% summarise(Mean = mean(abs_Coefficient), Median = median(abs_Coefficient), SD = sd(abs_Coefficient), SE = sd(abs_Coefficient) / sqrt(n()))


# Calculate maximum and minimum of highest ratios for selected regions
data_top_10_percent_abs_Coefficient_summary_max_min <- data_top_10_percent_abs_Coefficient %>% group_by(Acoustic_Region, Season) %>% summarise(Max = max(abs_Coefficient), Min = min(abs_Coefficient))

# View maximum and minimum of highest ratios for selected regions
data_top_10_percent_abs_Coefficient_summary_max_min

# Transpose all ratios for selected regions
data_top_10_percent_abs_Coefficient_transpose <- t(data_top_10_percent_abs_Coefficient$abs_Coefficient)

# View transposed ratios for selected regions
data_top_10_percent_abs_Coefficient_transpose

# Save plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/top_region_seasons_10_percent_minute_sum_abs_coefficient.png", plot = last_plot())


# Merge results into a single data frame
all_results <- merge(region_season_counts_wide_complete, data_top_10_percent_abs_Coefficient_summary, by.x = c("Region_top10"), by.y = c("Acoustic_Region"), all = TRUE)
all_results <- merge(all_results, data_top_10_percent_abs_Coefficient_summary_max_min, by.x = c("Region_top10"), by.y = c("Acoustic_Region"), all = TRUE)

# Replace NA values with 0
all_results[is.na(all_results)] <- 0

# Write combined results to csv file
write.csv(all_results, file = "D:/Acoustic Region Analysis 2023/Results/top_regions_occurrance_10_percent_ALL_results_minute_sum_abs_coefficient.csv", row.names = FALSE)
