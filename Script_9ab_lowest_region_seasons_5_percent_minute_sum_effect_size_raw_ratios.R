library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Read in data
data <- readxl::read_excel("D:/Acoustic Region Analysis 2023/Results/non_para_pairwise_results_minute_sum.xlsx")

# Filter significant rows
#data <- data %>% filter(Significant == "*")

# Calculate the lowest 5% ratio_raw values
lowest_5_percent_ratio_raw <- quantile(data$ratio_raw, 0.05) # change here to calculate a different percentage

# Filter rows with lowest 5% ratio_raw values
data_lowest_5_percent_ratio_raw <- data %>% filter(ratio_raw <= lowest_5_percent_ratio_raw) # Change the comparison operator to <=


# Extract regions and seasons for lowest 5% ratio_raw values
regions_seasons_lowest_5_percent_ratio_raw <- data_lowest_5_percent_ratio_raw %>% dplyr::select(acoustic_region, season) %>% filter(season %in% c("May", "March"))

# Count occurrences of regions and seasons
region_season_counts <- regions_seasons_lowest_5_percent_ratio_raw %>% group_by(acoustic_region, season) %>% summarise(Count = n())

# Spread data to wide format
region_season_counts_wide <- region_season_counts %>% spread(key = season, value = Count)

# Replace NA values with 0
region_season_counts_wide[is.na(region_season_counts_wide)] <- 0

# Calculate total occurrence for each region
region_season_counts_wide$Total_Occurrence <- rowSums(region_season_counts_wide[,c("March", "May")])

# Rename columns
colnames(region_season_counts_wide) <- c("Region_lowest5%", "March", "May", "Total_Occurrence")

# Reorder columns
region_season_counts_wide <- region_season_counts_wide[,c("Region_lowest5%", "Total_Occurrence", "March", "May")]

# Order rows by Total_Occurrence in descending order
#region_season_counts_wide <- region_season_counts_wide[order(-region_season_counts_wide$Total_Occurrence),]

# Define the mapping of region labels
region_labels <- c("Low-frequency_Pre-Dawn", "Mid-low-frequency_Pre-Dawn", "Mid-high-frequency_Pre-Dawn", "High-frequency_Pre-Dawn", "Low-frequency_Dawn", "Mid-low-frequency_Dawn", "Mid-high-frequency_Dawn", "High-frequency_Dawn", "Low-frequency_Day", "Mid-low-frequency_Day", "Mid-high-frequency_Day", "High-frequency_Day", "Low-frequency_Dusk", "Mid-low-frequency_Dusk", "Mid-high-frequency_Dusk", "High-frequency_Dusk", "Low-frequency_Night", "Mid-high-frequency_Night", "Mid-low-frequency_Night", "High-frequency_Night")

# Add rows for regions with 0 occurrence
all_regions <- data.frame(Region_lowest5 = region_labels)
region_season_counts_wide_complete <- all_regions %>% 
  left_join(region_season_counts_wide, by = c("Region_lowest5" = "Region_lowest5%"))

# Replace NA values with 0
region_season_counts_wide_complete[is.na(region_season_counts_wide_complete)] <- 0

# Write result to csv file
write.csv(region_season_counts_wide_complete, file = "D:/Acoustic Region Analysis 2023/Results/lowest_region_occurrences_5_percent_with_seasons_minute_sum.csv", row.names = FALSE)

# Gather data to long format
region_season_counts_long <- region_season_counts_wide_complete %>% gather(key = "season", value = "Count", -Region_lowest5, -Total_Occurrence)

# Convert Region_lowest5 to a factor and specify the levels
region_season_counts_long$Region_lowest5 <- factor(region_season_counts_long$Region_lowest5, levels = region_season_counts_wide_complete$Region_lowest5)

# Create stacked bar plot
p <- ggplot(region_season_counts_long, aes(x = Region_lowest5, y = Count, fill = season)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  scale_x_discrete(name = "Acoustic Regions", labels = region_labels) +
  scale_y_continuous(name = "Occurrences in lowest 5% ratios") +
  scale_fill_manual(values = c("gray", "lightgray"), name = "season") +
  labs(title = "Occurrence of acoustic regions within the 5% lowest ratios") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Make x-axis labels vertical and adjust size
        axis.text.y = element_text(size = 8),  # Adjust size of y-axis labels
        plot.title = element_text(size = 10, face = "bold"),  # Adjust size and style of title
        legend.title = element_text(size = 10),  # Adjust size of legend title
        legend.text = element_text(size = 8))  # Adjust size of legend text

plot(p)

# Save the plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/lowest_region_seasons_mean_ratio_5_percent_minute_sum.png", plot = p, width = 5/2, height = 11.69/2, dpi = 300, units = "in")

# Calculate mean, median, sd, and se of lowest ratios for selected regions
data_lowest_5_percent_ratio_raw_summary <- data_lowest_5_percent_ratio_raw %>% group_by(acoustic_region, season) %>% summarise(Mean = mean(ratio_raw), Median = median(ratio_raw), SD = sd(ratio_raw), SE = sd(ratio_raw) / sqrt(n()))


# Calculate maximum and minimum of lowest ratios for selected regions
data_lowest_5_percent_ratio_raw_summary_max_min <- data_lowest_5_percent_ratio_raw %>% group_by(acoustic_region, season) %>% summarise(Max = max(ratio_raw), Min = min(ratio_raw))

# View maximum and minimum of lowest ratios for selected regions
data_lowest_5_percent_ratio_raw_summary_max_min

# Transpose all ratios for selected regions
data_lowest_5_percent_ratio_raw_transpose <- t(data_lowest_5_percent_ratio_raw$ratio_raw)

# View transposed ratios for selected regions
data_lowest_5_percent_ratio_raw_transpose

# Save plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/lowest_region_seasons_mean_ratio_5_percent_minute_sum.png", plot = last_plot())


# Merge results into a single data frame
all_results <- merge(region_season_counts_wide_complete, data_lowest_5_percent_ratio_raw_summary, by.x = c("Region_lowest5"), by.y = c("acoustic_region"), all = TRUE)
all_results <- merge(all_results, data_lowest_5_percent_ratio_raw_summary_max_min, by.x = c("Region_lowest5"), by.y = c("acoustic_region"), all = TRUE)

# Replace NA values with 0
all_results[is.na(all_results)] <- 0

# Write combined results to csv file
write.csv(all_results, file = "D:/Acoustic Region Analysis 2023/Results/lowest_regions_occurrance_5_percent_ALL_results_minute_sum.csv", row.names = FALSE)
