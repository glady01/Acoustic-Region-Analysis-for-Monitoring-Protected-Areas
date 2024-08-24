library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Read in data
data <- readxl::read_excel("D:/Acoustic Region Analysis 2023/Results/non_para_pairwise_results_minute_sum_wilcoxon_z_ratio.xlsx")

# Filter significant rows
data <- data %>% filter(Significant == "*")

# Calculate the top 10% ratio_raw values
top_10_percent_ratio_raw <- quantile(data$ratio_raw, 0.9) # change here to calculate a different percentage

# Filter rows with top 10% ratio_raw values
data_top_10_percent_ratio_raw <- data %>% filter(ratio_raw >= top_10_percent_ratio_raw)

# Extract regions and seasons for top 10% ratio_raw values
regions_seasons_top_10_percent_ratio_raw <- data_top_10_percent_ratio_raw %>% dplyr::select(acoustic_region, season) %>% filter(season %in% c("May", "March"))

# Count occurrences of regions and seasons
region_season_counts <- regions_seasons_top_10_percent_ratio_raw %>% group_by(acoustic_region, season) %>% summarise(Count = n())

# Spread data to wide format
region_season_counts_wide <- region_season_counts %>% spread(key = season, value = Count)

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
write.csv(region_season_counts_wide_complete, file = "D:/Acoustic Region Analysis 2023/Results/top_region_occurrences_10_percent_with_seasons_minute_sum.csv", row.names = FALSE)

# Gather data to long format
region_season_counts_long <- region_season_counts_wide_complete %>% gather(key = "season", value = "Count", -Region_top10, -Total_Occurrence)

# Convert Region_top10 to a factor and specify the levels
region_season_counts_long$Region_top10 <- factor(region_season_counts_long$Region_top10, levels = region_season_counts_wide_complete$Region_top10)

# Create stacked bar plot
p <- ggplot(region_season_counts_long, aes(x = Region_top10, y = Count, fill = season)) +
  geom_bar(stat = "identity", color = "black", size = 0.1) +
  scale_x_discrete(name = "Acoustic Regions", labels = region_labels) +
  scale_y_continuous(name = "Occurrences in highest 10% ratios") +
  scale_fill_manual(values = c("gray", "lightgray"), name = "Season") +
  #labs(title = "Occurrence of acoustic regions within the 10% highest ratios") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  # Make x-axis labels vertical and adjust size
        axis.text.y = element_text(size = 8),  # Adjust size of y-axis labels
        plot.title = element_text(size = 10, face = "bold"),  # Adjust size and style of title
        legend.title = element_text(size = 10),  # Adjust size of legend title
        legend.text = element_text(size = 8))  # Adjust size of legend text

plot(p)

# Save the plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/top_region_seasons_ratio_10_percent_minute_sum.png", plot = p, width = 10/2, height = 11.69/2, dpi = 200, units = "in")

#############
# Create a new column for site
data$site <- ifelse(data$ratio < 0, "Olakara", "Munipara")

# Create a scatter plot
scatter_plot <- ggplot(data, aes(x = ratio_raw, y = `Wilcoxon Z Value`, color = site)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("Olakara" = "blue", "Munipara" = "red")) +
  facet_wrap(~ acoustic_region) +
  labs(x = "Ratio Raw", y = "Wilcoxon Z Value", color = "Site") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 9, face = "bold"))

# Print the scatter plot
print(scatter_plot)

# Save the scatter plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/scatter_plot_region_ratio_wilcoxon_with_seasons_index.png", plot = scatter_plot, width = 10/2, height = 11.69/2, dpi = 300, units = "in")

# Create a new column for site
data$site <- ifelse(data$ratio < 0, "Olakara", "Munipara")

# Create a new column for site_color that combines the site and Wilcoxon Z Value information
data$site_color <- ifelse(data$site == "Olakara", data$`Wilcoxon Z Value`, -data$`Wilcoxon Z Value`)

# Create a bubble plot
bubble_plot <- ggplot(data, aes(x = acoustic_region, y = interaction(season, index), size = abs(ratio_raw), fill = site_color)) +
  geom_point(shape = 21, alpha = 0.6) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, guide = "colourbar", 
                       name = "Site (Wilcoxon Z Score)") +
  scale_size_continuous(range = c(1, 10)) +
  labs(x = "Acoustic regions", y = "Season*Index", fill = "Site Color", size = "Ratio Raw") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 9, face = "bold"))

# Print the bubble plot
print(bubble_plot)

# Save the bubble plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/bubble_plot_region_ratio_wilcoxon_with_seasons_index1.png", plot = bubble_plot, width = 10/2, height = 11.69/2, dpi = 300, units = "in")

# Create a bubble plot
bubble_plot <- ggplot(data, aes(x = acoustic_region, y = interaction(season, index), size = abs(ratio_raw), fill = site_color)) +
  geom_point(shape = 21, alpha = 0.6) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, guide = "colourbar", 
                       name = "Site (Wilcoxon Z Score)", limits = c(0, 60)) +
  scale_size_continuous(range = c(1, 10)) +
  labs(x = "Acoustic regions", y = "Season*Index", fill = "Site Color", size = "Ratio Raw") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(size = 9, face = "bold"))

# Print the bubble plot
print(bubble_plot)

# Save the bubble plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/bubble_plot_region_ratio_wilcoxon_with_seasons_index2.png", plot = bubble_plot, width = 10/2, height = 11.69/2, dpi = 300, units = "in")

# Create separate data frames for the two sites
data_olakara <- subset(data, site == "Olakara")
data_munipara <- subset(data, site == "Munipara")

# Combine the data frames
data_combined <- rbind(data_olakara, data_munipara)

# Add a new column to distinguish the sites
data_combined$site <- factor(data_combined$site, levels = c("Olakara", "Munipara"))

# Create a bubble plot for the combined data
# Define font sizes
title_size <- 14
axis_title_size <- 12
axis_text_size <- 10
legend_title_size <- 12
legend_text_size <- 10
facet_label_size <- 11  # New variable for facet label size

bubble_plot_combined <- ggplot(data_combined, aes(x = acoustic_region, y = interaction(season, index), size = abs(ratio_raw), fill = `Wilcoxon Z Value`)) +
  geom_point(shape = 21, alpha = 0.6) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, guide = "colourbar") +
  scale_size_continuous(range = c(1, 10), name = "Ratio") +  # Change legend title
  labs(x = "Acoustic regions", y = "Season*Index") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = title_size, face = "bold"),
    axis.title.x = element_text(size = axis_title_size),
    axis.title.y = element_text(size = axis_title_size),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = axis_text_size),
    axis.text.y = element_text(size = axis_text_size),
    legend.title = element_text(size = legend_title_size),
    legend.text = element_text(size = legend_text_size),
    strip.text = element_text(size = facet_label_size)  # Change facet label size
  ) +
  facet_wrap(~ site)

# Print the bubble plot for the combined data
print(bubble_plot_combined)

# Copy the current plot to a png device
dev.copy(png, filename = "D:/Acoustic Region Analysis 2023/Results/final_bubble_plot_region_ratio_wilcoxon_with_seasons_index_combined.png", width = 10, height = 11.69, units = "in", res = 300)

# Save the plot
dev.off()
