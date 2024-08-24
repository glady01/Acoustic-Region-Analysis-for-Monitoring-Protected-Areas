# Load necessary libraries
library(ggplot2)
library(reshape2)
library(readxl)
library(dplyr)
library(tidyverse)
library(stringr)
library(GGally)
library(openxlsx)

# Define the output directory
output_dir <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/"

# Read the data from the xlsx file
merged_data <- read_excel(paste0(output_dir, "Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients.xlsx"))

# Rename the columns
names(merged_data)[names(merged_data) == "abs_Coefficient"] <- "abs_Coefficient"
names(merged_data)[names(merged_data) == "Coefficient"] <- "Coefficients"
names(merged_data)[names(merged_data) == "ratio_raw"] <- "Ratios"
names(merged_data)[names(merged_data) == "acoustic_region"] <- "Acoustic Regions"
names(merged_data)[names(merged_data) == "index"] <- "Indices"
names(merged_data)[names(merged_data) == "site"] <- "Sites"
names(merged_data)[names(merged_data) == "season"] <- "Seasons"
names(merged_data)[names(merged_data) == "Scientific.name"] <- "Events"

# Replace underscores with slashes in the Acoustic Regions column
merged_data$`Acoustic Regions` <- str_replace_all(merged_data$`Acoustic Regions`, "_", "/")

#---------------------------------------------
# Define the order of the events and acoustic regions
#event_order <- c("Cicadas", "Orthopterans", "Anurans", "Birds", "Mammals", "Rummaging", "Logging", "Dog", "Plains", "Wind", "Rain", "Thunder")
#region_order <- c("High-frequency/Dawn", "High-frequency/Day", "High-frequency/Pre-Dawn", "Mid-high-frequency/Day", "Mid-high-frequency/Pre-Dawn", "High-frequency/Night", "Low-frequency/Dusk", "Mid-high-frequency/Night")

# Convert the variables to factors with specified levels
#merged_data$Events <- factor(merged_data$Events, levels = event_order)
#merged_data$`Acoustic Regions` <- factor(merged_data$`Acoustic Regions`, levels = region_order)

########################
# Convert the variables to factors
merged_data$Indices <- as.factor(merged_data$Indices)
merged_data$`Acoustic Regions` <- as.factor(merged_data$`Acoustic Regions`)
merged_data$Site <- as.factor(merged_data$Sites)
merged_data$Season <- as.factor(merged_data$Seasons)
merged_data$Events <- as.factor(merged_data$Events)

# Filter rows where Coefficients is greater than or equal to 0
merged_data <- merged_data[merged_data$Coefficients >= 0,]
print(nrow(merged_data))

# Create a new column that combines Acoustic Regions, Sites, Seasons, and Indices
merged_data$combined <- paste(merged_data$`Acoustic Regions`, merged_data$Sites, merged_data$Seasons, merged_data$Indices, sep = ":")

# Create unique_combinations
unique_combinations <- unique(merged_data[, c("Acoustic Regions", "Sites", "Seasons", "Indices", "combined")])

# Filter merged_data to only include rows where the combination of Acoustic Regions:Sites:Seasons:Indices is in the unique combinations for that site
merged_data <- merged_data[merged_data$combined %in% unique_combinations$combined,]
print(nrow(merged_data))

# Remove regions:Munipara:March/May from Olakara section of y axis, and regions:Olakara:March/May from Munipara section of y axis
merged_data <- merged_data[!((merged_data$Sites == "Olakara" & merged_data$`Acoustic Regions` == "Munipara" & merged_data$Seasons %in% c("March", "May")) |
                               (merged_data$Sites == "Munipara" & merged_data$`Acoustic Regions` == "Olakara" & merged_data$Seasons %in% c("March", "May"))),]
print(nrow(merged_data))

# Get the maximum Ratio for each Acoustic Regions:Sites:Seasons:Indices combination
max_ratios <- merged_data %>% group_by(`Acoustic Regions`, Sites, Seasons, Indices) %>% summarise(max_ratio = ifelse(any(!is.na(Ratios)), max(Ratios, na.rm = TRUE), NA), ratio_raw = round(first(Ratios), 1), .groups = "drop")

# Join max_ratios back to merged_data and sort by max_ratio in ascending order
merged_data <- merged_data %>% left_join(max_ratios, by = c("Acoustic Regions", "Sites", "Seasons", "Indices")) %>% arrange(desc(max_ratio))

# Create a factor for Acoustic Regions with levels ordered by max_ratio
merged_data$`Acoustic Regions` <- factor(merged_data$`Acoustic Regions`, levels = unique(merged_data$`Acoustic Regions`[order(merged_data$max_ratio)]))

# Create a new column that combines Indices and ratio_raw
merged_data$combined <- paste0(merged_data$Indices, " (", merged_data$ratio_raw, ")")

# Group by Acoustic Regions and Sites and paste together the unique combined values for each region
# Sort the combined values in descending order before pasting them together
index_groups <- merged_data %>% group_by(`Acoustic Regions`, Sites) %>% summarise(combined = paste(sort(unique(combined), decreasing = TRUE), collapse = ", "))

# Join index_groups back to merged_data
merged_data <- merged_data %>% left_join(index_groups, by = c("Acoustic Regions", "Sites"))

# Create a new column that combines Acoustic Regions, Sites and combined from index_groups
# Use str_wrap() to create line breaks after a certain width
merged_data$combined <- paste0(merged_data$`Acoustic Regions`, ":", merged_data$Sites, "\n", str_wrap(merged_data$combined.y, width = 60))

# Check if every facet will have at least one row of data
facet_combinations <- expand.grid(Sites = unique(merged_data$Sites), Seasons = unique(merged_data$Seasons))
missing_facets <- facet_combinations[!paste(facet_combinations$Sites, facet_combinations$Seasons) %in% paste(merged_data$Sites, merged_data$Seasons),]

if(nrow(missing_facets) > 0) {
  stop("The following combinations of Sites and Seasons do not have any data: ", paste(missing_facets$Sites, missing_facets$Seasons, sep = ":", collapse = ", "))
}

##########################################
# Pairwise Scatterplots
ggpairs(merged_data[, c("Coefficients", "Ratios", "Acoustic Regions", "Indices", "Sites", "Seasons", "Events")])
ggsave(filename = "above_0_7__20%_top_ratios_Pairwise scatterplot showing interactions of Coefficients, Ratios, Acoustic Regions, Indices, Site, and Season_Final.png", plot = last_plot(), path = output_dir, width = 10, height = 10, dpi = 300)

########################
# Create the scatterplot matrix
plot <- ggpairs(merged_data[, c("Coefficients", "Ratios", "Acoustic Regions", "Indices", "Sites", "Seasons", "Events")],
                lower = list(continuous = wrap("points", alpha = 0.5)),
                upper = list(continuous = wrap("cor", size = 3)),
                diag = list(continuous = wrap("barDiag")),
                progress = FALSE) +
  theme_bw() +
  theme(text = element_text(size = 12), # Increase size to 16
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 11), # Rotate x-axis labels and increase size to 14
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=1, size = 10)) # Rotate y-axis labels and increase size to 14

# Save the plot
ggsave(filename = "above_0_7__20%_top_ratios_Pairwise scatterplot showing interactions of Coefficients, Ratios, Acoustic Regions, Indices, Site, and Season1.png", plot = plot, path = output_dir, width = 15, height = 10, dpi = 800)
########################

# Create the heatmap for Coefficients
if(nrow(merged_data) > 0) {
  heatmap_plot_coefficient <- ggplot(merged_data, aes(x = Events, y = combined, fill = Coefficients)) +
    geom_tile() +
    facet_grid(Sites ~ Seasons) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.y = element_text(size = rel(1.05))) +  # Adjust the font size of the y-axis labels
    labs(x = "Species", y = "Region\nIndex (Ratio)", fill = "Coefficients",
         title = "Heatmap of Coefficients by Region:Index and Events",
         subtitle = "Each cell represents the coefficient of an event for a given region:index combination")
  
  # Save the heatmap plot for Coefficients as a PNG file with an appropriate size
  ggsave(filename = paste0(output_dir, "/above_0_7__20%_top_ratios_Heatmap_of_Coefficients_by_Acoustic Region_Index_and_Events__FINAL2.png"), plot = heatmap_plot_coefficient, width = 10, height = 8)
} else {
  print("No data to plot.")
}

# Create the heatmap
ggplot(merged_data, aes(x = combined, y = Sites, fill = Coefficients)) +
  geom_tile(colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Events ~ Seasons) +
  labs(x = "Index (Ratio)", y = "Site", fill = "Coefficients")

# Save the plot
ggsave(filename = "above_0_7__20%_top_ratios_Heatmap_of_Coefficients_by_Region_Index_and_Events__FINAL3.png", plot = last_plot(), path = output_dir, width = 10, height = 10, dpi = 300)

########################

# Identify the combinations that are present in both sections
common_combinations <- intersect(merged_data$combined[merged_data$Sites == "Site1" & merged_data$Seasons == "Season1"],
                                 merged_data$combined[merged_data$Sites == "Site2" & merged_data$Seasons == "Season2"])

# Filter the data to include only the common combinations
filtered_data <- merged_data[merged_data$combined %in% common_combinations,]


#Create the bubble plot
if(nrow(merged_data) > 0) {
  bubble_plot <- ggplot(merged_data, aes(x = Events, y = combined, size = Coefficients, color = Events)) +
    geom_point(alpha = 0.5) +
    facet_grid(Sites ~ Seasons) +
    scale_size_continuous(range = c(1,10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Adjust this line
          axis.text.y = element_text(size = rel(1.05))) +  # Adjust the font size of the y-axis labels
    labs(x = "Events", y = "Acoustic Regions\nIndices (Ratios)", size = "Coefficients", color = "Events",
         title = "Bubble Plot of Coefficients by Acoustic Regions, Indices and Events",
         subtitle = "Each cell represents the coefficient of an event for a given region:index combination")
  
  # Save the bubble plot as a PNG file with an appropriate size
  ggsave(filename = paste0(output_dir, "/above_0_7__20%_top_ratios_Bubble_Plot_of_Coefficients_by_Region_Index_and_Species__FINAL1.png"), plot = bubble_plot, width = 10, height = 8)
}

# Bubble Plot2
ggplot(merged_data, aes(x = combined, y = `Acoustic Regions`, size = Coefficients, color = Events)) +
  geom_point(alpha = 0.5) +
  facet_grid(Events ~ Seasons) +
  labs(x = "Index (Ratio)", y = "Acoustic Regions", size = "Coefficients", color = "Events") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ggsave(filename = "above_0_7__20%_top_ratios_Bubble_Plot_of_Coefficients_by_Region_Index_and_Events__FINAL2.png", plot = last_plot(), path = output_dir, width = 10, height = 10, dpi = 300)

###########################################################

# Print the unique values of the columns used for the x and y axes
print(unique(merged_data$Events))
print(unique(merged_data$combined))

# Print the unique values of the column used for the fill aesthetic
print(unique(merged_data$Coefficients))

# Save the data used for the pairwise scatterplot to an Excel file
write.xlsx(merged_data[, c("Coefficients", "Ratios", "Acoustic Regions", "Indices", "Sites", "Seasons", "Events")], 
           file.path(output_dir, "above_0_7__20%_top_ratios_Bubble_pairwise_scatterplot_data.xlsx"))

# Save the data used for the heatmap to an Excel file
write.xlsx(merged_data, file.path(output_dir, "above_0_7__20%_top_ratios_Bubble_heatmap_data.xlsx"))

# Save the data used for the bubble plot to an Excel file
write.xlsx(merged_data, file.path(output_dir, "above_0_7__20%_top_ratios_Bubble_bubble_plot_data.xlsx"))

############################################################
####without Sites#############
# Modified Bubble Plot
# Create the bubble plot
if(nrow(merged_data) > 0) {
  bubble_plot <- ggplot(merged_data, aes(x = Events, y = combined, size = Coefficients, color = Events)) +
    geom_point(alpha = 0.5) +
    facet_grid(. ~ Seasons) +  # Modify this line
    scale_size_continuous(range = c(1,10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Adjust this line
          axis.text.y = element_text(size = rel(1.05))) +  # Adjust the font size of the y-axis labels
    labs(x = "Events", y = "Acoustic Regions\nIndices (Ratios)", size = "Coefficients", color = "Events",
         title = "Bubble Plot of Coefficients by Acoustic Regions, Indices and Events",
         subtitle = "Each cell represents the coefficient of an event for a given region:index combination")
  
  # Save the bubble plot as a PNG file with an appropriate size
  ggsave(filename = paste0(output_dir, "/above_0_7_without_Sites__20%_top_ratios_Bubble_Plot_of_Coefficients_by_Region_Index_and_Species__FINAL1.png"), plot = bubble_plot, width = 10, height = 8)
}

############################################
# Create a new variable that combines Sites and Seasons
merged_data$Site_Season <- paste(merged_data$Sites, merged_data$Seasons, sep = ":")

# Create the bubble plot
if(nrow(merged_data) > 0) {
  bubble_plot <- ggplot(merged_data, aes(x = Events, y = combined, size = Coefficients, color = Events)) +
    geom_point(alpha = 0.5) +
    facet_grid(. ~ Site_Season) +  # Modify this line
    scale_size_continuous(range = c(1,10)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Adjust this line
          axis.text.y = element_text(size = rel(1.05))) +  # Adjust the font size of the y-axis labels
    labs(x = "Events", y = "Acoustic Regions\nIndices (Ratios)", size = "Coefficients", color = "Events",
         title = "Bubble Plot of Coefficients by Acoustic Regions, Indices and Events",
         subtitle = "Each cell represents the coefficient of an event for a given region:index combination")
  
  # Save the bubble plot as a PNG file with an appropriate size
  ggsave(filename = paste0(output_dir, "/above_0_7_with_Sites_20%_top_ratios_Bubble_Plot_of_Coefficients_by_Region_Index_and_Species__FINAL1.png"), plot = bubble_plot, width = 10, height = 8)
}

###############################################
####Without Site Label, but combined###########
# Create a new variable that combines Sites and Seasons
merged_data$Site_Season <- paste(merged_data$Sites, merged_data$Seasons, sep = ":")

# Create the bubble plot
if(nrow(merged_data) > 0) {
  bubble_plot <- ggplot(merged_data, aes(x = Events, y = combined, size = Coefficients, color = Events)) +
    geom_point(alpha = 0.5) +
    facet_grid(. ~ Site_Season, labeller = labeller(Site_Season = function(x) gsub(".*:", "", x))) +  # Modify this line
    scale_size_continuous(range = c(1,10), breaks = c(1, 10, 100, 500, 1000)) +  # Modify this line
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  # Adjust this line
          axis.text.y = element_text(size = rel(1.05))) +  # Adjust the font size of the y-axis labels
    labs(x = "Events", y = "Acoustic Regions\nIndices (Ratios)", size = "Coefficients", color = "Events",
         title = "Bubble Plot of Coefficients by Acoustic Regions, Indices and Events",
         subtitle = "Each cell represents the coefficient of an event for a given region:index combination")
  
  # Save the bubble plot as a PNG file with an appropriate size
  ggsave(filename = paste0(output_dir, "/above_0_7_with_Sites2_20%_top_ratios_Bubble_Plot_of_Coefficients_by_Region_Index_and_Species__FINAL1.png"), plot = bubble_plot, width = 10, height = 8)
}