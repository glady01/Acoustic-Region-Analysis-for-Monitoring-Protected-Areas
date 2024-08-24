# Load necessary libraries
library(dplyr)
library(ggplot2)

# Function to convert to sentence case
toSentenceCase <- function(x) {
  sapply(strsplit(x, split = "[:_]"), function(word) paste(toupper(substring(word, 1,1)), substring(word, 2), sep = "", collapse = " "))
}

# Read in data
data1 <- readxl::read_excel("D:/Acoustic Region Analysis 2023/Results/non_para_pairwise_results_minute_sum_wilcoxon_z_ratio.xlsx")
data2 <- readxl::read_excel("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 100 percent).xlsx")

# Filter significant rows
data1 <- data1 %>% filter(Significant == "*")

# Create a new column for site
data1$site <- ifelse(data1$ratio < 0, "Olakara", "Munipara")

# Make sure the common columns have the same names and levels in both data frames
names(data1)[names(data1) == "acoustic_region"] <- "Acoustic_Region"
data1$Acoustic_Region <- toSentenceCase(data1$Acoustic_Region)
data1$season <- toSentenceCase(data1$season)
data1$index <- toSentenceCase(data1$index)

data2$Acoustic_Region <- toSentenceCase(data2$Acoustic_Region)
data2$Season <- toSentenceCase(data2$Season)
data2$Index <- toSentenceCase(data2$Index)

# Join data1 and data2 on the common columns
data <- left_join(data1, data2, by = c("Acoustic_Region", "season" = "Season", "index" = "Index"))

# Define font sizes
title_size <- 14
axis_title_size <- 12
axis_text_size <- 10
legend_title_size <- 12
legend_text_size <- 10
facet_label_size <- 11  # New variable for facet label size

# Create a bubble plot
bubble_plot <- ggplot(data, aes(x = Acoustic_Region, y = interaction(season, index), size = abs(ratio_raw))) +
  geom_point(aes(fill = ifelse(Coefficient > 0, Coefficient, NA)), shape = 21, alpha = 0.6) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, guide = "colourbar", 
                       name = "Coefficient", na.value = "white", limits = c(10, 40)) +  # Set limits to match the range of your data
  scale_size_continuous(range = c(1, 10)) +
  labs(x = "Acoustic Regions", y = "Season*Index", fill = "Color", size = "Ratio Raw") +
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

# Print the bubble plot
print(bubble_plot)

# Save the bubble plot
ggsave(filename = "D:/Acoustic Region Analysis 2023/Results/bubble_plot_region_ratio_coefficient_with_seasons_index.png", plot = bubble_plot, width = 10/2, height = 11.69/2, dpi = 300, units = "in")
