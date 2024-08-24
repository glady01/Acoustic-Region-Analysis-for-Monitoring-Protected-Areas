# Load required libraries
library(ggplot2)
library(dplyr)
library(readxl)
library(xlsx)

# Set your output directory
output_dir <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output_both/"

# Read the data from the files
df1 <- read_excel("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 10 percent).xlsx")
df2 <- read_excel("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_tot_occurrence/Scientific.name_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 10 percent).xlsx")

# Merge the dataframes on the common columns
merged_df <- merge(df1, df2, by=c("Acoustic_Region", "Site", "Season"))

# Create the bubble plot 
p <- ggplot(merged_df, aes(x = Index, y = Scientific.name, size = Coefficient.x, color = Coefficient.y)) +
  geom_point(alpha = 0.6) +
  facet_grid(Acoustic_Region ~ Site + Season, switch = "y", scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill="lightgrey", colour="grey", size=0.1)) +
  labs(size = "Coefficient (Index)", color = "Coefficient (Events)", y = "Acoustic Regions: Events") +
  scale_color_continuous(limits = c(4, 16)) +
  guides(size = guide_legend(override.aes = list(fill = "#D3D3D3", color = "#D3D3D3")))

# Increase the width of the plot
p <- p + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
p <- p + scale_color_continuous(trans = "reverse")

# Save the plot to the output directory
ggsave(paste0(output_dir, "/bubble_plot_10_percent.png"), plot = p, width = 12, height = 8)

####################
# Create the bubble plot (reverse the roles of Coefficient.x and Coefficient.y)
p <- ggplot(merged_df, aes(x = Index, y = Scientific.name, size = Coefficient.y, color = Coefficient.x)) +
  geom_point(alpha = 0.6) +
  facet_grid(Acoustic_Region ~ Site + Season, switch = "y", scales = "free_x") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill="lightgrey", colour="grey", size=0.1)) +
  labs(size = "Coefficient (Events)", color = "Coefficient (Index)", y = "Acoustic Regions: Events") +
  scale_color_continuous(limits = c(4, 16)) +
  guides(size = guide_legend(override.aes = list(fill = "#D3D3D3", color = "#D3D3D3")))

# Increase the width of the plot
p <- p + theme(plot.margin = margin(1, 1, 1, 1, "cm"))
p <- p + scale_color_continuous(trans = "reverse")

# Save the plot to the output directory
ggsave(paste0(output_dir, "/bubble_plot_10_percent_reverse.png"), plot = p, width = 12, height = 8)



####################
# Create a new column for the abbreviations
merged_df$Abbreviation <- ifelse(grepl("Birds", merged_df$Scientific.name), "B",
                                 ifelse(grepl("Cicadas", merged_df$Scientific.name), "C",
                                        ifelse(grepl("Orthopterans", merged_df$Scientific.name), "O", NA)))

# Bar plot
p_bar <- ggplot(merged_df, aes(x = Index, y = Coefficient.x, fill = Coefficient.y)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(Acoustic_Region ~ Site + Season, switch = "y", scales = "free_x") +
  geom_text(aes(label=Abbreviation), vjust=-0.3, color="black", size=3.5, position = position_dodge(width = 0.9)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect(fill="lightgrey", colour="grey", size=0.1)) +
  labs(fill = "Coefficient (Scientific.name)", y = "Coefficient (Index)")

# Save the bar plot to the output directory
ggsave(paste0(output_dir, "/bar_plot_events_10_percent.png"), plot = p_bar, width = 12, height = 8)

# Save the data used for the bubble plot to an Excel file
write.xlsx(merged_df, file.path(output_dir, "Bubble_plot_data_based_on_both_ridge_regression_coefficients_of_minute_sum_vs_tot_occurrence_10_perecent.xlsx"))
