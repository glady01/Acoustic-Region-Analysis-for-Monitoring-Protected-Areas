# Load necessary libraries
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# Define the output directory
output_dir <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/"

# Read the CSV files
df1 <- read.csv("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/MUNIPARA_minute_sum ~ acoustic_region-site-season-index/MUNIPARA_coefficients_descending_best_Ridge_model.csv")
df2 <- read.csv("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models_minute_sum/OLAKARA_minute_sum ~ acoustic_region-site-season-index/OLAKARA_coefficients_descending_best_Ridge_model.csv")

# Add site information to df1 and df2
df1$Site <- "Munipara"
df2$Site <- "Olakara"

# Combine df1 and df2
df12 <- rbind(df1, df2)

# Split the 'Feature' column into separate columns
df12 <- df12 %>%
  separate(Feature, c("Acoustic_Region", "Season", "Index"), sep = ":", extra = "merge", fill = "right")

# Reorder the columns so that 'Site' comes after 'Acoustic_Region'
df12 <- df12 %>%
  select(X, Acoustic_Region, Site, everything())

# Ensure that abs_coefficients are positive
df12$abs_Coefficient <- abs(df12$abs_Coefficient)

# Extract the top 10 percent abs_Coefficients
top_10_percent <- df12 %>% 
  top_n(n = nrow(df12) * 0.1, wt = abs_Coefficient)

# Write the top 10 percent data frame to an Excel file in the output directory
write_xlsx(top_10_percent, paste0(output_dir, "index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 10 percent).xlsx"))

# Extract the top 20 percent abs_Coefficients
top_20_percent <- df12 %>% 
  top_n(n = nrow(df12) * 0.2, wt = abs_Coefficient)

# Write the top 20 percent data frame to an Excel file in the output directory
write_xlsx(top_20_percent, paste0(output_dir, "index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 20 percent).xlsx"))

# Extract the top 100 percent abs_Coefficients
top_100_percent <- df12 %>% 
  top_n(n = nrow(df12) * 1.0, wt = abs_Coefficient)

# Write the top 100 percent data frame to an Excel file in the output directory
write_xlsx(top_100_percent, paste0(output_dir, "index_Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients (top 100 percent).xlsx"))
