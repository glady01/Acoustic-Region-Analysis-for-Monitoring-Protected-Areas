library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

# Define the output directory
output_dir <- "D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/"

# Read the Excel file
df1 <- read_excel("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/For Renaming and Combining Features of Acoustic Region Ratio Results (top 20 percent).xlsx ")

# Read the CSV files
df2 <- read.csv("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/MUNIPARA_tot_occurrence ~ Acoustic_Region-Site-Season-Scientific.name/MUNIPARA_coefficients_descending_best_Ridge_model.csv")
df3 <- read.csv("D:/Acoustic_Regions_filtered_Raven_Events_All_Region/output/Minute_Regularisation_models/OLAKARA_tot_occurrence ~ Acoustic_Region-Site-Season-Scientific.name/OLAKARA_coefficients_descending_best_Ridge_model.csv")

# Add site information to df2 and df3
df2$site <- "Munipara"
df3$site <- "Olakara"

# Combine df2 and df3
df23 <- rbind(df2, df3)

# Split the 'Feature' column into separate columns
df23 <- df23 %>%
  separate(Feature, c("Acoustic_Region", "Season", "Scientific.name"), sep = ":")

# Merge df1 and df23
merged_df <- merge(df1, df23, by.x = c("acoustic_region", "site", "season"), by.y = c("Acoustic_Region", "site", "Season"))

# Ensure that abs_coefficients are positive
merged_df$abs_Coefficient <- abs(merged_df$abs_Coefficient)

# Write the merged data frame to an Excel file in the output directory
write_xlsx(merged_df, paste0(output_dir, "Merged Acoustic Region Ratio Results with Site-Wise Ridge Model Coefficients.xlsx"))
