library(openxlsx)

# Set the working directory
setwd("D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024")

# Get a list of all the xlsx files in the directory that match the specified pattern
files <- list.files(pattern = "^(DAILY)_(Mid-high-frequency|Mid-low-frequency|Low-frequency|High-frequency)_(Pre-Dawn|Dawn|Day|Dusk|Night)_(Munipara|Olakara)_(Olakara|Munipara)_(March|May)_(Bird_Species_SUMMARY)\\.xlsx$")

# Initialize an empty data frame to store the merged data
merged_data <- data.frame()

# Loop through each file
for (file in files) {
  # Read the data from the file
  data <- read.xlsx(file)
  
  # Parse and format the Day column using base R's as.Date() function
  data$Day <- as.Date(data$Day, origin = "1899-12-30")
  
  # Extract the region name and time of day from the file name
  file_parts <- strsplit(file, "_")[[1]]
  frequency <- file_parts[2]
  time_of_day <- file_parts[3]
  
  # Combine the frequency and time of day to create the content definition
  content_definition <- paste(frequency, time_of_day, sep = "_")
  
  # Insert the content definition as the first column of the data with column name Acoustic_Region
  data <- cbind(Acoustic_Region = content_definition, data)
  
  # Append the data to the merged data frame
  merged_data <- rbind(merged_data, data)
}

# Write the merged data to a new xlsx file with specified name
write.xlsx(merged_data, "__DAILY__GRAND_SUMMARY_BirdNET_Bird_Species_Selected_REGIONS.xlsx")
