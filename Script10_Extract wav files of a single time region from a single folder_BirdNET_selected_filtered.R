library(tuneR)
library(seewave)

# Set working directory
setwd("H:/________________ALL_AFTER_2023_MAY_15/Acoustic_Regions_filtered_all_regions")

# Specify the folder name
folder_name <- "High_frequency"

# Specify the time range
#time_range <- c("053000", "085900")  # Dawn 
#time_range  = c("090000", "172900") # Day
time_range = c("000000", "052900") # Pre-Dawn

# Create directory for the folder in the new directory
dir.create(paste0("H:/Acoustic_Regions_extracted_filtered_selected/", folder_name), recursive = TRUE)

# Get all the .wav files in the current folder
wav_files <- list.files(paste0("H:/________________ALL_AFTER_2023_MAY_15/Acoustic_Regions_filtered_all_regions/", folder_name), pattern = ".wav$", full.names = TRUE)

# Get the start and end times for the time range
start_time <- as.POSIXct(time_range[1], format = "%H%M%S")
end_time <- as.POSIXct(time_range[2], format = "%H%M%S")

# Loop through each .wav file and extract it if it falls within the time range
for (j in seq_along(wav_files)) {
  # Read in the .wav file using readWave()
  wave <- readWave(wav_files[j])
  
  # Get the start and end times for the current .wav file
  wav_file_start_time <- as.POSIXct(substr(basename(wav_files[j]), 10, 17), format = "%H%M%S")
  wav_file_end_time <- wav_file_start_time + duration(wave)
  
  # Extract the .wav file if it falls within the time range
  if (wav_file_start_time >= start_time & wav_file_end_time <= end_time) {
    # Get the output directory for the time range and create it if it doesn't exist
    output_dir <- paste0("H:/Acoustic_Regions_extracted_filtered_selected/", folder_name, "/Pre-Dawn")
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }
    
    # Extract the .wav file to the output directory
    output_file <- paste0(output_dir, "/", basename(wav_files[j]))
    file.copy(wav_files[j], output_file)
  }
}
