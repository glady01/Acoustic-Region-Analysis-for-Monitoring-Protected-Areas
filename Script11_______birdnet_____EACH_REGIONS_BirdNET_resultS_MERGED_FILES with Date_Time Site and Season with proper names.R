# set base directory
base_dir <- "D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024"

# list all subdirectories in the base directory
sub_dirs <- list.dirs(base_dir, recursive = TRUE)

# loop over subdirectories
for (sub_dir in sub_dirs) {
  # set working directory to subdirectory
  setwd(sub_dir)
  
  # list all csv files in the subdirectory
  csv_files <- list.files(pattern = "\\.csv$")
  
  # get unique seasons from file names
  seasons <- unique(sapply(strsplit(csv_files, "[_.]"), function(x) x[4]))
  
  # get time period from subdirectory name
  time_period <- basename(sub_dir)
  
  # loop over seasons
  for (season in seasons) {
    # create an empty list to store data frames
    data_list <- list()
    
    # loop over csv files and append data frames to the list
    for (csv_file in csv_files) {
      # check if file name contains season
      if (grepl(season, csv_file)) {
        # read csv file
        data <- read.csv(csv_file)
        
        # keep only the first 5 columns of data
        data <- data[, 1:5]
        
        # check if data is not empty
        if (nrow(data) > 0) {
          # split file name into parts
          file_name_parts <- strsplit(csv_file, "[_.]")[[1]]
          
          # add Date_Time, Site and Season to all rows in columns 6, 7 and 8 respectively
          data[, 6] <- paste(file_name_parts[1], file_name_parts[2], sep = "_")
          data[, 7] <- file_name_parts[3]
          data[, 8] <- file_name_parts[4]
          
          # append data frame to the list
          data_list[[csv_file]] <- data
        }
      }
    }
    
    # concatenate all data frames in the list into one data frame
    merged_data <- do.call(rbind, data_list)
    
    # rename columns of merged_data
    colnames(merged_data) <- c("Start (s)", "End (s)", "Scientific name", "Common name", "Confidence", "Date_Time", "Site", "Season")
    
    # get unique site names from merged_data
    site_names <- unique(merged_data$Site)
    
    # create file name for merged file
    sub_dir_name <- basename(dirname(sub_dir))
    merged_file_name <- paste(sub_dir_name, time_period, paste(site_names, collapse = "_"), season, "BirdNET.csv", sep = "_")
    
    # write the merged data frame to a csv file in the base directory
    write.csv(merged_data, file.path(base_dir, merged_file_name), row.names = FALSE)
  }
}
