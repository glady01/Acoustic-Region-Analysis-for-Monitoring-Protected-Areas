library(dplyr)
library(tidyr)
library(lubridate)
library(xlsx)

# Set the working directory
setwd("D:/Acoustic_Regions_filtered_Raven/output_bird_incidence_2024")


# Get a list of all CSV files in the working directory that match the specified pattern
input_files <- list.files(pattern = "^(Mid-high-frequency|Mid-low-frequency|Low-frequency|High-frequency)_(Pre-Dawn|Dawn|Day|Dusk|Night)_(Olakara|Munipara)_(Olakara|Munipara)_(May|March)_BirdNET\\.csv$")

# Loop over all input files
for (input_file in input_files) {
  # Derive the region name, time, sites, and month from the input file name
  input_file_parts <- strsplit(input_file, "_")[[1]]
  region_name <- input_file_parts[1]
  time <- input_file_parts[2]
  site1 <- input_file_parts[3]
  site2 <- input_file_parts[4]
  month <- input_file_parts[5]
  
  # Read in the data
  data <- read.csv(input_file)
  
  # Format the scientific name and common name
  data$Scientific.name <- gsub("\\.", " ", data$Scientific.name)
  data$Common.name <- gsub("\\.", " ", data$Common.name)
  
  # Format the date_time column
  data$Date_Time <- ymd_hms(data$Date_Time)
  
  # Calculate total incidence of each species at a site
  incidence <- data %>%
    group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time)) %>%
    summarize(tot_incidence = n_distinct(Date_Time), .groups = "drop")
  
  # Add a new column that calculates the highest confidence for each species for both sites
  incidence <- incidence %>%
    left_join(
      data %>%
        group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time)) %>%
        summarize(
          max_confidence = max(Confidence),
          .groups = "drop"
        ),
      by = c("Season", "Scientific.name", "Common.name", "Site", "Day")
    )
  
  # Find for each species at a site at which hour the species incidence was the highest
  incidence <- incidence %>%
    left_join(
      data %>%
        mutate(hour = hour(Date_Time)) %>%
        group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time), hour) %>%
        summarize(count = n(), .groups = "drop") %>%
        group_by(Season, Scientific.name, Common.name, Site, Day) %>%
        summarize(hour = hour[which.max(count)], .groups = "drop"),
      by = c("Season", "Scientific.name", "Common.name", "Site", "Day")
    )
  
  incidence$hour <- format(
    as.POSIXct(sprintf("%02d:00:00", incidence$hour), format = "%H:%M:%S"),
    format = "%I:%M %p"
  )
  
  munipara_data <- incidence %>%
    filter(Site == "Munipara") %>%
    select(Season, Scientific.name)
  olakara_data <- incidence %>%
    filter(Site == "Olakara") %>%
    select(Season, Scientific.name)
  
  munipara_exclusive_species <- setdiff(munipara_data, olakara_data)
  olakara_exclusive_species <- setdiff(olakara_data, munipara_data)
  common_species <- intersect(munipara_data, olakara_data)
  
  # Add a status column indicating exclusive or common to each of the site
  incidence$status <- NA
  incidence$status[incidence$Site == "Munipara" & 
                     incidence$Scientific.name %in% munipara_exclusive_species$Scientific.name] <-
    "Exclusive"
  incidence$status[incidence$Site == "Olakara" & 
                     incidence$Scientific.name %in% olakara_exclusive_species$Scientific.name] <-
    "Exclusive"
  incidence$status[incidence$Scientific.name %in% common_species$Scientific.name] <-
    "Common"
  
  # Create a new column representing the 6-minute interval for each observation
  data$six_minute_interval <- cut(
    minute(as.POSIXlt(data$Date_Time, format = "%Y-%m-%d %H:%M:%S")),
    breaks = seq(0, 60, by = 6),
    labels = c("00-06", "06-12", "12-18", "18-24", "24-30", "30-36", "36-42", "42-48", "48-54", "54-60"),
    include.lowest = TRUE
  )
  
  # Calculate average incidence, sd and se for average incidence for each species in any given 6-minute interval
  incidence <- data %>%
    mutate(Date_Time = as.POSIXlt(Date_Time, format = "%Y-%m-%d %H:%M:%S")) %>%
    group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time), hour(Date_Time), six_minute_interval) %>%
    summarize(interval_incidence = n_distinct(Date_Time), .groups = "drop") %>%
    group_by(Season, Scientific.name, Common.name, Site, Day) %>%
    summarize(avg_hrly_occurrence_tot = mean(interval_incidence),
              sd_hrly_occurrence_tot = sd(interval_incidence),
              se_hrly_occurrence_tot = sd(interval_incidence)/sqrt(n()), .groups = "drop") %>%
    left_join(incidence, by = c("Season", "Scientific.name", "Common.name", "Site", "Day"))
  
  # Calculate total incidence of each species at a site for each confidence interval
  confidence_intervals <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
  for (i in confidence_intervals) {
    incidence <- incidence %>%
      left_join(
        data %>%
          filter(Confidence >= i) %>%
          group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time)) %>%
          summarize(
            !!paste0(i, "-1 (tot)") := n_distinct(Date_Time),
            .groups = "drop"
          ),
        by = c("Season", "Scientific.name", "Common.name", "Site", "Day")
      )
    # Replace NA with 0
    incidence[is.na(incidence)] <- 0
  }
  
  # Add additional columns to calculate total incidence for incidences above a specific confidence threshold (i.e. greater than or equal to 0.3)
  incidence <- incidence %>%
    left_join(
      data %>%
        filter(Confidence >= 0.3) %>%
        group_by(Season, Scientific.name, Common.name, Site, Day = as.Date(Date_Time)) %>%
        summarize(
          `0.3_above_total_occurrance` = n_distinct(Date_Time),
          avg_hrly_occurrence_03 = mean(n_distinct(Date_Time)),
          sd_hrly_occurrence_03 = sd(n_distinct(Date_Time)),
          se_hrly_occurrence_03 = sd(n_distinct(Date_Time))/sqrt(n()),
          .groups = "drop"
        ),
      by = c("Season", "Scientific.name", "Common.name", "Site", "Day")
    )
  # Replace NA with 0
  incidence[is.na(incidence)] <- 0
  
  
  # Arrange the result in descending order of tot_incidence
  incidence <- incidence %>%
    arrange(desc(tot_incidence))
  
  # Order the species based on highest total incidence to lowest
  incidence <- incidence %>%
    arrange(desc(tot_incidence))
  
  # Find exclusive and common species for both sites
  munipara_exclusive_species <- setdiff(munipara_data, olakara_data)
  olakara_exclusive_species <- setdiff(olakara_data, munipara_data)
  common_species <- intersect(munipara_data, olakara_data)
  
  # Filter incidence to include only exclusive and common species for both sites
  munipara_exclusive_species_data <- incidence %>%
    filter(Site == "Munipara" & Scientific.name %in% munipara_exclusive_species$Scientific.name)
  olakara_exclusive_species_data <- incidence %>%
    filter(Site == "Olakara" & Scientific.name %in% olakara_exclusive_species$Scientific.name)
  common_species_data <- incidence %>%
    filter(Scientific.name %in% common_species$Scientific.name)
  
  # Save the summary data to an Excel workbook
  output_file <- paste0("DAILY_", region_name,"_",time,"_",site1,"_",site2,"_",month,"_Bird_Species_SUMMARY.xlsx")
  
  # Format the columns
  incidence$hour <- format(incidence$hour, width = 5)
  incidence$status <- format(incidence$status, width = 5)
  incidence$avg_hrly_occurrence_tot <- round(incidence$avg_hrly_occurrence_tot, digits = 2)
  incidence$sd_hrly_occurrence_tot <- round(incidence$sd_hrly_occurrence_tot, digits = 2)
  incidence$se_hrly_occurrence_tot <- round(incidence$se_hrly_occurrence_tot, digits = 2)
  incidence$max_confidence <- round(incidence$max_confidence, digits = 3)
  
  
  # Format the Day column using base R's as.Date() function
  incidence$Day <- as.Date(incidence$Day, origin = "1899-12-30")
  
  # Save the summary data to an Excel file
  write.xlsx(incidence[, c("Season", "Scientific.name", "hour", "status","Site", "Day", "Common.name",  "tot_incidence", "0.3_above_total_occurrance", 
                           "max_confidence",  "avg_hrly_occurrence_tot", 
                           "sd_hrly_occurrence_tot","se_hrly_occurrence_tot", 
                           paste0(confidence_intervals, "-1 (tot)"))], output_file)
  
} 
