#### Combine CSV Files ###################

## Purpose: to read in all the separate files of a directory and combine them into one

# Created 9/27/2023

# Last modified: 9/27/2023

combine_csv_files <- function(directory_path) {
  # Get a list of all CSV files in the specified directory
  csv_files <- list.files(path = directory_path, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty dataframe to store the combined data
  combined_data <- data.frame()
  
  # Loop through each CSV file and combine its data with the existing dataframe
  for (file in csv_files) {
    file_data <- read.csv(file)
    combined_data <- rbind(combined_data, file_data)
  }
  
  return(combined_data)
}