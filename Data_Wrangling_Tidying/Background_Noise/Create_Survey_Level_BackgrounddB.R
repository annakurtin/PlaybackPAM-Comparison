#### Create Survey Period Estimates of Background Noise ###################

## Purpose: to read in the datasheets with estimates of background noise 

# Created 5/16/2024

# Last modified: 5/16/2024

#TODO
# Wait until region 5 is finished running
# incorporate region 5 into this script
# Write the data from this script
# Copy over your analysis code
# Incorporate decibel as a survey level covariate in the detection models and see what happens

#### Setup #################################
packages <- c("tidyverse","janitor","lubridate")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Create_SiteColumn_fromPointID.R")
load_packages(packages)

#### Read in and Combine Data #############################

umbel <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_UMBEL_Background_Noise.csv")
fwpr7 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR7_Background_Noise.csv")
fwpr6 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR6_Background_Noise.csv")
fwpr5 <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/Background_Noise_Bandpass/2023_FWPR5_Background_Noise.csv")
# Combine them
all_dat <- rbind(umbel,fwpr7,fwpr6,fwpr5) %>% clean_names()
# Remove the filler headers from combining the datasheets
all_dat <- all_dat %>% filter(!file_name == "File_Name")
# Split the name into a site name
#all_dat <- all_dat %>% separate(file_name, into = c("site_id","point_dt"), sep = "-", remove = FALSE) %>% select(-point_dt)
all_dat <- all_dat %>% separate(file_name, into = c("point_id","date2","time2"), sep = "_", remove = FALSE) %>% select(-date2,-time2)
all_dat <- all_dat %>% create_site_col()
# Format the date column
all_dat$date <- as.Date(all_dat$date, format = "%m%d%Y")
# change the average db to a numeric
all_dat$avg_db <- as.numeric(all_dat$average_d_b)

# Define your survey periods
survey_periods <- list(
  "2023-06-01_2023-06-14" = c(as.Date("2023-06-01"), as.Date("2023-06-14")),
  "2023-06-15_2023-06-28" = c(as.Date("2023-06-15"), as.Date("2023-06-28")),
  "2023-06-29_2023-07-12" = c(as.Date("2023-06-29"), as.Date("2023-07-12")),
  "2023-07-13_2023-07-26" = c(as.Date("2023-07-13"), as.Date("2023-07-26")),
  "2023-07-27_2023-08-09" = c(as.Date("2023-07-27"), as.Date("2023-08-09")),
  "2023-08-10_2023-08-15" = c(as.Date("2023-08-10"), as.Date("2023-08-15"))
)

# Create a function to classify dates into periods
classify_period <- function(date, periods) {
  for (period in names(periods)) {
    if (date >= periods[[period]][1] & date <= periods[[period]][2]) {
      return(period)
    }
  }
  return(NA)
}

# Apply the function to create a new column for the period
all_dat$period <- sapply(all_dat$date, classify_period, periods = survey_periods)

# Filter out only the rows that fall within the survey period 
dat_surveyper <- all_dat %>% filter(is.na(period) == FALSE)

# Group by site_id and period, then calculate the average decibel readings
summary_dat <- dat_surveyper %>%
  group_by(site_id, period) %>%
  summarise(avg_db = round(mean(avg_db, na.rm = TRUE),2)) %>%
  ungroup()

# Spread the data to wide format
final_dat <- summary_dat %>%
  pivot_wider(names_from = period, values_from = avg_db)

#write the output
#write.csv(final_dat, "C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Detection_Covariates/BackgroundNoiseBandpass_2023_ARUPoints.csv" ,row.names = FALSE)


