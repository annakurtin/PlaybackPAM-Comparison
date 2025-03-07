# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Combine Detection Covariates ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# This is a script made from the code from Detection Model 1 to combine together the detection covariates
# only combining detection data from 2023

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Setup ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyverse)
library(janitor)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Combine and Clean Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site-level covariates
# Read in vegetation density for each site
vegdense <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Detection_Covariates/ShrubTreeDensityComposite_2023_ARUPoints.csv")
vegdense <- vegdense %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# take the average of this across the site
vegdense_avg <- vegdense %>% group_by(site_id,sampling_design) %>% summarize(veg_density_avg = round(mean(composite_dense,na.rm = TRUE),2))


# background noise
background_db <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Habitat_Model_Covariates/Detection_Covariates/BackgroundNoiseBandpass_2023_ARUPoints.csv")
colnames(background_db) <- c("site_id","backdb_survey1","backdb_survey2","backdb_survey3","backdb_survey4","backdb_survey5","backdb_survey6")


# Read in detection and effort data 
effort <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Detection_History/2023_All_ARUs/Outputs/DetectionHist_CamTrapR/bbcu__effort__not_scaled_14_days_per_occasion__2024-04-29.csv") %>% clean_names
survey_periods <- colnames(effort)
colnames(effort) <- c("site_session","effort_survey1","effort_survey2","effort_survey3","effort_survey4","effort_survey5","effort_survey6")
effort <- effort %>% separate(site_session, into = c("site_id","session_name"), sep = "__") %>% select(-session_name)


arutype <- read.csv("./Data/Habitat_Model_Covariates/Detection_Covariates/ARUtype_22-23.csv")
# separate out 2023 data
arutype <- arutype %>% filter(year == "2023") %>% select(-year)


detections_orig <- read.csv("C:/Users/annak/OneDrive/Documents/UM/Research/Coding_Workspace/Cuckoo-Research/Data/Detection_History/2023_All_ARUs/Outputs/DetectionHist_CamTrapR/bbcu__detection_history__with_effort__14_days_per_occasion__2024-04-29.csv") %>% clean_names
detections <- detections_orig
colnames(detections) <- c("site_session","det_survey1","det_survey2","det_survey3","det_survey4","det_survey5","det_survey6")
# Split apart the detections into site and point
detections <- detections %>% separate(site_session, into = c("site_id","session_name"), sep = "__")


# extract the  dates
survey_startdate <- colnames(detections_orig)
# Remove the value with just "x", remove "x" from the rest, reformat dates
surveys_cleaned <- survey_startdate[-1] %>% 
  gsub("^x", "", .) %>%            # Remove the leading "x"
  gsub("_", "-", .)                # Replace underscores with hyphens
# Convert to a dataframe
surveys_df <- data.frame(survey_period = surveys_cleaned)
# Split the survey_period into start_date and end_date 
surveys_df <- surveys_df %>%
  separate(survey_period, into = c("start_date", "end_date"),sep = "(?<=\\d{4}-\\d{2}-\\d{2})-")
# Convert it to date
surveys_df <- surveys_df %>% mutate(start_date = as.Date(start_date, format = "%Y-%m-%d"),
                                    end_date = as.Date(end_date, format = "%Y-%m-%d"))
# Convert to Julian Date
surveys_df <- surveys_df %>% mutate(start_julian = yday(start_date),
                                    end_julian = yday(end_date))
# assign this to dates
dates <- detections %>% select(site_id)
dates$start_date_s1 <- surveys_df[1,3] # first row of third column for julian date
dates$start_date_s2 <- surveys_df[2,3] 
dates$start_date_s3 <- surveys_df[3,3] 
dates$start_date_s4 <- surveys_df[4,3] 
dates$start_date_s5 <- surveys_df[5,3] 
dates$start_date_s6 <- surveys_df[6,3] 


# # Combine them
full_dat <- left_join(detections,background_db, by = "site_id") %>% 
  left_join(vegdense_avg, by = "site_id") %>% 
  left_join(effort, by = "site_id") %>% 
  left_join(arutype, by = "site_id") %>%
  left_join(dates, by = "site_id")

full_dat <- full_dat %>% select(-c(session_name, sampling_design))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Save Unscaled Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#saveRDS(full_dat, file ="./Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")
#test <- readRDS("./Data/Habitat_Model_Covariates/Full_DetectionData_JAGSModels_HabChap.Rdata")




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Center and Scale Variables ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Background dB
background_db <- full_dat[,c(1,8:13)]
db_long <- background_db %>% pivot_longer(cols = c(backdb_survey1,
                                                   backdb_survey2,
                                                   backdb_survey3,
                                                   backdb_survey4,
                                                   backdb_survey5,
                                                   backdb_survey6), 
                                          names_to = "survey_period",values_to = "db" )
# Scale it
db_long$db_scaled <- scale(db_long$db, center = TRUE)[,1]
# reformat to combine with other columns
db_fin <- db_long %>% select(-db) %>% pivot_wider(names_from = survey_period, values_from = db_scaled)


# Veg Density 
veg_fin <- full_dat %>% select(site_id, veg_density_avg)
veg_fin$veg_density_avg_scaled <- scale(full_dat$veg_density_avg, center = TRUE)[,1]
veg_fin <- veg_fin %>% select(-veg_density_avg)


# Effort 
effort <- full_dat[,c(1,15:20)]
effort_long <- effort %>% pivot_longer(cols = c(effort_survey1,
                                                effort_survey2,
                                                effort_survey3,
                                                effort_survey4,
                                                effort_survey5,effort_survey6), 
                                       names_to = "survey_period",values_to = "effort" )
# Scale it
effort_long$effort_scaled <- scale(effort_long$effort, center = TRUE)[,1]
# reformat
effort_fin <- effort_long %>% select(-effort) %>% pivot_wider(names_from = survey_period, values_from = effort_scaled)


# Date
date <- full_dat[,c(1,23:28)]
date_long <- date %>% pivot_longer(cols = c(start_date_s1,
                                            start_date_s2,
                                            start_date_s3,
                                            start_date_s4,
                                            start_date_s5,
                                            start_date_s6), 
                                   names_to = "survey_period",values_to = "date_col")
# Scale the start date
date_long$date_scaled <- scale(date_long$date_col, center = TRUE)[,1]
# Reformat
date_fin <- date_long %>% select(-date_col) %>% pivot_wider(names_from = survey_period, values_from = date_scaled)

# # Combine them
full_dat_s <- left_join(detections,db_fin, by = "site_id") %>% 
  left_join(veg_fin, by = "site_id") %>% 
  left_join(effort_fin, by = "site_id") %>% 
  left_join(date_fin, by = "site_id") %>% 
  left_join(arutype, by = "site_id")
full_dat_s <- full_dat_s %>% select(-session_name)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Save Scaled Data ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#saveRDS(full_dat_s, file = "./Data/Habitat_Model_Covariates/Full_DetectionDataSCALED_JAGSModels_HabChap.Rdata") 



# scaled_orig <- readRDS("./Data/Habitat_Model_Covariates/Archive/Full_DetectionDataSCALED_JAGSModels_HabChap_noARUtype.Rdata")
#test_s <- readRDS("./Data/Habitat_Model_Covariates/Full_DetectionDataSCALED_JAGSModels_HabChap.Rdata")

