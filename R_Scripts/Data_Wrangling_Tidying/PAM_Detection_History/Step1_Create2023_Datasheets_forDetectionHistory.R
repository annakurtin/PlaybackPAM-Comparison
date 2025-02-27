#### Create Detection Histories for Data ###################

## Purpose: to create datasheets to read into camtrapR

# Created 4/26/2023

# Last modified: 4/26/2024

#### Setup #################################
packages <- c("tidyverse","janitor","camtrapR")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
source("./R_Scripts/6_Function_Scripts/Combine_CSV_Files_In_Directory.R")
load_packages(packages)

### Create functions ######
create_site_col <- function(datasheet){
  datasheet_new <- datasheet %>%
    mutate(site_id = case_when(
      # If point_id has four letters then a dash then three numbers
      grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
      # If point_id has two numbers, three numbers, or three letters then a dash then one number
      grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
        grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
    ))
  return(datasheet_new)
}
##### Create start and end columns ####
# In the camtrapR package, these are the deployment and retrieval dates. Since we deployed them before we started to analyze the audio, we'll be masking out the periods where the ARU was deployed and recording but not a part of the study period. I do this by reading in the recording data and taking the first and last date that appear in the clips audio

# Read in the problematic periods file
prob_period <- read.csv("./Data/Detection_History/2023_All_ARUs/Raw_Data/List_Problematic_ARU_Files_2023_Start_Stop_ofIssues.csv") %>% select(-c(year, Problem2_from, Problem2_to))
prob_period <- prob_period %>% mutate(Problem1_from = as_datetime(Problem1_from,tz = "America/Denver"),
                                      Problem1_to = as_datetime(Problem1_to,tz = "America/Denver"))
# Change the datetime
prob_period <- prob_period %>% mutate(Problem1_from = case_when(
  hour(Problem1_from) == 1 ~ as.POSIXct(paste0(format(Problem1_from, "%Y-%m-%d"), " 00:00:00")),
  hour(Problem1_from) == 7 ~ as.POSIXct(paste0(format(Problem1_from, "%Y-%m-%d"), " 06:00:00")),
  hour(Problem1_from) == 9 ~ as.POSIXct(paste0(format(Problem1_from, "%Y-%m-%d"), " 12:00:00")),
  hour(Problem1_from) == 23 ~ as.POSIXct(paste0(format(Problem1_from, "%Y-%m-%d"), " 18:00:00"))))
prob_period <- prob_period %>% mutate(Problem1_to = case_when(
  hour(Problem1_to) == 1 ~ as.POSIXct(paste0(format(Problem1_to, "%Y-%m-%d"), " 00:00:00")),
  hour(Problem1_to) == 7 ~ as.POSIXct(paste0(format(Problem1_to, "%Y-%m-%d"), " 06:00:00")),
  hour(Problem1_to) == 9 ~ as.POSIXct(paste0(format(Problem1_to, "%Y-%m-%d"), " 12:00:00")),
  hour(Problem1_to) == 23 ~ as.POSIXct(paste0(format(Problem1_to, "%Y-%m-%d"), " 18:00:00")))) 



# get audio quality data
recper_23 <- read.csv("./Data/PAM/2023_AllCollabs_AcousticFiles.csv") 
recper_23_used <- recper_23 %>% filter(correct_size == "Y")
unique(recper_23_used$time) # This still has the MISO-032 points in it
# Clean this up a bit
recper_23_used <- recper_23_used %>% separate(file_name, into = c("point_id","date2","time2"), sep = "_", remove = FALSE) %>% select(-c(date2,time2))
# some of the datetimes aren't formatted right?
# 102-5 and MISO-032, MISO-204 formatted 5/26/2023 1:00
# split apart these, format them separately, then recombine
format1 <- recper_23_used %>% filter(point_id %in% c("102-5","MISO-032","MISO-204"))
#format1 <- format1 %>% mutate(datetime = as.POSIXct(datetime, format = "%m/%d/%Y %H:%M")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) 
format1$date <- str_extract(format1$file_name, "([[:digit:]]{8})")
format2 <- recper_23_used %>% filter(!(point_id %in% c("102-5","MISO-032","MISO-204")))
#format2 <- format2 %>% mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%d %H:%M:%S")) %>%  mutate(datetime = as_datetime(datetime,  tz = "America/Denver"))
format2$date <- str_extract(format2$file_name, "([[:digit:]]{8})")
recper_23_format <- rbind(format1,format2)
#unique(recper_23_format$point_id) looks good 
recper_23_format <- recper_23_format %>% unite(col= point_date, c("point_id","date"), sep = "_", remove = FALSE)
# Change the times in a new column so that the camera trap sampling will evenly account for effort
recper_23_format <- recper_23_format %>% mutate(time_effort = case_when(
  time == 10000 ~ "00:00:00",
  time == 70000 ~ "06:00:00",
  time == 90000 ~ "12:00:00",
  time == 230000 ~ "18:00:00"
))
# make a new datetime column for the effort time
#recper_23_format$time_colon <- sub("(\\d{1,2})(\\d{2})(\\d{2})", "\\1:\\2:\\3", recper_23_format$time_effort)
recper_23_format$combo_effort <- paste0(recper_23_format$date,"-", recper_23_format$time_effort)
recper_23_format$datetime_effort <- as.POSIXct(recper_23_format$combo_effort, format = "%Y%m%d-%H:%M:%S")

# Create a sheet that has the absolute first and last recording
#recsabsolute_first_last <- recper_23_format %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE),last_rec = max(datetime_effort, na.rm = TRUE))
# Pull out just the first date from the recording periods
recs_first <- recper_23_format %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE))
# Group by point ID
#recs_firstlast_abs <- create_site_col(recsabsolute_first_last)
#recs_firstlast_abs <- recs_firstlast_abs %>% mutate(year = 2023)
#deploy_table_1 <- left_join(recs_firstlast_abs,prob_period, by = "point_id")
# write this for use
#write.csv(deploy_table_1,"./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_NotClipMasked_4-29.csv",row.names = FALSE)
#saveRDS(deploy_table_1,file = "./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_NotClipMasked_4-29.RData")

# Mask another datasheet to clips data
# Read in the clips data - this represents all the days that were run through the classifier and are part of our study period
# The monitors were deployed before June 1st, this data we aren't counting
clips_23 <- read.csv("./Data/Classifier_Results/Model2.0/Outputs/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv") %>% mutate(datetime = as_datetime(datetime,  tz = "America/Denver")) %>% mutate(date_formatted = as.Date(date_formatted, format = "%Y-%m-%d"))
clips_23 <- clips_23 %>% unite(col= point_date, c("point_id","date"), sep = "_", remove = FALSE)
# Filter out only the files that were in the classifier runs
recs_withclips <- recper_23_format %>% filter(point_date %in% clips_23$point_date)
# Pull out the first and the last recording periods
#recs_first_last <- recs_withclips %>% group_by(point_id) %>% summarize(first_rec = min(datetime_effort, na.rm = TRUE),last_rec = max(datetime_effort, na.rm = TRUE))
# Pull out just the last day in the survey period 
recs_last <- recs_withclips %>% group_by(point_id) %>% summarize(last_rec = max(datetime_effort, na.rm = TRUE))
# Change the datetime to reflect the fact that we have a full day of data here
recs_last <- recs_last %>% 
  mutate(last_rec = case_when(
    hour(last_rec) == 18 & month(last_rec) == 8 & mday(last_rec) == 15 ~ {
      new_date <- as.Date("2023-08-16")
      as.POSIXct(paste(new_date, format(new_date, "%H:%M:%S"), " 00:00:00"))
    },
    TRUE ~ last_rec 
    ))
# need to separate out the date and time columns
#recs_first_last %>% mutate(date = as.Date(datetime), time = format(datetime, format = "%H:%M:%S"))
deployhist_wsite <- create_site_col(recs_first_last)
# Create a column for year
#deployhist_wsite <- deployhist_wsite %>% mutate(year = 2023)
#deploy_table_2 <- left_join(deployhist_wsite,prob_period, by = "point_id")

# Write this data for use in camtrapR #
#write.csv(deployhist_wsite,"./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_4-26.csv",row.names = FALSE)


# Join together the data you've pulled out
deploy_table3 <- left_join(recs_last,recs_first, by = "point_id")
#deploy_table3_test <- left_join(recs_first,recs_last, by = "point_id")
# add in the problem periods
deploy_table3_fin <- left_join(deploy_table3,prob_period, by = "point_id")
# Create a column for site ID and year
deploy_table3_fin <- create_site_col(deploy_table3_fin)
deploy_table3_fin <- deploy_table3_fin %>% mutate(year = 2023)
# save this
saveRDS(deploy_table3_fin,file = "./Data/Detection_History/2023_All_ARUs/Outputs/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")




###### Read in clips data from before and create a site ID column #######################
clips_wsite <- clips_23 %>%
  mutate(site_id = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ point_id,
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) |
      grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ gsub("-.*", "", point_id)
  ))
# Filter out just the detections and create a species column
clips_wsite <- clips_wsite %>% filter(annotation == 1) %>% mutate(species = case_when(annotation == 1 ~ 'bbcu'))
#unique(clips_wsite$species) # looks good

# write this to a csv for use
#write.csv(clips_wsite,"./Data/Detection_History/2023_All_ARUs/2023_BBCUDetections.csv", row.names = FALSE)

# Write this to an .rdata file for easier use
#saveRDS(clips_wsite,file = "./Data/Detection_History/2023_All_ARUs/2023_BBCUDetections.RData")
