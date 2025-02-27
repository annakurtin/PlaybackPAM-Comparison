#### Create Detection Histories for Data ###################

## Purpose: to create detection histories with camtrap R

# Created 4/26/2023

# Last modified: 4/26/2024

# Left off: was able to make a datasheet for the operation periods and detection history
## This package is made for camera traps, so I'm not sure how well the effort metric applies to our discrete sampling periods. Could I just change the date for the camera operation table to be a quarter of each day? this would be every six hours so I would change 1 am to 00:00, 7 am to 6:00, 9 am to 12:00, and 11 pm to 18:00??? how do I make this work??
## Could I just make a detection effort period table on my own?
## I think I got this figured out, next I just need to go through and make sure it's doing what I think it's doing


#### Setup #################################
packages <- c("tidyverse","janitor","camtrapR")
source("./R_Scripts/6_Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Main Data Sheet #############################
# Number of detection periods to consider: 2 week longest (14 days), 3 days shortest, 7 days intermediate

# Read in the data that does mask recording periods to clips
# ARUtable <- read.csv("./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_4-26.csv")
# ARUtable <- ARUtable %>% filter(!(point_id == "PRD-2"))
# ARUtable <- ARUtable %>% mutate(first_rec_dt = as_datetime(first_rec,tz = "America/Denver"))
# ARUtable <- ARUtable %>% mutate(last_rec_dt = as_datetime(last_rec,tz = "America/Denver"))

# Read in the data that doesn't mask the recording periods to the start and end clips
#ARUtable2 <- read.csv("./Data/Detection_History/2023_All_ARUs/2023_DeployPeriodTable_CorrectedEffort_NotClipMasked_4-29.csv")
ARUtable2 <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")
# filter out PRD-2 because the data starts and stops on the same day
ARUtable2 <- ARUtable2 %>% filter(!(point_id == "PRD-2"))
#ARUtable2 <- ARUtable2 %>% mutate(first_rec_dt = as_datetime(first_rec,tz = "America/Denver"))
#ARUtable2 <- ARUtable2 %>% mutate(last_rec_dt = as_datetime(last_rec,tz = "America/Denver"))


op_table <- cameraOperation(CTtable = ARUtable2, 
                            # Used station column to indicate the grid cell
                            stationCol = "site_id", 
                            cameraCol = "point_id", 
                            # Used session column to indicate the season
                            sessionCol = "year",
                            setupCol = "first_rec", 
                            retrievalCol = "last_rec",
                            # Indicates that there are problem columns:
                            hasProblems = TRUE, 
                            # byCamera = FALSE allows for lumping cameras by 
                            # 'station', in this case, grid
                            byCamera = FALSE, 
                            # allCamsOn = FALSE, means that not all cameras 
                            # need to be active for the station/grid cell to 
                            # be considered active
                            allCamsOn = FALSE,
                            # camerasIndependent = TRUE allows matrix to 
                            # return the number of operational cameras at 
                            # the station/grid cell on each encounter period
                            camerasIndependent = TRUE, 
                            dateFormat = "%Y-%m-%d %H:%M:%S", ### PICK UP HERE
                            #timeFormat = "%H:%M:%S",
                            occasionStartTime = 0,
                            writecsv = FALSE, # whether or not to save the outputted camera operation table for later reading
                            outDir = getwd()) # where to save it



# Read in detection data
#detection_data <- read.csv("./Data/Detection_History/2023_All_ARUs/2023_BBCUDetections.csv")
detection_data <- readRDS("./Data/Detection_History/2023_All_ARUs/Outputs/2023_BBCUDetections.RData")

# Create detection history data for 3 day sampling period
dethist_surveyperiod <- detectionHistory(recordTable = detection_data,
                            species = "bbcu",
                            camOp = op_table,
                            output = "binary",
                            stationCol = "site_id",
                            speciesCol = "species",
                            recordDateTimeCol = "datetime",
                            recordDateTimeFormat = "%Y-%m-%d %H:%M:%S",
                            occasionLength = 14, # how many days per encounter period?
                            # minActiveDaysPerOccasion,
                            # maxNumberDays,
                            day1 = "2023-06-01", # specify the start date of the survey period
                            # buffer,
                            includeEffort = TRUE,
                            scaleEffort = FALSE, # I scaled everything later and wanted to have the unscaled amount too
                            # occasionStartTime = "deprecated",
                            datesAsOccasionNames = TRUE, # this way I knew which days each encounter period lined up with
                            timeZone = "UTC", 
                            writecsv = TRUE,# save csvs with the outputted tables for recalling later
                            outDir = "./Data/Detection_History/2023_All_ARUs", # where to save
                            unmarkedMultFrameInput = FALSE) # not doing a multiseason so left as false
# for a stacked model you have to condense this across years - have to make a site_year column and then change the datetime to the same year just keep that in mind when working with the data (set this to a fake year)
