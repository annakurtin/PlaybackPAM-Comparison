#### Create Detection Histories for Data ###################

## Purpose: to create detection histories with camtrap R

# Created 4/26/2023

# Last modified: 4/26/2024


#### Setup #################################
packages <- c("tidyverse","janitor","camtrapR")
source("./Function_Scripts/Install_Load_Packages.R")
load_packages(packages)

#### Main Data Sheet #############################
# Number of detection periods to consider: 2 week longest (14 days), 3 days shortest, 7 days intermediate

# Read in the data that doesn't mask the recording periods to the start and end clips
ARUtable2 <- readRDS("./Data_Wrangling_Tidying/PAM_Detection_History/Data/2023_DeployPeriodTable_CorrectedEffort_ClipMaskedFinalRec_4-29.RData")
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
detection_data <- readRDS("./Data_Wrangling_Tidying/PAM_Detection_History/Data/2023_BBCUDetections.RData")

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
                            outDir = "./Data_Wrangling_Tidying/PAM_Detection_History/Data", # where to save
                            unmarkedMultFrameInput = FALSE) # not doing a multiseason so left as false
