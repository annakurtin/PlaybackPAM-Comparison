#### Create Detection History from 14 day CamTrapR Periods ####

library(tidyverse)

detections <- read.csv("./Data_Wrangling_Tidying/PAM_Detection_History/Data/bbcu__detection_history__with_effort__14_days_per_occasion__2024-04-29.csv") %>% clean_names
colnames(detections) <- c("site_session","det_survey1","det_survey2","det_survey3","det_survey4","det_survey5","det_survey6")

# Split apart the detections into site and point
detections <- detections %>% separate(site_session, into = c("site_id","session_name"), sep = "__") %>% select(-session_name)

write.csv(detections, "Data_Wrangling_Tidying/PAM_Detection_History/Data/SiteDetHist_TwoWeekSurvey_Cleaned_7-24.csv", row.names = FALSE)
