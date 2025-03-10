#### Combine Veg Survey and Plant Spp ###################

## Purpose: to read in the Survey123 data files from the veg data and combine them with the plant data 

# Created 4/23/2023

# Last modified: 4/23/2024


#### Setup #################################
packages <- c("tidyverse","janitor")
source("./Function_Scripts/Install_Load_Packages.R")
load_packages(packages)


##################### Main Data Sheet #############################
# read in current veg data
veg_parent <- read.csv("./Data_Wrangling_Tidying/Vegetation_Data/Data/2023_VegSurveyData_Cleaned4-23.csv")
veg <- veg %>% rename(parent_global_id = global_id)
veg %>% filter(point_id == "JDO-3")

# Read in the plant data for cleaning
tree <- read.csv("./Data_Wrangling_Tidying/Vegetation_Data/Data/Trees_1.csv") %>% clean_names()
# Clean up columns and names
tree <- tree %>% select(-c(creator,editor,to_add_another_tree_species,specify_other,object_id,for_each_tree_species_choose_the_most_dominant_in_the_plot_up_to_4))
tree <- tree %>% rename(child_global_id = global_id)
# Combine with parent
veg_tree <- left_join(veg,tree,by = "parent_global_id")
# Summarize across point ID
detection_trees <- veg_tree %>% group_by(point_id, sampling_design) %>% summarize(total_trees_s = sum(trees_8_23_cm_dbh),
                                                                                  total_trees_m = sum(trees_23_38_cm_dbh),
                                                                                  total_trees_l = sum(trees_38_cm_dbh))
# Sum the columns
detection_trees$total_trees <- rowSums(detection_trees[,c("total_trees_s","total_trees_m","total_trees_l")], na.rm = TRUE)
# Take the total number of trees and divide it by the maximum number of trees to give a metric of relative density of the plot
detection_trees <- detection_trees %>% mutate(scaled_treedense = round((total_trees/max(detection_trees$total_trees)),2))
# Filter out only the columns you need
detection_trees_fin <- detection_trees %>% select(point_id,sampling_design,scaled_treedense)


# Read in the shrub data
shrub <- read.csv("./Data_Wrangling_Tidying/Vegetation_Data/Data/ShrubCover_2.csv") %>% clean_names()
# Clean up columns and names
shrub <- shrub %>% select(-c(object_id,for_each_shrub_species_choose_the_most_dominant_in_the_plot_up_to_8,to_add_another_shrub_species, creator, editor))
shrub <- shrub %>% rename(child_global_id = global_id,percent_cover = x_cover)
# Combine with parent
veg_shrub <- left_join(veg,shrub,by = "parent_global_id")
# Filter out just shrub layers taller than 1 m that will affect detection
detection_shrubs <- veg_shrub %>% filter(shrub_height_m >= 1)
# Group by point id and summarize the percent of tall shrub cover 
detection_shrubs <- detection_shrubs %>% group_by(point_id, sampling_design) %>% summarize(percent_tallshrubcov_summed = round(sum((percent_cover)/100),2))
# Scale the percent cover to the maximum shrub cover create a relative density measurement to put it on the same scale as the tree data
detection_shrubs <- detection_shrubs %>% mutate(scaled_tallshrubdense = round((percent_tallshrubcov_summed/max(detection_shrubs$percent_tallshrubcov_summed)),2))
# filter out only the columns you need
detection_shrubs_fin <- detection_shrubs %>% select(point_id, scaled_tallshrubdense)


# Combine the data
veg_dense <- left_join(detection_trees_fin,detection_shrubs_fin, by = "point_id")
# replace NAs in shrub density with 0
veg_dense <- veg_dense %>% mutate(scaled_tallshrubdense = ifelse(is.na(scaled_tallshrubdense)==FALSE, scaled_tallshrubdense,0))
veg_dense$composite_dense <- rowSums(veg_dense[,c("scaled_treedense","scaled_tallshrubdense")], na.rm = TRUE)

clips_23 <- read.csv("./Data_Wrangling_Tidying/PAM_Detection_History/2023_AllCollab_topclips_filteredPBYBCU_4-24.csv")
points_aru <- unique(clips_23$point_id)
# extract the point ids where arus were present
#aru_sites <- veg %>% filter(aru_present == "yes") %>% select(point_id)
# filter the vegetation characteristics through this
veg_dense_fin <- veg_dense %>% filter(point_id %in% points_aru)
# Checking which one is missing
aru_dat_noveg <- setdiff(points_aru,veg_dense$point_id)
# MISO-197 is missing - this one doesn't have a veg survey because we weren't able to get the monitor
# Select the final columns you'll be using
veg_dense_fin <- veg_dense_fin %>% select(point_id, sampling_design, composite_dense)

# write this for use in the detection model 
#write.csv(veg_dense_fin, "./Data_Wrangling_Tidying/Vegetation_Data/Data/ShrubTreeDensityComposite_2023_ARUPoints.csv",row.names = FALSE)
