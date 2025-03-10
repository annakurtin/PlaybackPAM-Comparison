#Function to create a site column dependent on point_id column


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

create_samp_col <- function(datasheet){
  datasheet_new <- datasheet %>%
  mutate(sampling_design = case_when(
    # If point_id has four letters then a dash then three numbers
    grepl("^[[:alpha:]]{4}-[[:digit:]]{3}$", point_id) ~ "habitat_grts",
    # If point_id has two numbers, three numbers, or three letters then a dash then one number
    grepl("^[[:digit:]]{2,3}-[[:digit:]]{1}$", point_id) ~ "mmr_grts",
    grepl("^[[:alpha:]]{3}-[[:digit:]]{1}$", point_id) ~ "nonrand"
  ))
  return(datasheet_new)
}