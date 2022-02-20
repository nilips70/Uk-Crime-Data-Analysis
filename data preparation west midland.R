library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(lubridate)

rm(list = ls())

# reading the stop-and-search dataset for west midland 3yrs data (3 years)
temp_ss <- list.files(path = paste0(getwd(), "/west midland 3yrs data"), pattern = "*search.csv")
temp_ss <- paste0(getwd(), "/west midland 3yrs data/", temp_ss)
search <- lapply(temp_ss, read_csv)

# merging stop-and-search datasets vertically
df_search <- search %>% reduce(rbind)

# visualizing the NAs and data types
glimpse(df_search)
visdat::vis_dat(df_search)

# reading the street crime dataset for manchester (3 years)
temp_st <- list.files(path = paste0(getwd(), "/west midland 3yrs data"), pattern = "*street.csv")
temp_st <- paste0(getwd(), "/west midland 3yrs data/", temp_st)
street <- lapply(temp_st, read_csv)

df_street <- street %>% reduce(rbind)



# visualizing the NAs and data types
glimpse(df_street)
visdat::vis_dat(df_street)

################# DATA CLEANING #################
# removing na column and rows without lat/long and crime ID in street dataset
df_st <- df_street %>%
  filter(is.na(Longitude) == FALSE, is.na(Latitude) == FALSE, is.na(`Crime ID`) == FALSE) %>%
  select(-Context)


visdat::vis_dat(df_st)


df_st <- df_st %>% rename(
  crime_id = `Crime ID`,
  crime_type = `Crime type`,
  reported_by = `Reported by`,
  falls_within = `Falls within`,
  lsoa_code = `LSOA code`,
  lsoa_name = `LSOA name`,
  last_outcome_category = `Last outcome category`
)

df_st$crime_type <- as.factor(df_st$crime_type)

# saveRDS(df_st, "west_midland_st.rds")

# removing NA columns and rows with no lat/long stop and search dataset
df_ss <- df_search %>%
  select(-c("Part of a policing operation", "Policing operation")) %>%
  filter(is.na(Latitude) == FALSE, is.na(Longitude) == FALSE)


visdat::vis_dat(df_ss)


df_ss <- df_ss %>% rename(
  age = `Age range`,
  self_defined_ethnicity = `Self-defined ethnicity`,
  officer_defined_ethnicity = `Officer-defined ethnicity`,
  object_of_search = `Object of search`,
  outcome_linked_to_object_of_search = `Outcome linked to object of search`,
  removal_of_more_than_just_outer_clothing = `Removal of more than just outer clothing`
)

# saveRDS(df_ss, "west_midland_ss.rds")