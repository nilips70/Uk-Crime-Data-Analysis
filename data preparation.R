library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(lubridate)

rm(list = ls())

##############################################################################################
#################     Data preparation for 1 year (nov 2020 - nov 2021)     #################
##############################################################################################

# reading the stop-and-search dataset
ss_nov20 <- read_csv("data/2020-11/2020-11-city-of-london-stop-and-search.csv")
ss_dec20 <- read_csv("data/2020-12/2020-12-city-of-london-stop-and-search.csv")
ss_jan21 <- read_csv("data/2021-01/2021-01-city-of-london-stop-and-search.csv")
ss_feb21 <- read_csv("data/2021-02/2021-02-city-of-london-stop-and-search.csv")
ss_march21 <- read_csv("data/2021-03/2021-03-city-of-london-stop-and-search.csv")
ss_apr21 <- read_csv("data/2021-04/2021-04-city-of-london-stop-and-search.csv")
ss_may21 <- read_csv("data/2021-05/2021-05-city-of-london-stop-and-search.csv")
ss_jun21 <- read_csv("data/2021-06/2021-06-city-of-london-stop-and-search.csv")
ss_jul21 <- read_csv("data/2021-07/2021-07-city-of-london-stop-and-search.csv")
ss_aug21 <- read_csv("data/2021-08/2021-08-city-of-london-stop-and-search.csv")
ss_sep21 <- read_csv("data/2021-09/2021-09-city-of-london-stop-and-search.csv")
ss_oct21 <- read_csv("data/2021-10/2021-10-city-of-london-stop-and-search.csv")
ss_nov21 <- read_csv("data/2021-11/2021-11-city-of-london-stop-and-search.csv")

# merging stop-and-search datasets vertically
london_ss <- bind_rows(
  ss_nov20,
  ss_dec20,
  ss_jan21,
  ss_feb21,
  ss_march21,
  ss_apr21,
  ss_may21,
  ss_jun21,
  ss_jul21,
  ss_aug21,
  ss_sep21,
  ss_oct21,
  ss_nov21
)
# saveRDS(london_ss, "london_ss.rds")


# visualizing the NAs and data types
glimpse(london_ss)
visdat::vis_dat(london_ss)



# reading the street crime dataset
st_nov20 <- read_csv("data/2020-11/2020-11-city-of-london-street.csv")
st_dec20 <- read_csv("data/2020-12/2020-12-city-of-london-street.csv")
st_jan21 <- read_csv("data/2021-01/2021-01-city-of-london-street.csv")
st_feb21 <- read_csv("data/2021-02/2021-02-city-of-london-street.csv")
st_march21 <- read_csv("data/2021-03/2021-03-city-of-london-street.csv")
st_apr21 <- read_csv("data/2021-04/2021-04-city-of-london-street.csv")
st_may21 <- read_csv("data/2021-05/2021-05-city-of-london-street.csv")
st_jun21 <- read_csv("data/2021-06/2021-06-city-of-london-street.csv")
st_jul21 <- read_csv("data/2021-07/2021-07-city-of-london-street.csv")
st_aug21 <- read_csv("data/2021-08/2021-08-city-of-london-street.csv")
st_sep21 <- read_csv("data/2021-09/2021-09-city-of-london-street.csv")
st_oct21 <- read_csv("data/2021-10/2021-10-city-of-london-street.csv")
st_nov21 <- read_csv("data/2021-11/2021-11-city-of-london-street.csv")

# merging stop-and-search datasets vertically
london_st <- bind_rows(
  st_nov20,
  st_dec20,
  st_jan21,
  st_feb21,
  st_march21,
  st_apr21,
  st_may21,
  st_jun21,
  st_jul21,
  st_aug21,
  st_sep21,
  st_oct21,
  st_nov21
)
# saveRDS(london_st, "london_st.rds")


# visualizing the NAs and data types
glimpse(london_st)
summary(london_st)
visdat::vis_dat(london_st)


################# DATA CLEANING #################
# removing na column and rows without lat/long and crime ID in london_st.RDS
df_st <- london_st %>%
  filter(is.na(Longitude) == FALSE, is.na(Latitude) == FALSE, is.na(`Crime ID`) == FALSE) %>%
  select(-Context)

visdat::vis_dat(df_st)

# removing NA columns and rows with no lat/long in london_ss.RDS
df_ss <- london_ss %>%
  select(-c("Part of a policing operation", "Policing operation")) %>%
  filter(is.na(Latitude) == FALSE, is.na(Longitude) == FALSE)

visdat::vis_dat(df_ss)

################# DATA MERGING #################
df_merged <- list(
  df_st,
  df_ss
) %>%
  reduce(left_join)

summary(df_merged)
# saveRDS(df_merged, "london_crimes.rds")



##############################################################################################
#################     Data preparation for 3 years (dec 2018 - nov 2021)     #################
##############################################################################################

# reading the stop-and-search dataset for london (3 years)
temp_ss <- list.files(path = paste0(getwd(), "/london 3yrs data"), pattern = "*search.csv")
temp_ss <- paste0(getwd(), "/london 3yrs data/", temp_ss)
search <- lapply(temp_ss, read_csv)

# merging stop-and-search datasets vertically
df_search <- search %>% reduce(rbind)


# visualizing the NAs and data types
glimpse(df_search)
visdat::vis_dat(df_search)

# reading the street crime dataset for london (3 years)
temp_st <- list.files(path = paste0(getwd(), "/london 3yrs data"), pattern = "*street.csv")
temp_st <- paste0(getwd(), "/london 3yrs data/", temp_st)
street <- lapply(temp_st, read_csv)

df_street <- street %>% reduce(rbind)



# visualizing the NAs and data types
glimpse(df_street)
visdat::vis_dat(df_street)


################# DATA CLEANING #################
# removing na column and rows without lat/long and crime ID in london_st.RDS
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

# saveRDS(df_st, "df_st_london.rds")

# removing NA columns and rows with no lat/long in london_ss.RDS
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

# saveRDS(df_ss, "df_ss.rds")

################# DATA MERGING by latitude and longitude #################
df_merged <- left_join(df_st, df_ss)

summary(df_merged)

################# ################# ################# ################# #################
##################################        OTHER DATASETS      ##################################
# reading LSAO shapefile and prepaaration
lsoa_london <- readOGR(dsn = "ESRI", layer = "LSOA_2011_London_gen_MHW")

# transforming to long & lat
lsoa_london <- spTransform(lsoa_london, CRS("+proj=longlat +datum=WGS84"))

# converting into a sf object
lsoa_london <- st_as_sf(lsoa_london)

lsoa_london <- lsoa_london %>% rename(lsoa_code = LSOA11CD)



# reading income dataset
london_others_dataset <- read_excel("london_others dataset.xlsx")

london_others_dataset <- london_others_dataset %>% rename(lsoa_code = Codes)



# merging every data with GEOMETRY and LONDON STREET data
df_others <- left_join(london_others_dataset, lsoa_london)

df_others <- df_others %>% select(
  lsoa_code, LSOA11NM, "mean_annual_income_household",
  "median_annual_income_household", "households with no adults in employment with children rate",
  "All lone parent housholds with dependent children", "Lone parent not in employment rate",
  "Unemployment Rate", "No qualifications rate",
  "Average Score of PTAL", "population density (persons per hectar)",
  "age 0 to 15", "age 16to 29",
  "age 30 to 44", "age 45 to 64",
  "age 65>", "White rate",
  "Mixed ethnic groups rate", "Asian_Asian British rate",
  "Black_ African_Caribbean_Black British rate", "Other ethnic group rate",
  "BAME rate", "country of birth uk rate",
  "country of birth Not uk rate", "median price of house",  geometry
)



df_st_london <- readRDS("df_st_london.rds")
df_london_crime_census <- left_join(df_st_london, df_others)

# saveRDS(df_london_crime_census, "london_others.rds")

london_others <- readRDS("london_others.rds")

hist(london_others$`Unemployment Rate`)

london_others <- london_others %>% mutate(unemp = ifelse(`Unemployment Rate`< 2.7, "2.7 or below",
                                                         (ifelse(`Unemployment Rate` > 2.7 & `Unemployment Rate` <= 4.5, "2.7 - 4.5",
                                                                 ifelse(`Unemployment Rate` > 4.5 & `Unemployment Rate` <= 10, "4.5 - 10",
                                                                        "10 or above")))))
                                                         
hist(london_others$'No qualifications rate')                                                        
london_others <- london_others %>%  mutate(qualification = ifelse(`No qualifications rate` < 2, "2 or below",
                                                                  (ifelse(`No qualifications rate` > 2 & `No qualifications rate` <= 7, "2 - 7",
                                                                          (ifelse(`No qualifications rate` > 7 & `No qualifications rate` <= 20, "7 - 20",
                                                                                  "20 or above"))))))

hist(london_others$`Average Score of PTAL`)
london_others <- london_others %>% mutate(ptal = ifelse(`Average Score of PTAL`<= 7.8, "7.8 or below",
                                                        ifelse(`Average Score of PTAL`>7.8 & `Average Score of PTAL` <= 7.93, "7.8 - 7.93",
                                                               ifelse(`Average Score of PTAL`> 7.93 & `Average Score of PTAL` <= 7.98, "7.93 - 7.98",
                                                                      "7.98 or above"))))
# saveRDS(london_others, "london_others.rds")


