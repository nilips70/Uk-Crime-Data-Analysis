library(tidyverse)
library(readxl)
library(sf)
library(rgdal)
library(spdplyr)
library(tigris)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(lubridate)
rm(list=ls())

#reading the dataset
london_ss <- readRDS("london_ss.rds")
london_st <- readRDS("london_st.rds")
london_income <- readRDS("london_income.rds")
df_st <- readRDS("df_st.rds")
df_ss <- readRDS("df_ss.rds")

#################     LONDON ST     #################     
#################     data exploring / preparation    #################
unique(london_st$reported_by)
unique(london_st$falls_within)
unique(london_st$Location)
unique(london_st$crime_type)
unique(london_st$last_outcome_category)
n_distinct(london_st$Month)
table(london_st$last_outcome_category)
length(unique(london_st$lsoa_name))
length(unique(london_st$lsoa_code))



criminal_damage <- london_st %>% filter(crime_type == "Criminal damage and arson")
weapon <- london_st %>%  filter(crime_type == "Possession of weapons")
bike_theft <- london_st %>%  filter(crime_type == "Bicycle theft")
other_theft <- london_st %>% filter(crime_type == "Other theft")
burglary <- london_st %>% filter(crime_type == "Burglary")
drugs <- london_st %>% filter(crime_type == "Drugs")
public_order <- london_st %>% filter(crime_type == "Public order")
shoplifting <- london_st %>% filter(crime_type == "Shoplifting")
vehicle_crime <- london_st %>% filter(crime_type == "Vehicle crime")
violence_sexual <- london_st %>% filter(crime_type == "Violence and sexual offences")
robbery <- london_st %>% filter(crime_type == "Robbery")
theft_person <- london_st %>% filter(crime_type == "Theft from the person")
other_crime <- london_st %>% filter(crime_type == "Other crime")


# about outcome of the crime
unique(london_st$last_outcome_category)

df_under_invest <- london_st %>% filter(last_outcome_category == "Under investigation")  # all under investigation are in 2021

df_not_guilty <- london_st %>%  filter(last_outcome_category == "Defendant found not guilty")

df_awaiting_court <- london_st %>%  filter(last_outcome_category == "Awaiting court outcome")



#################     visualization     #################     both point and geometry data / income is for all over the LONDON
# crimes and locations, the data has 3 outliers
pal <- colorFactor(palette = c("#8B0000", "#FF0000", "#FF4500" ,"#FF8C00", "#FFFFCC",
                               "#99CCFF", "#00CCFF", "#0099FF", "#0066FF", "#0033CC", "#0000CC", "#FFD600", "#057C85"), 
                   levels = c("Possession of weapons", "Criminal damage and arson", "Violence and sexual offences", "Robbery", "Other theft" ,"Theft from the person", "Drugs", 
                              "Shoplifting", "Public order", "Vehicle crime","Other crime" , "Burglary", "Bicycle theft"
                   ))
london_st <- london_st %>% mutate(pop = paste0(crime_type, " at ", Location))


leaflet() %>%
  addTiles() %>% setView(0.0099, 51.5,zoom = 11) %>% #mape kolie kore zamin
  addCircleMarkers(data = london_st, lng = ~Longitude, 
                   lat  = ~Latitude, radius = 5, color = ~pal(crime_type) ,
                   stroke = F, fillOpacity = 1, group = "Crime Type", popup = london_st$pop) %>% 
  addLegend(pal = pal, group = "Crime Type" , values = london_st$crime_type, title = "Crime Type", opacity = 0.7)




# different crime types and locations
pal1 <- colorBin("viridis", london_income$`Median Annual Household Income estimate`, 8, pretty = FALSE)

london_income <- london_income %>% mutate(pop1 = paste0(`Median Annual Household Income estimate`, " at ", LSOA11NM))


leaflet() %>%
  addTiles() %>% setView(-0.1000, 51.5,zoom = 13) %>%
  addHeatmap(data = criminal_damage, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Criminal damage and arson") %>%
  addHeatmap(data = weapon, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Possession of weapons") %>%
  addHeatmap(data = bike_theft, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Bicycle theft") %>%
  addHeatmap(data = other_theft, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Other theft") %>% 
  addHeatmap(data = drugs, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Drugs") %>%
  addHeatmap(data = public_order, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Public order") %>%
  addHeatmap(data = shoplifting, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Shoplifting") %>%
  addHeatmap(data = vehicle_crime, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Vehicle crime") %>%
  addHeatmap(data = violence_sexual, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Violence and sexual offences") %>%
  addHeatmap(data = robbery, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Robbery") %>%
  addHeatmap(data = theft_person, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Theft from the person") %>%
  addHeatmap(data = other_crime, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Other crime") %>%
  addPolygons(data = london_income, fillColor = ~pal1(london_income$`Median Annual Household Income estimate`),
              fillOpacity = 0.7,
              group = "Income",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_income$pop1,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))  %>%
  addLayersControl(
    overlayGroups = c( "Criminal damage and arson", "Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                       "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                       "Robbery", "Theft from the person", "Other crime", "Income"),
    options = layersControlOptions(collapsed = T),
    position = "bottomleft"
  ) %>% hideGroup(c("Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                    "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                    "Robbery", "Theft from the person", "Other crime", "Income"))



# barchart crime type
ggplot(london_st, aes(x = reorder(crime_type,crime_type,function(x)-length(x)), fill = crime_type)) +
  geom_bar() + xlab("Crime type")  + theme_minimal()


##################     LONDON SS     #################     
# type of criminals who were asked to remove more than outer layer of clothing
removed_cloth <- london_ss %>% filter(removal_of_more_than_just_outer_clothing == TRUE)

unique(removed_cloth$object_of_search)

# does the outcome link to the object of search?
true_outcome <- london_ss %>% filter(outcome_linked_to_object_of_search == TRUE)

table(true_outcome$Outcome)
#???????????????????????????????????????
df_eth_true_out <- true_outcome %>% mutate(self_defined_ethnicity = as.logical.factor(self_defined_ethnicity)) %>% 
  filter(self_defined_ethnicity)

df_eth_true_out <- true_outcome %>% mutate(officer_defined_ethnicity = as.factor(officer_defined_ethnicity)) %>% 
  filter(officer_defined_ethnicity)

# age vs gender
london_ss <- london_ss %>%  mutate(age = as.factor(age))

ggplot(london_ss, aes( x = reorder(age, age, function(x)-length(x)), fill = Gender)) + geom_bar()

# hour vs age
ggplot(london_ss, aes(x = hour(Date), y = age)) + geom_boxplot()
ggplot(london_ss, aes(x = hour(Date), y = age)) + geom_violin()

# hour vs gender
ggplot(london_ss, aes(x = hour(Date), y = Gender)) + geom_boxplot()
ggplot(london_ss, aes(x = hour(Date), y = Gender)) + geom_violin()
ggplot(london_ss, aes(x = hour(Date), fill = Gender)) + geom_density(alpha = .5)

