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
library(plotly)
rm(list=ls())

#reading the dataset
london_ss <- readRDS("df_ss.rds")
london_st <- readRDS("df_st_london.rds")
london_others <- readRDS("london_others.rds")
df_st <- readRDS("df_st_london.rds")

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
pal1 <- colorBin("viridis", london_others$median_annual_income_household, 8, pretty = FALSE)
pal2 <- colorBin("viridis", london_others$`Unemployment Rate`, 8, pretty = FALSE)

#pal2 <- colorFactor(palette = c("#FF6600", "#FF9900","#FFCC66" ), levels = c(">21" , "14.5 - 21", "14.5>"))

london_others <- london_others %>% mutate(pop1 = paste0(round(median_annual_income_household), " at ", LSOA11NM),
                                          pop2 = paste0(`Unemployment Rate`, " at ", LSOA11NM))


leaflet() %>%
  addTiles() %>% setView(-0.1000, 51.5,zoom = 13) %>%
  addHeatmap(data = criminal_damage, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Criminal damage and arson") %>%
  addHeatmap(data = weapon, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Possession of weapons") %>%
  addHeatmap(data = bike_theft, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Bicycle theft") %>%
  addHeatmap(data = other_theft, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Other theft") %>% 
  addHeatmap(data = burglary, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Burglary") %>% 
  addHeatmap(data = drugs, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Drugs") %>%
  addHeatmap(data = public_order, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Public order") %>%
  addHeatmap(data = shoplifting, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Shoplifting") %>%
  addHeatmap(data = vehicle_crime, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Vehicle crime") %>%
  addHeatmap(data = violence_sexual, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Violence and sexual offences") %>%
  addHeatmap(data = robbery, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Robbery") %>%
  addHeatmap(data = theft_person, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Theft from the person") %>%
  addHeatmap(data = other_crime, lng = ~Longitude, lat = ~Latitude, intensity = 10 , max = 100, radius=20, blur = 10, group = "Other crime") %>%
  addPolygons(data = london_others, fillColor = ~pal1(london_others$median_annual_income_household),
              fillOpacity = 0.7,
              group = "Income",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop1,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"))  %>%
  addPolygons(data = london_others, fillColor = ~pal2(london_others$`Unemployment Rate`),
              fillOpacity = 0.7,
              group = "Unemployment Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop2,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLayersControl(
    overlayGroups = c( "Criminal damage and arson", "Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                       "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                       "Robbery", "Theft from the person", "Other crime", "Income", "Unemployment Rate"),
    options = layersControlOptions(collapsed = T),
    position = "bottomleft"
  ) %>% hideGroup(c("Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                    "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                    "Robbery", "Theft from the person", "Other crime", "Income", "Unemployment Rate"))



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
#df_eth_true_out <- true_outcome %>% mutate(self_defined_ethnicity = as.logical.factor(self_defined_ethnicity)) %>% 
  filter(self_defined_ethnicity)

#df_eth_true_out <- true_outcome %>% mutate(officer_defined_ethnicity = as.factor(officer_defined_ethnicity)) %>% 
  filter(officer_defined_ethnicity)

# age vs gender
london_ss <- london_ss %>%  mutate(age = as.factor(age))

ggplot(london_ss, aes( x = reorder(age, age, function(x)-length(x)), fill = Gender)) + geom_bar()

# hour vs age
ggplot(london_ss, aes(x = hour(Date), y = age)) + geom_boxplot()

# hour vs gender
ggplot(london_ss, aes(x = hour(Date), y = Gender)) + geom_boxplot()

##############################################################################
##############              Time Series               ########################
##############################################################################
#aggregating crime numbers based on a month
crime_agg <- london_st %>% select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series crimes for 3 years
ggplot(crime_agg, aes(x = Month, y = value)) + geom_line() + theme_minimal()

#aggregating different crime types based on a month 
crime_by_type_all <- london_st %>% select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series for each crime in 3 years
c <- ggplot(crime_by_type_all, aes(x = Month, y = value, color = crime_type)) + geom_line() + theme_minimal()
ggplotly(c)

#aggregating crimes based on a month between 2018 and 2020 (before COVID-19 hit)
seasonality_total_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-07")) %>%
  select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 

#time series crimes until COVID-19 hit   
c1 <- ggplot(seasonality_total_2019, aes(x = Month, y = value)) + geom_line() + theme_minimal()
ggplotly(c1)

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 

#time series different crime types before COVID 19 hit
c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) + geom_line()
ggplotly(c2)

#correlation of different crime types on monthly basis
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, type = "upper", title = "2019")



#################     LONDON SS     #################     
#################     data exploring / preparation    #################
#aggregating outcome 
hourly <- london_ss %>% filter(Date < ym("2020-04")) %>% mutate(time = hour(Date)) %>% 
  group_by(Outcome, time) %>% summarise(value = n()) %>% group_by(time) %>% 
  mutate(total_stop = sum(value),
         rate = round(value/total_stop, 2)) %>% 
  filter(total_stop > 50, Outcome != "A no further action disposal") %>% 
  na.omit()

#

c3 <- ggplot(hourly, aes(x = time, y = rate, color = Outcome)) + geom_line()
ggplotly(c3)
