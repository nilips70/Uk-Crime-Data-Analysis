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

london_others = st_as_sf(london_others)
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
pal2 <- colorFactor(palette = c("#330000","#FF6600", "#FF9900","#FFCC66" ), levels = c("10 or above" , "4.5 - 10", "2.7 - 4.5", "2.7 or below"))
pal3 <- colorFactor(palette = c("#330000","#FF6600", "#FF9900","#FFCC66" ), levels = c("20 or above" , "7 - 20", "2 - 7", "2 or below"))
pal4 <- colorFactor(palette = c("#00FFFF","#00CCCC", "#009999","#006666" ), levels = c("7.8 or below" , "7.8 - 7.93", "7.93 - 7.98", "7.98 or above"))
pal5 <- colorBin("viridis", london_others$`country of birth Not uk rate`, 8, pretty = FALSE)
pal6 <- colorBin("viridis", london_others$`median price of house`, 8, pretty = FALSE)
pal7 <- colorBin("Greys", london_others$`White rate`, 8, pretty = FALSE)
pal8 <- colorBin("Greys", london_others$`Mixed ethnic groups rate`, 8, pretty = FALSE)
pal9 <- colorBin("Greys", london_others$`Asian_Asian British rate`, 8, pretty = FALSE)
pal10 <- colorBin("Greys", london_others$`Black_ African_Caribbean_Black British rate`, 8, pretty = FALSE)
pal11 <- colorBin("Greys", london_others$`Other ethnic group rate`, 8, pretty = FALSE)
pal12 <- colorBin("viridis", london_others$`population density (persons per hectar)`, 8, pretty = FALSE)


london_others <- london_others %>% mutate(pop1 = paste0(round(median_annual_income_household), " at ", LSOA11NM),
                                          pop2 = paste0(`Unemployment Rate`, "% at ", LSOA11NM),
                                          pop3 = paste0(`No qualifications rate`, "% at ", LSOA11NM),
                                          pop4= paste0(`Average Score of PTAL`, " at ", LSOA11NM),
                                          pop5 = paste0(`country of birth Not uk rate`, "% at ", LSOA11NM),
                                          pop6 = paste0(round(`median price of house`/1000), "K at ", LSOA11NM),
                                          pop7 = paste0(round(`White rate`,2), "% at ", LSOA11NM),
                                          pop8 = paste0(round(`Mixed ethnic groups rate`,2), "% at ", LSOA11NM),
                                          pop9 = paste0(round(`Asian_Asian British rate`,2), "% at", LSOA11NM),
                                          pop10 = paste0(round(`Black_ African_Caribbean_Black British rate`,2), "% at ", LSOA11NM),
                                          pop11 = paste0(round(`Other ethnic group rate`, 2), "% at ", LSOA11NM),
                                          pop12 = paste0(round(`population density (persons per hectar)`,2), " per hectare at ", LSOA11NM))


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
  addLegend(pal = pal1, group = "Income" , values = london_others$median_annual_income_household, title = "Median Income (£)" ,opacity = 0.7) %>%
  addPolygons(data = london_others, fillColor = ~pal2(london_others$unemp),
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
  addLegend(pal = pal2, group = "Unemployment Rate" , values = london_others$unemp, title = "Unemployment %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal3(london_others$qualification),
              fillOpacity = 0.4,
              group = "No Qualification Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop3,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>% 
  addLegend(pal = pal3, group = "No Qualification Rate" , values = london_others$qualification, title = "No Qualification %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal4(london_others$ptal),
              fillOpacity = 0.4,
              group = "Average PTAL score",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop4,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal4, group = "Average PTAL score" , values = london_others$ptal, title = "Average PTAL score" ,opacity = 0.7) %>%
  addPolygons(data = london_others, fillColor = ~pal5(london_others$`country of birth Not uk rate`),
              fillOpacity = 0.4,
              group = "Immigration Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop5,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal5, group = "Immigration Rate" , values = london_others$`country of birth Not uk rate`, title = "Immigration %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal6(london_others$`median price of house`),
              fillOpacity = 0.4,
              group = "Median House Price",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop6,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal6, group = "Median House Price" , values = london_others$`median price of house`, title = "Median House Price (£)" ,opacity = 0.7) %>%
  addPolygons(data = london_others, fillColor = ~pal7(london_others$`White rate`),
              fillOpacity = 0.4,
              group = "White Ethnic Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop7,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal7, group = "White Ethnic Rate" , values = london_others$`White rate`, title = "White Ethnic %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal8(london_others$`Mixed ethnic groups rate`),
              fillOpacity = 0.4,
              group = "Mixed Ethnic Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop8,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal8, group = "Mixed Ethnic Rate" , values = london_others$`Mixed ethnic groups rate`, title = "Mixed Ethnic %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal9(london_others$`Asian_Asian British rate`),
              fillOpacity = 0.4,
              group = "Asian/Asian British Ethnic Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop9,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal9, group = "Asian/Asian British Ethnic Rate" , values = london_others$`Asian_Asian British rate`, title = "Asian/Asian British Ethnic %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal10(london_others$`Black_ African_Caribbean_Black British rate`),
              fillOpacity = 0.4,
              group = "Black/Black British Ethnic Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop10,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal10, group = "Black/Black British Ethnic Rate" , values = london_others$`Black_ African_Caribbean_Black British rate`, title = "Black/Black British Ethnic %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal11(london_others$`Other ethnic group rate`),
              fillOpacity = 0.4,
              group = "Other Ethnics Rate",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop11,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal11, group = "Other Ethnics Rate" , values = london_others$`Other ethnic group rate`, title = "Other Ethnics %", opacity = 0.7,
            labFormat = labelFormat(suffix = " %")) %>%
  addPolygons(data = london_others, fillColor = ~pal12(london_others$`population density (persons per hectar)`),
              fillOpacity = 0.4,
              group = "Population Density",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                bringToFront = TRUE),
              label=london_others$pop12,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend(pal = pal12, group = "Population Density" , values = london_others$`population density (persons per hectar)`, title = "Population (per hectare)" ,opacity = 0.7) %>%
  addLayersControl(
    overlayGroups = c( "Criminal damage and arson", "Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                       "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                       "Robbery", "Theft from the person", "Other crime", "Income", "Unemployment Rate", "No Qualification Rate",
                       "Average PTAL score", "Immigration Rate", "Median House Price", "White Ethnic Rate", "Mixed Ethnic Rate",
                       "Asian/Asian British Ethnic Rate","Black/Black British Ethnic Rate", "Other Ethnics Rate", "Population Density"),
    options = layersControlOptions(collapsed = T),
    position = "bottomleft"
  ) %>% hideGroup(c("Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                    "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                    "Robbery", "Theft from the person", "Other crime", "Income", "Unemployment Rate", "No Qualification Rate",
                    "Average PTAL score", "Immigration Rate", "Median House Price", "White Ethnic Rate", "Mixed Ethnic Rate",
                    "Asian/Asian British Ethnic Rate","Black/Black British Ethnic Rate","Other Ethnics Rate", "Population Density"))



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
ggplot(crime_agg, aes(x = Month, y = value)) + geom_line() + theme_minimal() + ylab("Count") + xlab("Year") + 
  ggtitle("Committed Crimes in London over 3 years")

#aggregating different crime types based on a month 
crime_by_type_all <- london_st %>% select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series for each crime in 3 years
c <- ggplot(crime_by_type_all, aes(x = Month, y = value, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types in London over 3 years")

ggplotly(c)

#aggregating crimes based on a month between 2018 and 2020 (before COVID-19 hit) first lockdown was introduced in march 2020
seasonality_total_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 

#time series crimes until COVID-19 hit   
c1 <- ggplot(seasonality_total_2019, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
  xlab("Date") + theme_minimal() + ggtitle("Committed Crimes in London before Covid Pandemic")

ggplotly(c1)

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 

#time series different crime types before COVID 19 hit
c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
  geom_line() + xlab("Date") +ylab("Count") + ggtitle("Different Crime Types in London before Covid Pandemic")

ggplotly(c2)

#correlation of different crime types on monthly basis
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper", title = "Correlogram of Different Crime Types on Monthly Basis")


#function for visualizing crimes before an specific date like lockdowns

total_crime_fun = function(data,x){
  
  seasonality_total <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 
  
  #time series crimes until COVID-19 hit   
  c1 <- ggplot(seasonality_total, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
    xlab("Date") + theme_minimal() + ggtitle(paste0("Committed Crimes in London before ", x))
  
  return(ggplotly(c1))
  
}

total_crime_fun(data = london_st, x = "2021-04")


#function for visualizing different crime types before an specific date like lockdowns

crime_by_type_fun = function(data,x){
  
  seasonality_by_type <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types in London before ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_fun(data = london_st, x = "2020-04")


#function for visualizing different crime types within an specific year

crime_by_type_yearly_fun = function(data,x){
  
  seasonality_by_type_2019 <- data %>% mutate(Month = ym(Month)) %>% 
    filter(year(Month) == x) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types in London in ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_yearly_fun(data = london_st, x = 2020)



#################     LONDON SS     #################     
#################     data exploring / preparation    #################
#aggregating outcome based on hour 
hourly <- london_ss %>% filter(Date < ym("2020-04")) %>% mutate(time = hour(Date)) %>% 
  group_by(Outcome, time) %>% summarise(value = n()) %>% group_by(time) %>% 
  mutate(total_stop = sum(value),
         rate = round(value/total_stop, 2)) %>% 
  filter(total_stop > 50, Outcome != "A no further action disposal") %>% 
  na.omit()


c3 <- ggplot(hourly, aes(x = time, y = rate, color = Outcome)) + geom_line() + xlab("hour") +
  ylab("Outcome Rate") + theme_minimal() + ggtitle("Different Serious Outcomes Rates per Hour")
ggplotly(c3)


#=========================================================================

