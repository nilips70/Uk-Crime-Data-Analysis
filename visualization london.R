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


# about outcome of the crime
unique(london_st$last_outcome_category)

df_under_invest <- london_st %>% filter(last_outcome_category == "Under investigation")  # all under investigation are in 2021

df_not_guilty <- london_st %>%  filter(last_outcome_category == "Defendant found not guilty")

df_awaiting_court <- london_st %>%  filter(last_outcome_category == "Awaiting court outcome")

  
##############################################################################
##############           London Street Data          ########################
##############################################################################

# barchart crime type
ggplot(london_st, aes(x = reorder(crime_type,crime_type,function(x)-length(x)), fill = crime_type)) +
  geom_bar() + xlab("Crime type")  + theme_minimal()


#leaflet map 1

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

#leaflet map 2

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
              fillOpacity = 0.3,
              group = "Income",
              weight = 0.2,
              smoothFactor = 0.2,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                fillOpacity = 0.2,
                sendToBack = TRUE),
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
                       "Average PTAL score", "Immigration Rate", "Median House Price", "Population Density"),
    options = layersControlOptions(collapsed = T),
    position = "bottomleft"
  ) %>% hideGroup(c("Possession of weapons", "Bicycle theft", "Other theft","Burglary", "Drugs","Public order", 
                    "Shoplifting", "Vehicle crime", "Violence and sexual offences",
                    "Robbery", "Theft from the person", "Other crime", "Income", "Unemployment Rate", "No Qualification Rate",
                    "Average PTAL score", "Immigration Rate", "Median House Price", "Population Density"))



##############################################################################
##############              Time Series               ########################
##############################################################################

                                # OVER 3 YEARS ALL CRIMES TOGETHER

#aggregating crime numbers based on a month
crime_agg <- london_st %>% select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series crimes over 3 years
p <- ggplot(crime_agg, aes(x = Month, y = value)) + geom_line() + theme_minimal() + ylab("Count") + xlab("Year") + 
  ggtitle("Committed Crimes in City of London Over 3 years") + labs(caption = "First National Lockdown: March to June 2020,
                                                            Second National Lockdown: November 2020,
                                                            Third National Lockdown: January to March 2021") +
   geom_vline(xintercept = as.Date("2020-03-13"), linetype="dashed", color="red") + geom_vline(xintercept = as.Date("2020-06-15"), linetype="dashed", color="red")+
  geom_vline(xintercept = as.Date("2020-11-01"), linetype="dashed", color="blue") + geom_vline(xintercept = as.Date("2020-11-30"), linetype="dashed", color="blue")+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", color="green") + geom_vline(xintercept = as.Date("2021-03-31"), linetype="dashed", color="green")

ggplotly(p) %>% layout(title = list(text = paste0('Committed Crimes in City of London Over 3 years',
                                                  '<br>',
                                                  '<sup>',
                                                  'National Lockdowns: March to June 2020, November 2020, January to March 2021',
                                                  '</sup>'))) 



#smoothed time series of crimes over 3yrs 
crime_agg <- crime_agg %>% mutate(roll_mean_2month = zoo::rollmean(value,2, na.pad = T), 
                     roll_mean_3months = zoo::rollmean(value,3, na.pad = T))

ggplot(crime_agg) + geom_line(aes(x = Month, y = value)) + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Smoothed Different Crime Types in City of London over 3 years")



ggplot(crime_agg) + geom_line(aes(x = Month, y = value)) + 
  geom_line(aes(x = Month, y = roll_mean_3months), color = "cyan4") +
  theme_minimal() +
  ylab("Count") + xlab("Year")+ labs(title = "Smoothed Different Crime Types in City of London Police over 3 years",
                                     caption = "sliding window = 3 months")




                                        #OVER 3 YEARS DIFFERENT CRIMES

#aggregating different crime types based on a month 
crime_by_type_all <- london_st %>% select(crime_type, Month) %>% 
  group_by(Month, crime_type) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series for each crime in 3 years
c <- ggplot(crime_by_type_all, aes(x = Month, y = value, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types in City of London over 3 years")

ggplotly(c)


#smoothed  time series by crime type
crime_by_type_smoothed <- crime_by_type_all %>% group_by(crime_type) %>% 
  mutate(roll_mean_2month = zoo::rollmean(value,2, na.pad = T),
         roll_mean_3month = zoo::rollmean(value,3, na.pad = T))

m <- ggplot(crime_by_type_smoothed, aes(x = Month, y = roll_mean_2month, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types in City of London Police over 3 years")

ggplotly(m)


                                        #EACH CRIME SMOOTHED

#smoothed time series for Bicycle theft
crime_by_type_smoothed %>% filter(crime_type == "Bicycle theft") %>% 
                                    ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Bicycle theft in City of London Police over 3 years (smoothed over 2 months)")

crime_by_type_smoothed %>% filter(crime_type == "Bicycle theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Bicycle theft in City of London over 3 years (smoothed over 3 months)")


#smoothed time series for Burglary
crime_by_type_smoothed %>% filter(crime_type == "Burglary") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Burglary in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Burglary") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Burglary in City of London over 3 years (smoothed over 2 months)")

#smoothed time series for Criminal damage and arson
crime_by_type_smoothed %>% filter(crime_type == "Criminal damage and arson") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Criminal damage and arson in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Criminal damage and arson") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Criminal damage and arson in City of London over 3 years (smoothed over 2 months)")

#smoothed time series for Drugs
crime_by_type_smoothed %>% filter(crime_type == "Drugs") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Drugs in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Drugs") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Drugs in City of London over 3 years (smoothed over 2 months)")

#smoothed time series for Other crime
crime_by_type_smoothed %>% filter(crime_type == "Other crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other crime in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Other crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other crime in City of London over 3 years (smoothed over 2 months)")

#smoothed time series for Other theft
crime_by_type_smoothed %>% filter(crime_type == "Other theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other theft in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Other theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other theft in City of London over 3 years (smoothed over 2 months)")


#smoothed time series for Possession of weapons
crime_by_type_smoothed %>% filter(crime_type == "Possession of weapons") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Possession of weapons in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Possession of weapons") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Possession of weapons in City of London over 3 years (smoothed over 2 months)")



#smoothed time series for Public order
crime_by_type_smoothed %>% filter(crime_type == "Public order") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Public order in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Public order") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Public order in City of London over 3 years (smoothed over 2 months)")


#smoothed time series for Robbery
crime_by_type_smoothed %>% filter(crime_type == "Robbery") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Robbery in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Robbery") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Robbery in City of London over 3 years (smoothed over 2 months)")


#smoothed time series for Shoplifting
crime_by_type_smoothed %>% filter(crime_type == "Shoplifting") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Shoplifting in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Shoplifting") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Shoplifting in City of London over 3 years (smoothed over 2 months)")


#smoothed time series for Theft from the person
crime_by_type_smoothed %>% filter(crime_type == "Theft from the person") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Theft from the person in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Theft from the person") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Theft from the person in City of London over 3 years (smoothed over 2 months)")



#smoothed time series for Vehicle crime
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Vehicle crime Registered in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Vehicle crime in City of London over 3 years (smoothed over 2 months)")

#smoothed time series for Violence and sexual offences
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Violence and sexual offences Registered in City of London over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Violence and sexual offences") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Violence and sexual offences in City of London over 3 years (smoothed over 2 months)")

                                              

                                          #BEFORE COVID #ALL CRIMES TOGETTHER

#aggregating crimes based on a month between 2018 and 2020 (before COVID-19 hit) first lockdown was introduced in march 2020
seasonality_total_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 

#time series crimes until COVID-19 hit   
c1 <- ggplot(seasonality_total_2019, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
  xlab("Date") + theme_minimal() + ggtitle("Committed Crimes in City of London Police before Covid Pandemic")

ggplotly(c1)


                                          #BEFORE COVID #DIFFERENT CRIMES

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 

#time series different crime types before COVID 19 hit
c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
  geom_line() + xlab("Date") +ylab("Count") + ggtitle("Different Crime Types in City of London before Covid Pandemic")

ggplotly(c2)


###########################################################
###################  CORRELOGRAM  #########################
###########################################################
#correlation of different crime types on monthly basis before COVID-19 hits
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")



#correlation of different crime types on monthly basis 3yrs
seasonality_3yrs <- london_st %>% mutate(Month = ym(Month)) %>% select(crime_type, Month) %>%
  group_by(Month, crime_type) %>%  summarise(value = n())


corr1 = seasonality_3yrs_wide = seasonality_3yrs %>% pivot_wider(names_from = crime_type, values_from = value) %>% 
  ungroup() %>% select(-Month) %>% cor(use="pairwise.complete.obs")

corrplot::corrplot(corr1, diag = FALSE, type = "upper")


#correlation of different crime types on monthly basis after people getting vaccinated and things goes back to normal
seasonality_by_type_after <- london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month > ym("2021-03")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


corr2 = seasonality_by_type_after_long = seasonality_by_type_after %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr2, diag = FALSE, type = "upper")


######################################################
##############  FUNCTIONS FOR LIMITIN TIME  ##########
######################################################

#function for visualizing crimes before an specific date like lockdowns

total_crime_fun = function(data,x){
  
  seasonality_total <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 
  
  #time series crimes until COVID-19 hit   
  c1 <- ggplot(seasonality_total, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
    xlab("Date") + theme_minimal() + ggtitle(paste0("Committed Crimes in City of London before ", x))
  
  return(ggplotly(c1))
  
}

total_crime_fun(data = london_st, x = "2021-04")


#function for visualizing different crime types before an specific date like lockdowns

crime_by_type_fun = function(data,x){
  
  seasonality_by_type <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types in City of London before ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_fun(data = london_st, x = "2020-04")


#function for visualizing different crime types within an specific year

crime_by_type_yearly_fun = function(data,x){
  
  seasonality_by_type_2019 <- data %>% mutate(Month = ym(Month)) %>% 
    filter(year(Month) == x) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types in City of London in ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_yearly_fun(data = london_st, x = 2020)


#=========================================================================
x <- ts(crime_agg$value, start = c(2018, 12), end = c(2020, 3), frequency = 30)
m <- decompose(x)
plot(m)
plot(m$seasonal)
plot(x)
crime_agg


#################     Stop and Search     #################     
#################     data exploring / preparation    #################

#aggregating outcome based on hour 
hourly <- london_ss %>% filter(Date < ym("2020-04")) %>% mutate(time = hour(Date)) %>% 
  group_by(Outcome, time) %>% summarise(value = n()) %>% group_by(time) %>% 
  mutate(total_stop = sum(value),
         rate = round(value/total_stop, 2)) %>% 
  na.omit() %>% 
  filter(total_stop > 50, Outcome != "A no further action disposal")
 


c3 <- ggplot(hourly, aes(x = time, y = rate, color = Outcome)) + geom_line() + xlab("hour") +
  ylab("Outcome Rate") + theme_minimal() + ggtitle("Different Serious Outcomes Rates per Hour")
ggplotly(c3)


# type of criminals who were asked to remove more than outer layer of clothing
removed_cloth <- london_ss %>% filter(removal_of_more_than_just_outer_clothing == TRUE)

unique(removed_cloth$object_of_search)

# does the outcome link to the object of search?
true_outcome <- london_ss %>% filter(outcome_linked_to_object_of_search == TRUE)

table(true_outcome$Outcome)

# age vs gender
london_ss <- london_ss %>%  mutate(age = as.factor(age))

ggplot(london_ss, aes( x = reorder(age, age, function(x)-length(x)), fill = Gender)) + geom_bar()

# hour vs age
ggplot(london_ss, aes(x = hour(Date), y = age, fill = Gender)) + geom_boxplot()+ theme_classic() +scale_fill_manual(values = c("deeppink", "cyan3", "grey"))+
  labs(title="City of London Police", caption = "Jan 2019 - Dec 2021")

# hour vs gender
ggplot(london_ss, aes(x = hour(Date), y = Gender)) + geom_boxplot() + theme_classic()

