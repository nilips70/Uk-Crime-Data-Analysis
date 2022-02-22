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

#reading manchester datasets
metropolitan_ss <- readRDS("metropolitan_ss.rds")
metropolitan_st <- readRDS("metropolitan_st.rds")



##############################################################################
##############           Metropolitan Police Service Street Data          ########################
##############################################################################    

# barchart crime type
m <- ggplot(metropolitan_st, aes(x = reorder(crime_type,crime_type,function(x)-length(x)), fill = crime_type)) +
  geom_bar() + xlab("Crime type")  + theme_minimal()
ggplotly(m)



#leaflet
# crimes and locations
pal <- colorFactor(palette = c("#8B0000", "#FF0000", "#FF4500" ,"#FF8C00", "#FFFFCC",
                               "#99CCFF", "#00CCFF", "#0099FF", "#0066FF", "#0033CC", "#0000CC", "#FFD600", "#057C85"), 
                   levels = c("Possession of weapons", "Criminal damage and arson", "Violence and sexual offences", "Robbery", "Other theft" ,"Theft from the person", "Drugs", 
                              "Shoplifting", "Public order", "Vehicle crime","Other crime" , "Burglary", "Bicycle theft"
                   ))
metropolitan_st <- metropolitan_st %>% mutate(pop = paste0(crime_type, " at ", Location))


leaflet() %>%
  addTiles() %>% setView(-2.24, 53.48,zoom = 9) %>% #mape kolie kore zamin
  addCircleMarkers(data = metropolitan_st, lng = ~Longitude, 
                   lat  = ~Latitude, radius = 5, color = ~pal(crime_type) ,
                   stroke = F, fillOpacity = 1, group = "Crime Type", popup = metropolitan_st$pop) %>% 
  addLegend(pal = pal, group = "Crime Type" , values = metropolitan_st$crime_type, title = "Crime Type", opacity = 0.7)


##############################################################################
##############              Time Series               ########################
##############################################################################

                            # OVER 3 YEARS ALL CRIMES TOGETHER


#aggregating crime numbers based on a month
crime_agg_metro <- metropolitan_st %>% select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series crimes over 3 years
p <- ggplot(crime_agg_metro, aes(x = Month, y = value)) + geom_line() + theme_minimal() + ylab("Count") + xlab("Year") + 
  ggtitle("Committed Crimes Registered in Metropolitan Police Service Over 3 years") + labs(caption = "First National Lockdown: March to June 2020,
                                                            Second National Lockdown: November 2020,
                                                            Third National Lockdown: January to March 2021") +
  geom_vline(xintercept = as.Date("2020-03-13"), linetype="dashed", color="red") + geom_vline(xintercept = as.Date("2020-06-15"), linetype="dashed", color="red")+
  geom_vline(xintercept = as.Date("2020-11-01"), linetype="dashed", color="blue") + geom_vline(xintercept = as.Date("2020-11-30"), linetype="dashed", color="blue")+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", color="green") + geom_vline(xintercept = as.Date("2021-03-31"), linetype="dashed", color="green")

ggplotly(p) %>% layout(title = list(text = paste0('Committed Crimes Registered in Metropolitan Police Service Over 3 years',
                                                  '<br>',
                                                  '<sup>',
                                                  'National Lockdowns: March to June 2020, November 2020, January to March 2021',
                                                  '</sup>'))) 


#simultaneous winth visualization london.R (bcz of crime_agg)
crime_agg_metro <- crime_agg_metro %>%  mutate(region = "City of London")
crime_agg_metro <- crime_agg_metro %>% mutate(region = "Metropolitan Police Service")

crime_agg_total <- rbind(crime_agg, crime_agg_metro)


crime_agg_total %>% 
  group_by(region) %>% 
  mutate(scaled_val = scale(value)) %>% #scaling data to have zero mean and standard deviatoin on 1 for comparable time series
ggplot(aes(x = Month, y = scaled_val, color = region) ) + geom_line() + theme_minimal() + ylab("Count") + xlab("Year") + 
  ggtitle("Committed Crimes Over 3 years") + labs(caption = "First National Lockdown: March to June 2020,
                                                            Second National Lockdown: November 2020,
                                                            Third National Lockdown: January to March 2021") +
  geom_vline(xintercept = as.Date("2020-03-13"), linetype="dashed", color="red") + geom_vline(xintercept = as.Date("2020-06-15"), linetype="dashed", color="red")+
  geom_vline(xintercept = as.Date("2020-11-01"), linetype="dashed", color="blue") + geom_vline(xintercept = as.Date("2020-11-30"), linetype="dashed", color="blue")+
  geom_vline(xintercept = as.Date("2021-01-01"), linetype="dashed", color="green") + geom_vline(xintercept = as.Date("2021-03-31"), linetype="dashed", color="green")


#smoothed time series of crimes over 3yrs 
crime_agg_metro <- crime_agg_metro %>% filter(region == "Metropolitan Police Service") %>% mutate(roll_mean_2month = zoo::rollmean(value,2, na.pad = T), 
                                  roll_mean_3months = zoo::rollmean(value,3, na.pad = T))

ggplot(crime_agg_metro) + geom_line(aes(x = Month, y = value)) + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  theme_minimal() +
  ylab("Count") + xlab("Year")+ labs(title = "Smoothed Different Crime Types Registered in Metropolitan Police Service over 3 years",
                                     caption = "sliding window = 2 months")



ggplot(crime_agg_metro) + geom_line(aes(x = Month, y = value)) + 
  geom_line(aes(x = Month, y = roll_mean_3months), color = "cyan4", show.legend = FALSE) +
  theme_minimal() +
  ylab("Count") + xlab("Year")+ labs(title = "Smoothed Different Crime Types Registered in Metropolitan Police Service over 3 years",
                                     caption = "sliding window = 3 months")



                                            #OVER 3 YEARS DIFFERENT CRIMES


#aggregating different crime types based on a month 
crime_by_type_all <- metropolitan_st %>% select(crime_type, Month) %>% 
  group_by(Month, crime_type) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series for each crime in 3 years
c <- ggplot(crime_by_type_all, aes(x = Month, y = value, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types Registered in Metropolitan Police Service over 3 years")

ggplotly(c)


#smoothed  time series by crime type
crime_by_type_smoothed <- crime_by_type_all %>% group_by(crime_type) %>% 
  mutate(roll_mean_2month = zoo::rollmean(value,2, na.pad = T),
         roll_mean_3month = zoo::rollmean(value,3, na.pad = T))

m <- ggplot(crime_by_type_smoothed, aes(x = Month, y = roll_mean_2month, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types Registered in Metropolitan Police Service over 3 years")

ggplotly(m)


                                                      #EACH CRIME SMOOTHED


#smoothed time series for Bicycle theft
crime_by_type_smoothed %>% filter(crime_type == "Bicycle theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Bicycle theft in Metropolitan Police Service over 3 years (smoothed over 2 months)")

crime_by_type_smoothed %>% filter(crime_type == "Bicycle theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Bicycle theft Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")

#smoothed time series for Burglary
crime_by_type_smoothed %>% filter(crime_type == "Burglary") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Burglary Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Burglary") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Burglary in Metropolitan Police Service over 3 years (smoothed over 2 months)")

#smoothed time series for Criminal damage and arson
crime_by_type_smoothed %>% filter(crime_type == "Criminal damage and arson") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Criminal Damage and Arson Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Criminal damage and arson") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Criminal damage and arson in Metropolitan Police Service over 3 years (smoothed over 2 months)")

#smoothed time series for Drugs
crime_by_type_smoothed %>% filter(crime_type == "Drugs") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Drugs Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Drugs") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Drugs in Metropolitan Police Service over 3 years (smoothed over 2 months)")

#smoothed time series for Other crime
crime_by_type_smoothed %>% filter(crime_type == "Other crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other Crime Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Other crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other crime in Metropolitan Police Service over 3 years (smoothed over 2 months)")

#smoothed time series for Other theft
crime_by_type_smoothed %>% filter(crime_type == "Other theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other theft Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Other theft") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other theft in Metropolitan Police Service over 3 years (smoothed over 2 months)")


#smoothed time series for Possession of weapons
crime_by_type_smoothed %>% filter(crime_type == "Possession of weapons") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Possession of Weapons Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Possession of weapons") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Possession of weapons in Metropolitan Police Service over 3 years (smoothed over 2 months)")



#smoothed time series for Public order
crime_by_type_smoothed %>% filter(crime_type == "Public order") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Public Order Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Public order") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Public order in Metropolitan Police Service over 3 years (smoothed over 2 months)")


#smoothed time series for Robbery
crime_by_type_smoothed %>% filter(crime_type == "Robbery") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Robbery Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Robbery") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Robbery in Metropolitan Police Service over 3 years (smoothed over 2 months)")


#smoothed time series for Shoplifting
crime_by_type_smoothed %>% filter(crime_type == "Shoplifting") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Shoplifting Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Shoplifting") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Shoplifting in Metropolitan Police Service over 3 years (smoothed over 2 months)")


#smoothed time series for Theft from the person
crime_by_type_smoothed %>% filter(crime_type == "Theft from the person") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Theft from the Person Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Theft from the person") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Theft from the person in Metropolitan Police Service over 3 years (smoothed over 2 months)")


#smoothed time series for Vehicle crime
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Vehicle crime Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Vehicle crime in Metropolitan Police Service over 3 years (smoothed over 2 months)")

#smoothed time series for Violence and sexual offences
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_3month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Violence and sexual offences Registered in Metropolitan Police Service over 3 years (smoothed over 3 months)")


crime_by_type_smoothed %>% filter(crime_type == "Violence and sexual offences") %>% 
  ggplot() + geom_line(aes(x = Month, y = roll_mean_2month), color = "orange") +
  geom_point(aes(x = Month, y = value), color = "black") + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Violence and sexual offences in Metropolitan Police Service over 3 years (smoothed over 2 months)")

  


                                            #BEFORE COVID #ALL CRIMES TOGETTHER


#aggregating crimes based on a month between 2018 and 2020 (before COVID-19 hit) first lockdown was introduced in march 2020
seasonality_total_2019 <- metropolitan_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 

#time series crimes until COVID-19 hit   
c1 <- ggplot(seasonality_total_2019, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
  xlab("Date") + theme_minimal() + ggtitle("Committed Crimes in Metropolitan Police Service before Covid Pandemic")

ggplotly(c1)

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- metropolitan_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 

#time series different crime types before COVID 19 hit
c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
  geom_line() + xlab("Date") +ylab("Count") + ggtitle("Different Crime Types in Metropolitan Police Service before Covid Pandemic")

ggplotly(c2)

#correlation of different crime types on monthly basis before COVID-19 hits
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis befor Covid 19 hits")


#correlation of different crime types on monthly basis over 3 yrs
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")



#correlation of different crime types on monthly basis 3yrs
seasonality_3yrs <- metropolitan_st %>% mutate(Month = ym(Month)) %>% select(crime_type, Month) %>%
  group_by(Month, crime_type) %>%  summarise(value = n())


corr1 = seasonality_3yrs_wide = seasonality_3yrs %>% pivot_wider(names_from = crime_type, values_from = value) %>% 
  ungroup() %>% select(-Month) %>% cor(use="pairwise.complete.obs")

corrplot::corrplot(corr1, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis Between 2018-2021")


#correlation of different crime types on monthly basis after people getting vaccinated and things goes back to normal
seasonality_by_type_after <- metropolitan_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month > ym("2021-03")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


corr2 = seasonality_by_type_after_long = seasonality_by_type_after %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr2, diag = FALSE, type = "upper")




#function for visualizing crimes before an specific date like lockdowns

total_crime_fun = function(data,x){
  
  seasonality_total <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month) %>% summarise(value = n()) 
  
  #time series crimes until COVID-19 hit   
  c1 <- ggplot(seasonality_total, aes(x = Month, y = value)) + geom_line() + ylab("Count") + 
    xlab("Date") + theme_minimal() + ggtitle(paste0("Committed Crimes Registered in Metropolitan Police Service before ", x))
  
  return(ggplotly(c1))
  
}

total_crime_fun(data = manchester_st, x = "2021-04")


#function for visualizing different crime types before an specific date like lockdowns

crime_by_type_fun = function(data,x){
  
  seasonality_by_type <- data %>% mutate(Month = ym(Month)) %>% 
    filter(Month < ym(x)) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types Registered in Metropolitan Police Service before ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_fun(data = manchester_st, x = "2020-04")


#function for visualizing different crime types within an specific year

crime_by_type_yearly_fun = function(data,x){
  
  seasonality_by_type_2019 <- data %>% mutate(Month = ym(Month)) %>% 
    filter(year(Month) == x) %>%
    select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 
  
  c2 <- ggplot(seasonality_by_type_2019, aes(x = Month, y = value, color = crime_type)) +
    geom_line() + xlab("Date") +ylab("Count") + ggtitle(paste0("Different Crime Types in London in ", x))
  
  return(ggplotly(c2))
  
}

crime_by_type_yearly_fun(data = manchester_st, x = 2020)

#=========================================================================
x <- ts(crime_agg$value, start = c(2018, 12), end = c(2020, 3), frequency = 30)
m <- decompose(x)
plot(m)
plot(m$seasonal)
plot(x)
crime_agg


##################     Stop and Search Dataset     #################     
#################     data exploring / preparation    #################
#aggregating outcome based on hour 
hourly <- metropolitan_ss %>% filter(Date < ym("2020-04")) %>% mutate(time = hour(Date)) %>% 
  group_by(Outcome, time) %>% summarise(value = n()) %>% group_by(time) %>% 
  mutate(total_stop = sum(value),
         rate = round(value/total_stop, 2)) %>% 
  na.omit() %>% 
  filter(total_stop > 50, Outcome != "A no further action disposal")



c3 <- ggplot(hourly, aes(x = time, y = rate, color = Outcome)) + geom_line() + xlab("hour") +
  ylab("Outcome Rate") + theme_minimal() + labs(title = "Different Serious Outcomes Rates per Hour for Metropolitan Police Service")
ggplotly(c3)


# age vs gender
metropolitan_ss <- metropolitan_ss %>%  mutate(age = as.factor(age))

ggplot(metropolitan_ss, aes( x = reorder(age, age, function(x)-length(x)), fill = Gender)) + geom_bar()

# hour vs age
ggplot(metropolitan_ss, aes(x = hour(Date), y = age, fill = Gender)) + geom_boxplot()+ theme_classic() +scale_fill_manual(values = c("deeppink", "cyan3", "grey")) +
  labs(title = "Metropolitan Police Service", caption = "Jan 2019 - Dec 2021")

# hour vs gender
ggplot(metropolitan_ss, aes(x = hour(Date), y = Gender)) + geom_boxplot() + theme_classic()
