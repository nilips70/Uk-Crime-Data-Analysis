library(GGally)

#reading  datasets
west_midland_ss <- readRDS("west_midland_ss.rds")
west_midland_st <- readRDS("west_midland_st.rds")
merseyside_ss <- readRDS("merseyside_ss.rds")
merseyside_st <- readRDS("merseyside_st.rds")
all_london_st <- readRDS("all_london_st.rds")

#data preparation
west_midland_ss <- west_midland_ss %>% mutate(region = "W")
west_midland_st <- west_midland_st %>% mutate(region = "W")

merseyside_ss <- merseyside_ss %>% mutate(region = "M")
merseyside_st <- merseyside_st %>% mutate(region = "M")

all_london_st <- all_london_st %>% mutate(region = "L")

all <- rbind(west_midland_st, merseyside_st, all_london_st)


all_monthly = all %>% 
  select(Month, region, crime_type) %>% 
  group_by(Month, region, crime_type) %>% 
  summarise(value = n()) %>% 
  ungroup()

all_monthly = all_monthly %>% 
  mutate(Month = ym(Month),
         period = case_when(Month < ym("2020-04") ~ 'pre_pand',
                            Month > ym("2021-03") ~ 'post_vaccine',
                            TRUE ~ 'mid_pand'))


all_monthly_wide = all_monthly %>% 
  pivot_wider(
    names_from = crime_type,
              values_from = value)


ggpairs(all_monthly_wide, columns= names(all_monthly_wide)[4:16] , mapping = aes(color = region))


test = all_monthly_wide %>% 
  group_by(region) %>% 
  mutate(across(`Bicycle theft`:`Violence and sexual offences`, ~ scale(.)))

ggpairs(test, columns= names(all_monthly_wide)[4:16])


##############################################################
                        #visualization
##############################################################

                          #OVER 3 YEARS DIFFERENT CRIMES


#aggregating different crime types based on a month 
crime_by_type_all <- all %>% select(crime_type, Month,region) %>% 
  group_by(Month, crime_type, region) %>% summarise(value = n()) %>% 
  mutate(Month = ym(Month))

#time series for each crime in 3 years
c <- ggplot(crime_by_type_all, aes(x = Month, y = value, color = crime_type, color = region)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types Registered in London over 3 years")

ggplotly(c)


#smoothed  time series by crime type
crime_by_type_smoothed <- crime_by_type_all %>% group_by(crime_type, region) %>% 
  mutate(roll_mean_2month = scale(zoo::rollmean(value,2, na.pad = T)),
         roll_mean_3month = scale(zoo::rollmean(value,3, na.pad = T)),
         scaled_value = scale(value))

m <- ggplot(crime_by_type_smoothed, aes(x = Month, y = roll_mean_2month, color = crime_type)) + geom_line() + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Different Crime Types Registered in London over 3 years")

ggplotly(m)



                            #EACH CRIME SMOOTHED

#smoothed time series for Bicycle theft

crime_by_type_smoothed %>% filter(crime_type == "Bicycle theft") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) +  geom_smooth(span = 0.3, alpha = 0.2)+ theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Bicycle theft over 3 years")

#smoothed time series for Burglary
crime_by_type_smoothed %>% filter(crime_type == "Burglary") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region))  + geom_smooth(span = 0.3, alpha = 0.2)+ theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Burglary over 3 years")

#smoothed time series for Criminal damage and arson
crime_by_type_smoothed %>% filter(crime_type == "Criminal damage and arson") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2)+ theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Criminal Damage and Arson over 3 years")


#smoothed time series for Drugs
crime_by_type_smoothed %>% filter(crime_type == "Drugs") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region))  + geom_smooth(span = 0.3, alpha = 0.2) + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Drugs over 3 years")


#smoothed time series for Other crime
crime_by_type_smoothed %>% filter(crime_type == "Other crime") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2)  +
           theme_minimal() +ylab("Count") + xlab("Year")+ ggtitle("Other Crime over 3 years")


#smoothed time series for Other theft
crime_by_type_smoothed %>% filter(crime_type == "Other theft") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region))  + geom_smooth(span = 0.3, alpha = 0.2)+ theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Other theft over 3 years")


#smoothed time series for Possession of weapons
crime_by_type_smoothed %>% filter(crime_type == "Possession of weapons") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2) +
   theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Possession of Weapons over 3 years")


#smoothed time series for Public order
crime_by_type_smoothed %>% filter(crime_type == "Public order") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2)+ theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Public Order over 3 years")


#smoothed time series for Robbery
crime_by_type_smoothed %>% filter(crime_type == "Robbery") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2) + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Robbery over 3 years")



#smoothed time series for Shoplifting
crime_by_type_smoothed %>% filter(crime_type == "Shoplifting") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.2)  + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Shoplifting over 3 years")


#smoothed time series for Theft from the person
crime_by_type_smoothed %>% filter(crime_type == "Theft from the person") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region))  +geom_smooth(span = 0.3, alpha = 0.2) + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Theft from the Person over 3 years")


#smoothed time series for Vehicle crime
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) +geom_smooth(span = 0.3, alpha = 0.2) + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Vehicle crime over 3 years")


#smoothed time series for Violence and sexual offences
crime_by_type_smoothed %>% filter(crime_type == "Vehicle crime") %>% 
  ggplot(aes(x = Month, y = scaled_value, color = region)) +geom_smooth(span = 0.3, alpha = 0.2) + theme_minimal() +
  ylab("Count") + xlab("Year")+ ggtitle("Violence and sexual offences over 3 years")




                                          #BEFORE COVID #ALL CRIMES TOGETTHER


#aggregating crimes based on a month between 2018 and 2020 (before COVID-19 hit) first lockdown was introduced in march 2020
seasonality_total_2019 <- all %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month, region) %>% group_by(Month, region) %>% summarise(value = n()) 


seasonality_total_2019 <- seasonality_total_2019 %>% group_by(region) %>% mutate(scaled_value = scale(value))

#time series crimes until COVID-19 hit   
c1 <- ggplot(seasonality_total_2019, aes(x = Month, y = scaled_value, color = region)) + geom_smooth(span = 0.3, alpha = 0.3) + ylab("Count") + 
  xlab("Date") + theme_minimal() + ggtitle("Committed Crimes before Covid Pandemic")

ggplotly(c1)



######################################################################
                            #correlograms
######################################################################

                                        #all london

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- all_london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


#correlation of different crime types on monthly basis before COVID-19 hits
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis befor Covid 19 hits")



#correlation of different crime types on monthly basis 3yrs
seasonality_3yrs <- all_london_st %>% mutate(Month = ym(Month)) %>% select(crime_type, Month) %>%
  group_by(Month, crime_type) %>%  summarise(value = n())


corr1 = seasonality_3yrs_wide = seasonality_3yrs %>% pivot_wider(names_from = crime_type, values_from = value) %>% 
  ungroup() %>% select(-Month) %>% cor(use="pairwise.complete.obs")

corrplot::corrplot(corr1, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis Between 2018-2021")


#correlation of different crime types on monthly basis after people getting vaccinated and things goes back to normal
seasonality_by_type_after <- all_london_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month > ym("2021-03")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


corr2 = seasonality_by_type_after_long = seasonality_by_type_after %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr2, diag = FALSE, type = "upper")



                                #merseyside

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- merseyside_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


#correlation of different crime types on monthly basis before COVID-19 hits
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis befor Covid 19 hits")



#correlation of different crime types on monthly basis 3yrs
seasonality_3yrs <- merseyside_st %>% mutate(Month = ym(Month)) %>% select(crime_type, Month) %>%
  group_by(Month, crime_type) %>%  summarise(value = n())


corr1 = seasonality_3yrs_wide = seasonality_3yrs %>% pivot_wider(names_from = crime_type, values_from = value) %>% 
  ungroup() %>% select(-Month) %>% cor(use="pairwise.complete.obs")

corrplot::corrplot(corr1, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis Between 2018-2021")


#correlation of different crime types on monthly basis after people getting vaccinated and things goes back to normal
seasonality_by_type_after <- merseyside_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month > ym("2021-03")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


corr2 = seasonality_by_type_after_long = seasonality_by_type_after %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr2, diag = FALSE, type = "upper")

                                          #west midlands

#aggregating each crime type based on a month between 18 and 2020 (before COVID-19 hit)
seasonality_by_type_2019 <- west_midland_st %>% mutate(Month = ym(Month)) %>% 
  filter(Month < ym("2020-04")) %>%
  select(crime_type, Month) %>% group_by(Month, crime_type) %>% summarise(value = n()) 


#correlation of different crime types on monthly basis before COVID-19 hits
corr = seasonality_by_type_2019_long = seasonality_by_type_2019 %>% 
  pivot_wider(names_from = crime_type,  values_from = value) %>% 
  ungroup() %>% 
  select(-Month) %>% cor()

corrplot::corrplot(corr, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis befor Covid 19 hits")



#correlation of different crime types on monthly basis 3yrs
seasonality_3yrs <- west_midland_st %>% mutate(Month = ym(Month)) %>% select(crime_type, Month) %>%
  group_by(Month, crime_type) %>%  summarise(value = n())


seasonality_3yrs_wide = seasonality_3yrs %>% pivot_wider(names_from = crime_type, values_from = value) %>% 
  ungroup() %>% select(-Month) 

ggpairs(seasonality_3yrs_wide)

corrplot::corrplot(corr1, diag = FALSE, type = "upper")
#, title = "Correlogram of Different Crime Types on Monthly Basis Between 2018-2021")


seasonality_3yrs_wide %>% 
  ggplot(aes(x = `Criminal damage and arson`, y = `Violence and sexual offences`))+geom_point()


##################     Stop and Search Dataset     #################     
#################     data exploring / preparation    #################
all_london_ss <- rbind(metropolitan_ss, london_ss)

#aggregating outcome based on hour 
hourly <- all_london_ss %>% filter(Date < ym("2020-04")) %>% mutate(time = hour(Date)) %>% 
  group_by(Outcome, time) %>% summarise(value = n()) %>% group_by(time) %>% 
  mutate(total_stop = sum(value),
         rate = round(value/total_stop, 2)) %>% 
  na.omit() %>% 
  filter(total_stop > 50, Outcome != "A no further action disposal")



c3 <- ggplot(hourly, aes(x = time, y = rate, color = Outcome)) + geom_line() + xlab("hour") +
  ylab("Outcome Rate") + theme_minimal() + labs(title = "Different Serious Outcomes Rates per Hour for Metropolitan Police Service")
ggplotly(c3)


# age vs gender
all_london_ss <- all_london_ss %>%  mutate(age = as.factor(age))

ggplot(all_london_ss, aes( x = reorder(age, age, function(x)-length(x)), fill = Gender)) + geom_bar()

# hour vs age
ggplot(all_london_ss, aes(x = hour(Date), y = age, fill = Gender)) + geom_boxplot()+ theme_classic() +scale_fill_manual(values = c("deeppink", "cyan3", "grey")) +
  labs(title = "Greater London", caption = "Jan 2019 - Dec 2021")

# hour vs gender
ggplot(metropolitan_ss, aes(x = hour(Date), y = Gender)) + geom_boxplot() + theme_classic()
