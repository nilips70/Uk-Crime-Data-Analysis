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
library(psych)
rm(list=ls())

# Reading data
west_midland_st <- readRDS("west_midland_st.rds")
merseyside_st <- readRDS("merseyside_st.rds")
all_london_st <- readRDS("all_london_st.rds")

#data preparation
west_midland_st <- west_midland_st %>% mutate(region = "W")
merseyside_st <- merseyside_st %>% mutate(region = "M")
all_london_st <- all_london_st %>% mutate(region = "L")

all <- rbind(west_midland_st, merseyside_st, all_london_st)


#####################################################################
# ============================== London =============================
#####################################################################

all_wide = all_london_st %>% 
  filter(crime_type != "Other crime") %>% select(Month, crime_type) %>% group_by(Month,crime_type) %>% 
  summarise(value = n()) %>% pivot_wider(names_from = crime_type, values_from = value) %>% ungroup() %>% 
  select(-Month)

corrplot::corrplot(cor(all_wide[,-1]), type = "upper", order = 'hclust')

fafitfree <- fa(all_wide,nfactors = ncol(all_wide), rotate = "none")

n_factors <- length(fafitfree$e.values)

scree <- data.frame(Factor_n =  as.factor(1:n_factors), Eigenvalue = fafitfree$e.values)

# Scree plot
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() + xlab("Number of factors") +
  ylab("Initial eigenvalue") + labs( title = "Scree Plot")

# Factor analysis
factanal <- factanal(all_wide, factors=4, scores = c("regression"), rotation = "varimax")
print(factanal)


fa.none <- fa(r=all_wide, 
              nfactors = 4, 
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax")

fa.diagram(fa.none)

#####################################################################
# ============================== Merseyside ==========================
#####################################################################

all_wide = merseyside_st %>% filter(crime_type != "Other crime") %>% select(Month, crime_type) %>% 
  group_by(Month,crime_type) %>% summarise(value = n()) %>% 
  pivot_wider(names_from = crime_type,values_from = value) %>% 
  ungroup() %>% select(-Month)

corrplot::corrplot(cor(all_wide[,-1]), type = "upper", order = 'hclust')

fafitfree <- fa(all_wide,nfactors = ncol(all_wide), rotate = "none")

n_factors <- length(fafitfree$e.values)

scree <- data.frame(Factor_n =  as.factor(1:n_factors), Eigenvalue = fafitfree$e.values)

# Scree plot
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  xlab("Number of factors") + ylab("Initial eigenvalue") + labs( title = "Scree Plot")

# Factor analysis
factanal <- factanal(all_wide, factors=4, scores = c("regression"), rotation = "varimax")
print(factanal)


fa.none <- fa(r=all_wide, 
              nfactors = 4, 
              fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
              max.iter=100, # (50 is the default, but we have changed it to 100
              rotate="varimax")

fa.diagram(fa.none)


#####################################################################
# ============================== West Midlands ======================
#####################################################################

all_wide = west_midland_st %>% filter(crime_type != "Other crime") %>% 
  select(Month, crime_type) %>% group_by(Month,crime_type) %>% 
  summarise(value = n()) %>% pivot_wider(names_from = crime_type,
              values_from = value) %>% ungroup()

corrplot::corrplot(cor(all_wide[,-1]), type = "upper", order = 'hclust')
# 
# fafitfree <- fa(all_wide,nfactors = ncol(all_wide), rotate = "none")
# n_factors <- length(fafitfree$e.values)
# scree     <- data.frame(
#   Factor_n =  as.factor(1:n_factors), 
#   Eigenvalue = fafitfree$e.values)

# Scree plot
# ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
#   geom_point() + geom_line() +
#   xlab("Number of factors") +
#   ylab("Initial eigenvalue") +
#   labs( title = "Scree Plot")

## Factor analysis
#all_wide2 = all_wide %>% 
#  select(-c("Criminal damage and arson","Bicycle theft", Month))
#
#factanal <- factanal(all_wide2, factors=2, scores = c("regression"), rotation = "varimax")
#print(factanal)
#
#new_data = factanal$scores
#new_data = as.data.frame(new_data)
#names(new_data) = c("Theft", "Offense")
#new_data$Month = all_wide$Month
#new_data$Month = ym(new_data$Month)
#
#new_data %>%
#  pivot_longer(names_to = "kind",
#               values_to = "value",
#               -Month) %>% 
#ggplot() + geom_line(aes(Month, value, color = kind))


# 
# fa.none <- fa(r=all_wide, 
#               nfactors = 3, 
#               fm="pa", # type of factor analysis we want to use (“pa” is principal axis factoring)
#               max.iter=100, # (50 is the default, but we have changed it to 100
#               rotate="varimax")
# 
# fa.diagram(fa.none)

