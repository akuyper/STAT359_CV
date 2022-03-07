# Divvy density data
# Purpose -----------------
# Calculates density measures rolled up to both the community and station level. 
# Here, the radius is 2 miles, but can be changed to any distance

# load packages ---- 


# only use the lib.loc = lib part if you're using a shared library

lib <- '/projects/e30686/R/4.1'



library(tidyverse,  lib.loc = lib)
library(lubridate,  lib.loc = lib)
library(sp, lib.loc = lib)
library(rgdal, lib.loc = lib)
library(raster) 
library(udunits2) 
library(readxl,  lib.loc = lib)



# Load in and clean data ---
# data is directly from CDOT and was last updated 12/29/2021

station_dates <- read_csv("mydata/Full Network Station Install Dates.csv") %>% 
  select(-'...1')
                                                                                
# dataframe that has each station name, latitude, longitude, and community_area. -----

station_location <- station_dates %>% 
  dplyr::select(c('name', 'lat', 'lon', 'community_area')) %>% 
  unique()

# merging station_location to itself to find every pair of stations -----

duplicated_station_data <- station_location %>% 
  merge(station_location, by = NULL) %>% 
  filter(
    name.x != name.y)

# finding the distance between each pair of stations and converting meters to miles -----

duplicated_station_data$distance <- pointDistance(duplicated_station_data[, c('lon.x', 'lat.x')],duplicated_station_data[, c('lon.y', 'lat.y')],  lonlat = TRUE) #output is meters
duplicated_station_data$miles <- ud.convert(duplicated_station_data$distance, 'meters', 'miles') #meters --> miles


# Finding the density as of 2021 for each station -----

stations_density_2021 <- duplicated_station_data %>%
  dplyr::select(c('name.x', 'miles')) %>% #only selecting the station name and the distance between this station and station2 
  filter(miles <= 2) %>% #less than 2 mile radius
  group_by(name.x) %>% #grouping for each station
  summarise(in_2_mi_radius = n()) %>% #counting how many stations are within a 2 mile radius
  rename(station_name = name.x) %>%
  left_join(station_location, by = c("station_name" = 'name')) #left-joining the original information about each station (lat, lon, community area)

# Writing out the file, station density ----------

#change output path to whatever is necessary
write.csv(stations_density_2021, 'mydata/processed/stations_density_2021.csv')

# Finding average density for community area by averaging the stations' densities -----

comm_density_2021 <- stations_density_2021 %>%
  group_by(community_area) %>% #grouping by community area
  summarise(avg_in_2_mi_radius = round(mean(in_2_mi_radius),2)) 

# Writing out the file, communtiy density ----------

write.csv(comm_density_2021, 'mydata/processed/comm_density_2021.csv')
