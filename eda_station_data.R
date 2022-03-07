
# Purpose -----------------------------------------------------------------

# preliminary insights and visualizations from processed Divvy core data

# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(janitor)
library(readxl)
library(sf)
library(viridis)

# read in your data
divvy_data <- read_csv("data/processed/divvy_allyrs_seasonal.csv")

# manipulate data so there is only one row for each community
# I've chosen to look at metrics during peak hours for summer 2021
d1 <- divvy_data %>% 
  filter(peak_hour == "Yes",
         season == "Summer",
         year == "2021") %>% 
  mutate(community = tolower(community))

# read in shape file
communities <- read_sf('data/geo_export_279165a1-c897-4e25-b141-d5fc4a4996ec.shp')

# match community format of shape file to that of divvy_data
communities <- communities %>% 
  mutate(community = tolower(community))

# join the two datasets together
divvy_count <- d1 %>%
  group_by(community) %>%
  left_join(communities, by = c('community' = 'community'))

data1 <- read_excel("data/neighborhood_info.xlsx")

# adjusting data1 so it can be joined
data1 <- data1 %>%
  mutate(neighborhood = tolower(neighborhood)) %>%
  rename(community = neighborhood)
divvy_data1 <- divvy_count %>%
  left_join(data1)
# finding the center of each community for geom_point
divvy_data1 <- divvy_data1 %>%
  mutate(geometry_center = st_centroid(geometry))
divvy_data1 <- divvy_data1 %>%
  mutate(lat_lon = st_coordinates(geometry_center),
         lat = lat_lon[,"X"],
         long = lat_lon[,"Y"])

# heat map of average percent_full for Chicago communities in 2021 summer peak hours
divvy_data1 %>%
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = percent_full)) +
  theme_void() +
  scale_fill_viridis(trans = 'reverse') 

# table with Chicago communities ranked on percent_full from highest to lowest in 2021 summer peak hours
divvy_data1 %>% 
  arrange(desc(percent_full)) %>% 
  select(community, num_stations, percent_full, docks_in_service)

# heat map of bike availability on Chicago communities in 2021 summer peak hours
divvy_data1 %>%
  ggplot() +
  geom_sf(mapping = aes(geometry = geometry, fill = available_bikes)) +
  theme_void() +
  scale_fill_viridis(trans = 'reverse') 

# table with Chicago communities ranked on bike availability from highest to lowest in 2021 summer peak hours
divvy_data1 %>% 
  arrange(desc(available_bikes)) %>% 
  select(community, num_stations, available_bikes, docks_in_service)
