library(tidyverse)
library(dplyr)

## read in rides by communities data
trips <- read.csv("data/processed/rides_community.csv")

# sample data because lag ----
trips_sample <- trips %>% 
  head(1000)

# select only the columns we're interested in
where <- trips_sample %>% 
  select(to_station_name, from_station_name, to_community, from_community)


### STATIONS ----
# To and From stations/communities and # of rides from stations
where %>% 
  group_by(to_station_name) %>% 
  mutate(end = n()) %>% 
  group_by(from_station_name) %>% 
  mutate(start = n()) %>% 
  select(to_station_name, from_station_name, end, start) 

# station name and # of rides ended at that station
to_station <- where %>% 
  group_by(to_station_name) %>% 
  summarise(end = n()) %>% 
  select(to_station_name, end) %>% 
  group_by(to_station_name) %>% 
  rename(station = to_station_name)

# station name and # of rides started at that station
from_station <- where %>% 
  group_by(from_station_name) %>% 
  summarise(start = n()) %>% 
  select(from_station_name, start) %>% 
  group_by(from_station_name) %>% 
  rename(station = from_station_name)

# combined above two tables
stations <- merge(from_station, to_station, by = "station")

### COMMUNITIES ----

# station name and # of rides ended at that station
to_community_name <- where %>% 
  group_by(to_community) %>% 
  summarise(end = n()) %>% 
  select(to_community, end) %>% 
  group_by(to_community) %>% 
  rename(community = to_community)

# station name and # of rides started at that station
from_community_name <- where %>% 
  group_by(from_community) %>% 
  summarise(start = n()) %>% 
  select(from_community, start) %>% 
  group_by(from_community) %>% 
  rename(community = from_community)

community <- merge(from_community_name, to_community_name, by = "community")


##### community names and number of stations in each community ----

number_of_stations <- where %>% 
  distinct(to_station_name, to_community) %>% 
  group_by(to_community) %>% 
  summarise(stations = n()) %>% 
  rename(community = to_community)

communities <- merge(community, number_of_stations, by = "community")


#### TRIP DATA----

Where <- trips %>% 
  select(to_station_name, from_station_name, to_community, from_community)

### STATIONS ----
# To and From stations/communities and # of rides from stations
Where %>% 
  group_by(to_station_name) %>% 
  mutate(end = n()) %>% 
  group_by(from_station_name) %>% 
  mutate(start = n()) %>% 
  select(to_station_name, from_station_name, end, start) 

# station name and # of rides ended at that station
To_Station <- Where %>% 
  group_by(to_station_name) %>% 
  summarise(end = n()) %>% 
  select(to_station_name, end) %>% 
  group_by(to_station_name) %>% 
  rename(station = to_station_name)

# station name and # of rides started at that station
From_Station <- Where %>% 
  group_by(from_station_name) %>% 
  summarise(start = n()) %>% 
  select(from_station_name, start) %>% 
  group_by(from_station_name) %>% 
  rename(station = from_station_name)

# combined above two tables
Stations <- merge(From_Station, To_Station, by = "station")

write_csv(Stations, "data/processed/trips_stations.csv")

### COMMUNITIES ----
# select only the columns we're interested in

To_Community_Name <- Where %>% 
  group_by(to_community) %>% 
  summarise(end = n()) %>% 
  select(to_community, end) %>% 
  group_by(to_community) %>% 
  rename(community = to_community)

# station name and # of rides started at that station
From_Community_Name <- Where %>% 
  group_by(from_community) %>% 
  summarise(start = n()) %>% 
  select(from_community, start) %>% 
  group_by(from_community) %>% 
  rename(community = from_community)

Community <- merge(From_Community_Name, To_Community_Name, by = "community")

##### community names and number of stations in each community

Number_Of_Stations <- Where %>% 
  distinct(to_station_name, to_community) %>% 
  group_by(to_community) %>% 
  summarise(stations = n()) %>% 
  rename(community = to_community)

Communities <- merge(Community, Number_Of_Stations, by = "community")

write_csv(Communities, "data/processed/trips_community.csv")



