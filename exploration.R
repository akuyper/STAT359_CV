# load package(s)
library(tidyverse)

## basic rides numbers, inner comm, between comm, by community

# load in data
rides <- read_csv("data/processed/rides_community.csv")
View(rides)

rides_station <- rides %>% 
  group_by(from_station_name) %>% 
  dplyr::select(from_station_name, from_community)

write_csv(rides_station, "data/processed/stations_by_community.csv")

# seasons and years variables created
rides <- rides %>% 
  mutate(start_time = strptime(start_time, "%m/%d/%Y %M:%S"))

rides <- rides %>% 
  mutate(month = as.numeric(format(start_time, "%m")),
         year = as.numeric(format(start_time, "%Y"))) %>% 
  mutate(season = case_when(
    month %in% 1:2 ~ "Winter",
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    month == 12 ~ "Winter"
  ))

# created within and between comms categorical variable
rides <- rides %>% 
  mutate(same_comm = ifelse(from_community == to_community, "within_comm_rides", "inter_comm_rides")) 

# skimr::skim_without_charts(rides)

## organized by year and by community
rides_yr <- rides %>% 
  group_by(year, from_community) %>% 
  count(same_comm) %>% 
  pivot_wider(names_from = same_comm, values_from = n)

rides_yr <- rides_test %>% 
  mutate(total_rides = within_comm_rides + inter_comm_rides)

View(rides_yr)

write_csv(rides_yr, "data/processed/ride_numbers_by_yr_by_community.csv")

## organized by season, year, and community
rides_s <- rides %>% 
  group_by(season, year, from_community) %>% 
  count(same_comm) %>% 
  pivot_wider(names_from = same_comm, values_from = n)

rides_s <- rides_s %>% 
  mutate(total_rides = within_comm_rides + inter_comm_rides)

View(rides_s)

write_csv(rides_s, "data/processed/ride_numbers_by_season_by_year_by_community.csv")

# broad rides by community analysis
 rides_comm <- rides %>% 
  group_by(from_community) %>% 
  count(same_comm) %>% 
  pivot_wider(names_from = same_comm, values_from = n)
 
 rides_comm <- rides_comm %>% 
   mutate(total_rides = within_comm_rides + inter_comm_rides)
 
 View(rides_comm)

 write_csv(rides_comm, "data/processed/ride_numbers_by_community.csv")
 
 
 