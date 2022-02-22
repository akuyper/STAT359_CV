# load package(s)
library(tidyverse)

## basic rides numbers, inner comm, between comm, by community

# load in data
rides <- read_csv("data/processed/station_hist_community_measures.csv")
View(rides)

# get timestamps sorted out for seasons and peak hours
rides <- rides %>% 
  mutate(timestamp = strptime(timestamp, "%m/%d/%Y %H:%M"))

rides <- rides %>% 
  mutate(month = as.numeric(format(timestamp, "%m")),
         year = as.numeric(format(timestamp, "%Y")),
         day = as.numeric(format(timestamp, "%d")),
         hour = as.numeric(format(timestamp, "%H"))) %>% 
  mutate(season = case_when(
    month %in% 1:2 ~ "Winter",
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    month == 12 ~ "Winter"
  ))

rides <- rides %>% 
  mutate(peak_hour = case_when(
    hour %in% 7:19 ~ "Yes",
    hour %in% 0:6 ~ "No",
    hour %in% 20:24 ~ "No"
  ))

# gets you average_daily emptiness
# View(rides %>% 
#   mutate(emptiness = (100 - percent_full)) %>% 
#   group_by(community, year, season, day, peak_hour) %>% 
#   mutate(avg_daily_emptiness = mean(emptiness, na.rm=TRUE)) %>% 
#   select(day, avg_daily_emptiness, emptiness))

# putting it all together
rides_1 <- rides %>% 
  mutate(emptiness = (100 - percent_full)) %>% 
  group_by(community, year, season, peak_hour) %>% 
  mutate(avg_seasonal_emptiness = mean(emptiness, na.rm=TRUE)) 

# how many stations in each community area
rides_station_num <- rides_1 %>% 
  group_by(year, community) %>% 
  select(year, community, station_name) %>% 
  unique() %>% 
  count(community) %>%
  mutate(num_stations_in_comm = n) %>% 
  select(-n)

# merging back with original dataset
rides_1 <- merge(rides_1, rides_station_num, by=c("year","community"))

rides_1 <- rides_1 %>% 
  group_by(community, year, season, peak_hour) %>% 
  select(community, year, num_stations_in_comm, season, peak_hour, avg_seasonal_emptiness) %>% 
  unique()

write_csv(rides_1, "data/processed/ride_by_comm_season_pkhrs_avgseasonalemptiness.csv")

# adding in bike availability
rides_2 <- rides %>% 
  group_by(community, year, season, peak_hour) %>% 
  mutate(avg_seasonal_bike_avail = mean(available_bikes, na.rm=TRUE)) 

rides_2 <- merge(rides_2, rides_station_num, by=c("year","community"))

rides_2 <- rides_2 %>% 
  group_by(community, year, season, peak_hour) %>% 
  select(community, year, num_stations_in_comm, season, peak_hour, avg_seasonal_bike_avail) %>% 
  unique()

# merge both files to get final summary document
rides_final <- merge(rides_1, rides_2, by=c("year","community", "num_stations_in_comm", "season", "peak_hour"))

write_csv(rides_final, "data/processed/ride_by_comm_season_pkhrs_avgseasonalemptiness_bikeavail.csv")
