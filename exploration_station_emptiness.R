# load package(s)
library(tidyverse)
library(arrow)

# load in data
rides <- read_csv_arrow("data/processed/station_hist_community_measures.csv")
# View(rides)

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

daily_community_station <- rides %>% 
  # if you only want peak hours
  # filter(peak_hour == "Yes") %>% 
  # date format for grouping: YYYY-MM-DD
  mutate(
    date = date(timestamp)     
  ) %>% 
  group_by(peak_hour, date, community, id) %>% 
  # variables to average
  dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
  # automatically keeps grouping vars
  # calculating station daily averages
  summarise(
    # can do median by switching mean with median
    across(where(is.numeric), mean)
  ) %>% 
  # automatically ungroups by last grouping var; follow up by removing it
  dplyr::select(-id) %>%
  # calculating community station daily averages
  summarise(
    num_stations = n(),
    across(where(is.numeric), mean)
  ) %>% 
  ungroup()

View(daily_community_station)

seasonal_community_station <- rides %>% 
  # if you only want peak hours
  # filter(peak_hour == "Yes") %>% 
  # if you just want year, can take season variable out from line below
  group_by(peak_hour, year, season, community, id) %>% 
  # variables to average
  dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
  # automatically keeps grouping vars
  # calculating station daily averages
  summarise(
    # can do median by switching mean with median
    across(where(is.numeric), mean)
  ) %>% 
  # automatically ungroups by last grouping var; follow up by removing it
  dplyr::select(-id) %>%
  # calculating community station daily averages
  summarise(
    num_stations = n(),
    across(where(is.numeric), mean)
  ) %>% 
  ungroup()

View(seasonal_community_station)


# # merge both files to get final summary document
# rides_final <- merge(rides_1, rides_2, by=c("year","community", "num_stations_in_comm", "season", "peak_hour", "day"))
# 
# write_csv(rides_final, "data/processed/ride_by_comm_season_pkhrs_avgdailyemptiness_bikeavail.csv")