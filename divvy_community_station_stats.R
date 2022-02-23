
# Set Up Packages and Data ------------------------------------------------

# load packages
library(tidyverse)
library(sf)
library(arrow)

# read in data with lat-long coordinates you want matched to community area (station stats)
point_data <- read_csv_arrow("data/divvy2013_sample.csv") %>% 
  
point_data <- point_data %>% 
  # # remove row number column -- might need this added in for some years
  # dplyr::select(-1) %>%
  # clean column names to be in lower case, workable format
  janitor::clean_names() 

# Map Stations to Community Areas -----------------------------------------

# read in shape file that outlines community areas
geo_data <- read_sf("data/geo_export_279165a1-c897-4e25-b141-d5fc4a4996ec.shp")

# assign_if_not_in is a string that is put in the community area column if the point from point_data 
# does not fall into any of the listed community areas
assign_if_not_in <- c("Not Listed")

# long_lat_vec should be a vector of the names of your longitude and latitude columns, in that order
long_lat_vec = c("longitude", "latitude")

# function to map data to community area based on lat-long coordinates
map_point_to_geo <- function(point_data, geo_data, community, assign_if_not_in, long_lat_vec){
  
  point_data_sf <- st_as_sf(point_data, coords = long_lat_vec, crs = st_crs(geo_data))
  
  #intersection column 
  point_data <- point_data_sf %>% 
    mutate(intersection = as.integer(st_intersects(geometry, geo_data)),
           community = if_else(is.na(intersection), '', geo_data[[community]][intersection])) 
  
  point_data_done <- tibble(point_data) %>%
    mutate(community = ifelse(is.na(intersection), assign_if_not_in, community)) %>%
    dplyr::select(-intersection)
  
  return(point_data_done)
}

# calling function to map our station stats data to community area based on lat-long coordinates
stations <- map_point_to_geo(point_data, geo_data, "community", "None", c("longitude", "latitude"))


# Analyze Station Daily Avg Stats by Community ----------------------------

# get timestamp made into correct format 
stations <- stations %>% 
  mutate(timestamp = strptime(timestamp, "%m/%d/%Y %H:%M"))

# sort timestamp into seasons and peak hours columns
stations <- stations %>% 
  mutate(month = as.numeric(format(timestamp, "%m")),
         year = as.numeric(format(timestamp, "%Y")),
         day = as.numeric(format(timestamp, "%d")),
         hour = as.numeric(format(timestamp, "%H"))) %>% 
  mutate(season = case_when(
    # set seasons based on months
    month %in% 1:2 ~ "Winter",
    month %in% 3:5 ~ "Spring",
    month %in% 6:8 ~ "Summer",
    month %in% 9:11 ~ "Fall",
    month == 12 ~ "Winter"
  ))

stations <- stations %>% 
  mutate(peak_hour = case_when(
    # set peak hours as from 7am - 7pm
    hour %in% 7:19 ~ "Yes",
    hour %in% 0:6 ~ "No",
    hour %in% 20:24 ~ "No"
  ))

# daily stat averages by community calculated
daily_community_station <- stations %>% 
  # # if you only want peak hours
  # filter(peak_hour == "Yes") %>% 
  # # date format for grouping: YYYY-MM-DD
  mutate(
    date = date(timestamp)     
  ) %>% 
  group_by(peak_hour, date, community, id) %>% 
  # variables to average
  dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
  # automatically keeps grouping vars
  # calculating station daily averages
  summarise(
    # can do median by switching "mean" with "median"
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

# # viewing the data before it gets written out into a csv
# View(daily_community_station)

# write out analyzed data into a workable csv file
write_csv(daily_community_station, "data/processed/divvy_community_station_daily_stats.csv")

# seasonal stat averages by community calculated
seasonal_community_station <- stations %>% 
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

# # viewing the data before it gets written out into a csv
# View(seasonal_community_station)

# write out analyzed data into a workable csv file
write_csv(seasonal_community_station, "data/processed/divvy_community_station_seasonal_stats.csv")
