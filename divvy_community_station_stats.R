# Purpose: ----------------------------------------------------------------

# This R Script allows you to process Divvy historical station data, 
# match them to Chicago community boundaries, and aggregate daily 
# and seasonal averages across the five station metrics of 
# total_docks, docks_in_service, available_docks, available_bikes, and percent_full.
# There is also commented out code at the very bottom of this file that can aggregate the 
# processed data to a yearly average as well.

# Set Up Packages and Data ------------------------------------------------

# load packages
library(tidyverse, lib = "/projects/e30686/R/4.1")
library(arrow, lib = "/projects/e30686/R/4.1")
library(dplyr, lib = "/projects/e30686/R/4.1")
library(lubridate, lib = "/projects/e30686/R/4.1")
library(janitor, lib = "/projects/e30686/R/4.1")
## if janitor package is saying it is not found in shared package library, install it on your Quest and use the code below instead
# library(janitor)

## sf package is not in shared package library on Quest
## users need to paste code below into their console to install sf 
# Sys.setenv(INCLUDE = "/software/geos/3.8.1/include:/software/gcc/8.4.0/include:/software/proj/7.1.1/include")
# Sys.setenv(PKG_CONFIG_PATH = "/software/sqlite/3.27.2/lib/pkgconfig:/software/proj/7.1.1/lib/pkgconfig:/software/gdal/3.1.3-R-4.1.1/lib/pkgconfig")
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/geos/3.8.1/bin", old_path, sep = ":"))
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/proj/7.1.1/bin", old_path, sep = ":"))
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/gdal/3.1.3-R-4.1.1/bin", old_path, sep = ":"))
# install.packages("sf", repos="https://cloud.r-project.org/")
library(sf)

# if there are further issues with sf package, refer to https://kb.northwestern.edu/troubleshooting-installing-r-packages#SF

# read in data with lat-long coordinates you want matched to community area (station stats)
point_data <- read_csv_arrow("/projects/e30686/data/raw/divvy_2021.csv") 
  
point_data <- point_data %>% 
  # # remove row number column -- might need this added in for some years
  # dplyr::select(-1) %>%
  # clean column names to be in lower case, workable format
  janitor::clean_names() 

# Map Stations to Community Areas -----------------------------------------

# read in shape file that outlines community area boundaries
geo_data <- read_sf("/projects/e30686/data/geo_export_279165a1-c897-4e25-b141-d5fc4a4996ec.shp")

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

# get timestamp read in with correct type and format
stations <- stations %>% 
  mutate(timestamp = strptime(timestamp, "%Y-%m-%d %H:%M:%S"))

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
  ## if you only want peak hours
  # filter(peak_hour == "Yes") %>% 
  ## extract only dates from timestamp
  ## date format for grouping: YYYY-MM-DD
  mutate(
    date = lubridate::date(timestamp)     
  ) %>% 
  # group data by all the different characteristics you want listed out/make data distinctive for
  group_by(peak_hour, date, community, id) %>% 
  # list out all station variables to aggregate averages across entire community area
  dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
  # automatically keeps grouping vars
  # calculating station daily averages
  summarise(
    # can do median by switching "mean" to "median"
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
write_csv(daily_community_station, "/projects/e30686/data/processed/divvy_community_station_daily_stats_2021.csv")

# seasonal stat averages by community calculated
seasonal_community_station <- stations %>% 
  ## if you only want peak hours
  # filter(peak_hour == "Yes") %>% 
  ## if you just want year, can take season variable out from line below
  group_by(peak_hour, year, season, community, id) %>% 
  # list out all station variables to aggregate averages across entire community area
  dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
  # automatically keeps grouping vars
  # calculating station daily averages
  summarise(
    # can do median by switching mean to median
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
write_csv(seasonal_community_station, "/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2021.csv")


# Merge all years into 1 dataset ------------------------------------------

## merge yearly community daily stats into 1 csv file

# read in all processed datasets for each year
# filter in place to make sure the same dates are not double counted across multiple years
# which would help avoid duplicate rows when binding the datasets together
# for example, making sure 2013 data that goes from 1/01/2013 does not continue into 1/01/2014, as 2014 data will have that
d13 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2013.csv") %>% 
  filter(date != "2014-01-01")
d14 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2014.csv") %>% 
  filter(date != "2015-01-01")
d15 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2015.csv") %>% 
  filter(date != "2016-01-01")
d16 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2016.csv") %>% 
  filter(date != "2017-01-01")
d17 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2017.csv") %>% 
  filter(date != "2018-01-01") %>% 
  filter(date != "2016-12-31")
d18 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2018.csv") %>% 
  filter(date != "2019-01-01")
d19 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2019.csv") %>% 
  filter(date != "2020-01-01")
d20 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2020.csv") %>% 
  filter(date != "2021-01-01")
d21 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_daily_stats_2021.csv") %>% 
  filter(date != "2022-01-01")


# bind them into one
divvy_allyrs_daily <- rbind(d13, d14, d15, d16, d17, d18, d19, d20, d21)

# adjustments before final data can be printed out (this step should be done even if only using one year's data only)
divvy_allyrs_daily <- divvy_allyrs_daily %>% 
  # no docks in service, that means the station has not opened yet or had measurement issues, so we are taking out these rows
  filter (docks_in_service != 0) %>% 
  # percent_full is percentage of the docks in service containing an available bike
  # recalculated to adjust for any negative numbers that are coded in if denominator is 0
  mutate(percent_full = available_bikes/docks_in_service*100)

# write out to a csv file
write_csv(divvy_allyrs_daily, "/projects/e30686/data/processed/divvy_allyrs_daily.csv")

## merge yearly community seasonal stats into 1 csv file

# read in all processed datasets for each year
# filter in place to make sure the same dates are not double counted across multiple years
# which would help avoid duplicate rows when binding the datasets together
d13 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2013.csv") %>% 
  filter(year == "2013")
d14 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2014.csv") %>% 
  filter(year == "2014")
d15 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2015.csv") %>% 
  filter(year == "2015")
d16 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2016.csv") %>% 
  filter(year == "2016")
d17 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2017.csv") %>% 
  filter(year == "2017") 
d18 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2018.csv") %>% 
  filter(year == "2018")
d19 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2019.csv") %>% 
  filter(year == "2019")
d20 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2020.csv") %>% 
  filter(year == "2020")
d21 <- read_csv_arrow("/projects/e30686/data/processed/divvy_community_station_seasonal_stats_2021.csv") %>% 
  filter(year == "2021")

# bind them into one
divvy_allyrs_seasonal <- rbind(d13, d14, d15, d16, d17, d18, d19, d20, d21)

# adjustments before final data can be printed out (this step should be done even if only using one year's data only)
divvy_allyrs_seasonal <- divvy_allyrs_seasonal %>% 
  # no docks in service, that means the station has not opened yet or had measurement issues, so we are taking out these rows
  filter (docks_in_service != 0) %>% 
  # percent_full is percentage of the docks in service containing an available bike
  # recalculated to adjust for any negative numbers that are coded in if denominator is 0
  mutate(percent_full = available_bikes/docks_in_service*100)

# write out to a csv file
write_csv(divvy_allyrs_seasonal, "/projects/e30686/data/processed/divvy_allyrs_seasonal.csv")


# Aggregate Divvy historical data into year level only --------------------

# divvy_allyrs <- divvy_allyrs_seasonal %>% 
#   group_by(peak_hour, year, community) %>% 
#   # list out all station variables to aggregate averages across entire community area
#   dplyr::select(total_docks, docks_in_service, available_docks, available_bikes, percent_full) %>% 
#   # automatically keeps grouping vars
#   # calculating station daily averages
#   summarise(
#     # can do median by switching mean to median
#     across(where(is.numeric), mean)
#   )
