
# Set Up Packages and Data ------------------------------------------------

# load packages
library(tidyverse, lib = "/projects/e30686/R/4.1")
library(arrow, lib = "/projects/e30686/R/4.1")
library(dplyr, lib = "/projects/e30686/R/4.1")
library(lubridate, lib = "/projects/e30686/R/4.1")
library(janitor)
## if janitor package is saying it is not found in shared package library, install it on your Quest and use the code below instead
# library(janitor)

library(sf) 
## sf package is not in shared package library on Quest
## users need to paste code below into their console to install sf ----
# Sys.setenv(INCLUDE = "/software/geos/3.8.1/include:/software/gcc/8.4.0/include:/software/proj/7.1.1/include")
# Sys.setenv(PKG_CONFIG_PATH = "/software/sqlite/3.27.2/lib/pkgconfig:/software/proj/7.1.1/lib/pkgconfig:/software/gdal/3.1.3-R-4.1.1/lib/pkgconfig")
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/geos/3.8.1/bin", old_path, sep = ":"))
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/proj/7.1.1/bin", old_path, sep = ":"))
# old_path <- Sys.getenv("PATH")
# Sys.setenv(PATH = paste("/software/gdal/3.1.3-R-4.1.1/bin", old_path, sep = ":"))
# install.packages("sf", repos="https://cloud.r-project.org/")

# if there are further issues with sf package, refer to https://kb.northwestern.edu/troubleshooting-installing-r-packages#SF

## Coordinate points into stations/
# read in data with lat-long coordinates you want matched to community area (station stats)
trip_data <- read_csv_arrow("/projects/e30686/data/raw/trip_data.csv")
stations_communites <- read_csv_arrow("/projects/e30686/data/Full Network Station Install Dates.csv")

# yearly data 
from_year <- trip_data %>% 
  mutate(
    year = year(start_time)
  ) %>% 
  count(year, from_station_name)

stations_communites <- stations_communites %>% 
  select(name, community_area, `install date`)

from_community <- merge(from_year, stations_communites, by.x = "from_station_name", by.y = "name")

to_year <- trip_data %>% 
  mutate(
    year = year(stop_time)
  ) %>% 
  count(year, to_station_name)

to_community <- merge(to_year, stations_communites, by.x = "to_station_name", by.y = "name")

pct_from_year <- from_year %>% 
  group_by(year) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  ) 

pct_to_year <- to_year %>% 
  group_by(year) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  )

pct_year <- left_join(
  pct_from_year, 
  pct_to_year, 
  by = c("from_station_name" = "to_station_name", "year" = "year")
) %>% 
  rename(
    station_name = from_station_name,
    from = n.x,
    pct_from = pct_rides.x,
    to = n.y,
    pct_to = pct_rides.y
  )

## final dataset
from_to_year <- left_join(
  from_year, 
  to_year, 
  by = c("from_station_name" = "to_station_name", "year" = "year")
) %>% 
  rename(
    station_name = from_station_name,
    from = n.x,
    to = n.y
  ) %>% 
  mutate(
    pct_return = 100 * to / from
  ) 

final_year <- left_join(
  from_to_year,
  pct_year,
  by = c("station_name", "year", "from", "to")
)

## use ----

data_2019 <- from_to_year %>% 
  filter(year == 2019)

data_2019_accesibility <- data_2019 %>% 
  filter(pct_return < 90) %>% 
  arrange(desc(pct_return))

# monthly data ----
from_month <- trip_data %>% 
  mutate(
    year = year(start_time),
    month = month(start_time)
  ) %>% 
  count(year, month, from_station_name)

from_community <- merge(from_month, stations_communites, by.x = "from_station_name", by.y = "name")

to_month <- trip_data %>% 
  mutate(
    year = year(stop_time),
    month = month(stop_time)
  ) %>% 
  count(year, month, to_station_name)

to_community <- merge(to_month, stations_communites, by.x = "to_station_name", by.y = "name")

## percentages 
pct_from_month <- from_month %>% 
  group_by(year, month) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  ) 

pct_to_month <- to_month %>% 
  group_by(year, month) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  )

pct_month <- left_join(
  pct_from_month, 
  pct_to_month, 
  by = c("from_station_name" = "to_station_name", "month" = "month", "year" = "year")
) %>% 
  rename(
    station_name = from_station_name,
    from = n.x,
    pct_from = pct_rides.x,
    to = n.y,
    pct_to = pct_rides.y
  )

from_to_month <- left_join(
  from_month, 
  to_month, 
  by = c("from_station_name" = "to_station_name", "month" = "month", "year" = "year")
) %>% 
  rename(
    station_name = from_station_name,
    from = n.x,
    to = n.y
  ) %>% 
  mutate(
    pct_return = 100 * to / from
  ) 

final_month <- left_join(
  from_to_month,
  pct_month,
  by = c("station_name", "month", "from", "to", "year")



