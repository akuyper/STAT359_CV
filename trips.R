
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

## load data
trip_data <- read_csv_arrow("/projects/e30686/data/raw/trip_data.csv")
station_community <- read_csv_arrow("/projects/e30686/data/Full Network Station Install Dates.csv")

## This dataset includes stations and their respective community area names
stations_communites <- station_community %>% 
  select(name, community_area, `install date`) %>% 
  mutate(
    install_date = as.Date(`install date`, "%m/%d/%Y"),
    install_year = year(install_date)
  ) %>%
  select(-c((`install date`), install_date))

# rides organized by year and stations where the rides departed
from_station <- trip_data %>% 
  mutate(
    year = year(start_time)
  ) %>% 
  count(year, from_station_name)

# rides organized by year and community areas where the rides departed
from_community <- merge(from_station, stations_communites, by.x = "from_station_name", by.y = "name")

# rides organized by year and stations where the rides arrived
to_station <- trip_data %>% 
  mutate(
    year = year(stop_time)
  ) %>% 
  count(year, to_station_name)

# rides organized by year and community areas where the rides arrived
to_community <- merge(to_station, stations_communites, by.x = "to_station_name", by.y = "name")

# percentage of rides from any stations out of the total rides that occured in each year
pct_from_station <- from_station %>% 
  group_by(year) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  ) 

# percentage of rides to any stations out of the total rides that occured in each year
pct_to_station <- to_station %>% 
  group_by(year) %>% 
  mutate(
    pct_rides = 100 * n / sum(n)
  )

## joined the above two percentages tables --> shows unique station names and percentages of rides that departed and arrived to it
pct_year <- left_join(
  pct_from_station, 
  pct_to_station, 
  by = c("from_station_name" = "to_station_name", "year" = "year")
) %>% 
  rename(
    station_name = from_station_name,
    from = n.x,
    pct_from = pct_rides.x,
    to = n.y,
    pct_to = pct_rides.y
  )

## yearly_rides ----
yearly_rides <- left_join(
  from_station, 
  to_station,
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

yearly_rides <- left_join(
  yearly_rides,
  pct_year,
  by = c("station_name", "year", "from", "to")
)

yearly_rides <- left_join(
  yearly_rides,
  stations_communites,
  by = c("station_name" = "name")
) %>% 
  select(year, station_name, from, to, pct_from, pct_to, pct_return, community_area, install_year)

write.csv(yearly_rides, 'my_data/yearly_rides.csv')

## accessibility example ----

accessibility_2019 <- yearly_rides %>% 
  filter(year == 2019) %>% 
  filter(pct_return < 90) %>% 
  arrange(desc(pct_return)) %>% 
  select(year, station_name, community_area, pct_return) 






