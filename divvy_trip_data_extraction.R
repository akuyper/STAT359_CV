# Divvy trip data extraction
# Purpose -----------------
# Grabs the data using the API for the trip data. Trip data hasn't been calculated since the end of 2020 so you will only need to grab the data once for your entire project going forward, unless divvy changes it.
# It follows directly from the divvy api script, but the URL changes
# Before starting, you need to get an app_token using the data.cityofchicago portal
# Steps on making an apptoken for the API:
# 1. Go to https://data.cityofchicago.org/login
# 2. Make an account or sign in
# 3. Press the little pen next to your username to edit your profile
# 4. Navigate to the "Developer Settings" Page using the sidebar menu
# 5. Click on the "Create New App Token" and fill out the popup accordingly. Keep it private
# 6. Copy your App Token and paste it into this script
# Also, this app token is custom to you and your account, so make sure you don't share it to others 

# load packages from shared library, change if needed

lib <- '/projects/e30686/R/4.1'

# load packages ----
# only use the lib.loc = lib part if you're using a shared library

library("RSocrata", lib.loc = lib)
library("tidyverse", lib.loc = lib)
library('lubridate', lib.loc = lib)
library('filesstrings', lib.loc = lib)

# Define pathways and parameters ----
# change when needed
#symbolic link/folder name 

my_folder <- "mydata"


# year nothing prior to 2013

input_year <- 2013
first_day <- str_c(input_year, "-01-01")
last_day <- str_c(input_year + 7, "-12-31") #pulls the entire range of data, change if divvy changes it

# write file to outpath

out_path <- str_c(my_folder,"/raw/trip_data.csv")


# Define apptoken using the process outlined above, should be a string

apptoken <- #insert app token here
  
# Define function for downloading data ----

get_data <- function(url){
  read.socrata(
    url,
    app_token = apptoken,
  ) %>%
    as_tibble()
}

# setup tibble for downloading and storing data ----

trips_db <- tibble(
  day = seq(ymd(first_day), ymd(last_day), by='day'),
  url_input = str_c(
    "https://data.cityofchicago.org/resource/fg6s-gzvg.csv?$where=start_time between ",
    "\'",
    day,
    "T00:00:00\' and \'",
    day,
    "T23:59:59\'"
  )
)

# collect daily data

trips_db <- trips_db %>%
  mutate(
    data = map(url_input, ~ get_data(url = .x))
  )

# remove necessary columns ----

trips_db <- trips_db %>%
  mutate(dim_check = map_int(data, nrow)) %>%
  filter(dim_check > 0) %>%
  unnest(data) %>%
  select(-url_input, -day, -dim_check)

# write out the data

write_csv(trips_db, out_path)

