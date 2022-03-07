# Divvy station data extraction 
# Purpose -----------------
# Grabs the data using the API for the divvy historical stations data. This script will grab the data for an entire year, based on the value of 'input_year'.
# The timestamp for the data can be tricky since it's pulled in UTC but between midnights in CST, so the last few lines of code changes UTC to CST.
# Before starting, you need to get an app_token using the data.cityofchicago portal
# Steps on making an apptoken for the API:
# 1. Go to https://data.cityofchicago.org/login
# 2. Make an account or sign in
# 3. Press the little pen next to your username to edit your profile
# 4. Navigate to the "Developer Settings" Page using the sidebar menu
# 5. Click on the "Create New App Token" and fill out the popup accordingly. Keep it private
# 6. Copy your App Token and paste it into this script
# Also, this app token is custom to you and your account, so make sure you don't share it to others 


# load packages from shared library, not necessary if not using QUEST or a shared library ------
lib <- '/projects/e30686/R/4.1'

# load packages ----
library("RSocrata", lib.loc = lib)
library("tidyverse", lib.loc = lib)
library('lubridate', lib.loc = lib)
library('filesstrings', lib.loc = lib)

# Define pathways and parameters ----

# folder to put data files in
my_folder <- "mydata"


# year nothing prior to 2013 -----
input_year <- 2014
first_day <- str_c(input_year, "-01-01")
last_day <- str_c(input_year, "-12-31")

# write file to path ----

out_path <- str_c(my_folder,"/raw/","divvy_", input_year, ".csv")

# Define apptoken using the process outlined above, should be a string -----

apptoken <- #insert app token here
  
# Define function for downloading data ----
get_data <- function(url){
  read.socrata(
    url,
    app_token = apptoken, #fill in with your own API token
  ) %>% 
    as_tibble()
}

# setup tibble for downloading and storing data ----
divvy_db <- tibble(
  day = seq(ymd(first_day), ymd(last_day), by='day'),
  url_input = str_c(
    "https://data.cityofchicago.org/resource/eq45-8inv.csv?$where=timestamp between ",
    "\'",
    day,
    "T00:00:00\' and \'",
    day,
    "T23:59:59\'"
  )
) 

# collect daily data -----
divvy_db <- divvy_db %>% 
  mutate(
    data = map(url_input, ~ get_data(url = .x))
  )

# remove necessary columns ----
divvy_db <- divvy_db %>% 
  mutate(dim_check = map_int(data, nrow)) %>% 
  filter(dim_check > 0) %>% 
  unnest(data) %>%
  select(-url_input, -day, -dim_check) 

# fixing the timestamps from UTC to CST -----
divvy_db <- divvy_db %>% 
  mutate(timestamp = format(timestamp, tz = "US/Central"))


# write out the data -----
write_csv(divvy_db, out_path)


