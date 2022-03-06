# load packages from shared library
lib <- '/projects/e30686/R/4.1'

# load packages ----
library("RSocrata", lib.loc = lib)
library("tidyverse", lib.loc = lib)
library('lubridate', lib.loc = lib)
library('filesstrings', lib.loc = lib)

# Define pathways and parameters ----

#symbolic link/folder name
my_folder <- "mydata"


# year nothing prior to 2013
input_year <- 2013
first_day <- str_c(input_year, "-01-01")
last_day <- str_c(input_year + 7, "-12-31") #pulls the entire range of data

# write file to

out_path <- str_c(my_folder,"/raw/trip_data.csv")

# Define function for downloading data ----
get_data <- function(url){
  read.socrata(
    url,
    app_token = "qi3rGNlSc9OLPzH9DazRP7aKL",
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

