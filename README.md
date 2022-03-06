# STATS 359 - Divvy Bike Data Science Project

## _Control Variables Team_

Olivier Gabison, Mimi Wang, Austin Kim, Akshya Dhinakaran, Sam Dailley

## Introduction

The goal of our project is to compile, clean, and analyze divvy bike data in Chicago, USA. Our responsibility was to determine which control variables we need to include in the datasheets that we are analzying and then sending it out for other groups to use. We have written R scripts to run on the divvy bike historical datasets in order to create datasheets that can be used for further analysis later.

## Directions

You can follow along these steps to build off or use our scripts in the future:

1. `git clone git@github.com:austinkim118/STAT359_CV.git`
1. After cloning the Github repo, move all the datasets being used into the cloned folder
1. Modify the R scripts so it outputs datasheets to your desired location (and modify any file names as needed)
1. Run your script!

## Size of dataset issues

Because the dataset files were so large (went as high as 35 GB), we were unable to run our scripts locally on our machine. To get around this, we split our data into years and ran the R scripts on Quest.

## Important Things to Know

- Due to the extremely large sizes of our datasets, we ran all our R scripts on [Quest](https://www.it.northwestern.edu/research/user-services/quest/overview.html), Northwestern's high performance computing system.
- To download data from the City of Chicago (historical divvy bike data, community areas), you need to request an API token from the website by creating an account [here](https://data.cityofchicago.org/login).
- You can modify the R scripts to adjust the boundaries for the communuity areas. However, when doing so, keep in mind that you need all the shape files in downloaded in the same folder for the scripts to work.

## Citations

[Divvy Bike Official Website](https://divvybikes.com/)

### Data Sets Used

- [Chicago Community Areas Datasheet](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6)
  - This data set was used to map coordinate points to specific community areas in Chicago
- [Divvy Bike Stations Historical Data](https://data.cityofchicago.org/Transportation/Divvy-Bicycle-Stations-Historical/eq45-8inv/data)
- [Divvy Trip History Data](https://ride.divvybikes.com/system-data)
