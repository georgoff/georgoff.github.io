########################################################################
# forest-mapping.R
#
# Author: Alec Georgoff
#
# Purpose: Create custom maps of forest coverage and clustering using data
#          obtained from Oxford
########################################################################

rm(list = ls())

########################################################################
#
# README
#
########################################################################

# Follow these directions to start making forest maps:

# 1) Download the necessary data from this link:
#    georgoff.github.io/forest_malaria/forest_function_files.zip

# 2) Set the data_directory variable to point to your downloaded files:
data_directory <- "/homes/georgoff/forest_data/forest_function_files/"

# 3) Source the forest utilities file:
source(paste0(data_directory, "forest-utilities.R"))

# 4) Install the necessary packages. Indicate whether you are running R
#    on IHME's cluster (with access to cluster drives) or if you are
#    running locally/elsewhere:
on_the_cluster <- TRUE
load_required_packages(ihme_cluster = on_the_cluster)

########################################################################
#
# Setup
#
########################################################################

# Set your forest coverage threshold, as a percentage:
forest_coverage_threshold <- 80

# Pick a year of satellite data to use. Default is 2013, which is the latest
# available year. 2001 is the earliest:
year_to_use <- 2013

# Indicate if you'd like to crop the map to a certain area. Note that data
# availability is limited to the following crop limits :[102,108,9,15]
# therefore, cropping is recommended: 
crop <- TRUE

# If cropping, set the crop limits in terms of latitude and longitude. The
# format is as follows: [left, right, bottom, top]
crop_limits <- c(102,108,9,15)

# Indicate if you'd like to include the country borders in your map:
country_borders <- TRUE

# Indicate which country borders to include. The options are Cambodia, Lao,
# Vietnam, Thailand, and Myanmar
countries_to_use <- c("Cambodia", "Lao", "Vietnam", "Thailand", "Myanmar")

########################################################################
#
# Execute
#
########################################################################

forest_map(data_directory = data_directory,
           forest_coverage_threshold = forest_coverage_threshold,
           country_borders = country_borders,
           countries_to_use = countries_to_use,
           crop = crop,
           crop_limits = crop_limits)
