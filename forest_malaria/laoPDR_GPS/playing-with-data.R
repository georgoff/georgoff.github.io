################################################
# Author: Alec Georgoff
#
# Date: 4/17/19
#
# Purpose: Begin cleaning and exploring GPS
#          logger data from Lao PDR study
################################################

# Initialization
rm(list = ls())

# IMPORTANT: replace this with the directory that contains the GPS data:
data_dir <- "H:/georgoff.github.io/forest_malaria/laoPDR_GPS/"

list.of.packages <- c("geosphere", "data.table", "animation", "ggplot2", "ggmap")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(geosphere)
library(animation)
library(ggplot2)
library(ggmap)

# read in data
load(paste0(data_dir, "GPS_HRP_1st_Cycle_clean.RData"))
load(paste0(data_dir, "GPS_HRP_2nd_Cycle_clean.RData"))
data1 <- as.data.table(GPS_HRP_1st_Cycle_clean)
data2 <- as.data.table(GPS_HRP_2nd_Cycle_clean)
data <- rbind(data1, data2)

# select logger(s) to track
logger <- c(30)

names(data)[6] <- "gps_logger"
data <- data[gps_logger %in% logger]

# sort by time
data <- data[order(time),]

# EXPLORE NON-LOGGED POINTS

# didn't get to this


# FILTER OUT "BOGUS" GPS READINGS
## loop thru and calculate:
## 1) distance from previous coordinate
## 2) distance between previous coordinate and next coordinate

data[, dist_around := 0]
data[, dist_back := 0]
data[, dist_forward := 0]


for(i in 1:nrow(data)) {
  if(i > 1 & i < nrow(data)) {
    data[i, dist_around := distVincentyEllipsoid(p1 = c(data[i-1, lon], data[i-1, lat]),
                                                 p2 = c(data[i+1, lon], data[i+1, lat]))]
  }

  if(i < nrow(data)) {
    data[i, dist_forward := distVincentyEllipsoid(p1 = c(data[i, lon], data[i, lat]),
                                                  p2 = c(data[i+1, lon], data[i+1, lat]))]
  }

  if(i > 1) {
    data[i, dist_back := data[i-1, dist_forward]]
  }
}

# PLOT GPS DATA POINTS ON MAP
# note: get_map() requires an API key from Google. See ?register_google() for details

map <- get_map(location = c(left = min(data$lon), bottom = min(data$lat),
                            right = max(data$lon), top = max(data$lat)),
               maptype = "hybrid")

data$gps_logger <- as.factor(data$gps_logger)

plot <- ggmap(map) +
  geom_point(data = data[1:100,], aes(x = lon, y = lat, colour = gps_logger))

plot

# DEFINE "ZONES"

# NOTE: I didn't actually get this to work, but it could serve as a starting point

## assume that, if a certain number of readings are within a certain distance of
## each other, the person is in a "zone"

# the distance that points must be within to be considered the same "zone"
# distance in meters:
distance_thres <- 500

# how many consecutive data points must be within the distance threshold
duration_thres <- 4

group <- 1
data$group <- NA

for(i in (1+duration_thres):nrow(data)) {
  # assume this point is part of the current group:
  part_of_group <- T
  
  for(j in 1:duration_thres) {
    # check every point behind it to see if they're all within
    # the distance threshold:
    distance_to_target <- sum(data$dist_back[i-j:i])
    if(distance_to_target > distance_thres) {
      part_of_group <- F
    }
  }
  
  if(!part_of_group) {
    group <- group + 1
  }
  
  data$group[i] <- group
}

data$group <- as.factor(data$group)

map_groups <- get_map(location = c(left = min(data$lon), bottom = min(data$lat),
                            right = max(data$lon), top = max(data$lat)),
               maptype = "hybrid")

plot_groups <- ggmap(map_groups) +
  geom_point(data = data, aes(x = lon, y = lat, colour = group))

plot_groups

# alternative: pick a time duration. person must stay within distance threshold
# for that long (I didn't implement this, just an idea)


# I tried to subset the data to just movements that were over a certain distance threshold,
# to figure out larger-scale movement patterns. It doesn't work correctly yet
data_long_move <- data[dist_back > 1000,]

map_long_move <- get_map(location = c(left = min(data_long_move$lon), bottom = min(data_long_move$lat),
                            right = max(data_long_move$lon), top = max(data_long_move$lat)),
               maptype = "hybrid")

plot_long_move <- ggmap(map_long_move) +
  geom_point(data = data_long_move, aes(x = lon, y = lat, colour = gps_logger))

plot_long_move