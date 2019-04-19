################################################
# Author: Alec Georgoff
#
# Date: 4/17/19
#
# Purpose: Attempt to animate GPS data
################################################

# Note: This script uses the "moveVis" package. More information can be
# found at http://movevis.org/

# Initialization
rm(list = ls())

# IMPORTANT: replace this with the directory that contains the GPS data:
data_dir <- "H:/georgoff.github.io/forest_malaria/laoPDR_GPS/"

list.of.packages <- c("geosphere", "data.table", "moveVis", "ggplot2", "ggmap")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(data.table)
library(geosphere)
library(moveVis)
library(ggplot2)
library(ggmap)

# read in data
load(paste0(data_dir, "GPS_HRP_1st_Cycle_clean.RData"))
load(paste0(data_dir, "GPS_HRP_2nd_Cycle_clean.RData"))
data1 <- as.data.table(GPS_HRP_1st_Cycle_clean)
data2 <- as.data.table(GPS_HRP_2nd_Cycle_clean)
data <- rbind(data1, data2)

# select logger to track
logger <- 7
data <- data[`GPS Logger` == logger]
names(data)[6] <- "gps_logger"

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

# Note: I had a hard time converting the time markers in the data to the POSIXct format...
# this part of the code actually stopped working because of that so it needs more work

move_data <- df2move(data,
                     proj = "+proj=longlat",
                     x = "lon",
                     y = "lat",
                     time = "time_posix",
                     track_id = "gps_logger")

move_data_aligned <- align_move(move_data, res = 240, digit = 0, unit = "secs")

frames <- frames_spatial(move_data_aligned,
                         map_service = "osm",
                         map_type = "watercolor",
                         alpha = 0.5)

animate_frames(frames, out_file = "H:/georgoff.github.io/forest_malaria/laoPDR_GPS/example_1.gif")
