library(moveVis)
library(geosphere)
library(move)
library(data.table)

rm(list = ls())

data_dir <- "H:/georgoff.github.io/forest_malaria/laoPDR_GPS/"

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

data[, dist_around := 0]
data[, dist_back := 0]
data[, dist_forward := 0]

data <- data[2000:2100,]

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

data$time_posix <- as.POSIXct(data[1,time][1], origin = "2000-01-01")

for(i in 1:nrow(data)) {
  data$time_posix[i] <- as.POSIXct(data[i, time][1], origin = "2000-01-01")
}

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
