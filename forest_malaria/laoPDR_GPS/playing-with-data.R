# Initialization
rm(list = ls())

on_cluster <- T
local <- F

list.of.packages <- c("geosphere", "data.table", "animation", "ggplot2", "ggmap")

if(local) {
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  require(data.table)
  require(geosphere)
  require(animation)
  require(ggplot2)
  require(ggmap)
  
  data_dir <- "H:/georgoff.github.io/forest_malaria/laoPDR_GPS/"
}

if(on_cluster) {
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages(lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")[,"Package"])]
  if(length(new.packages)) install.packages(new.packages, lib = "/ihme/malaria_modeling/georgoff/Rlibs/")
  
  require(data.table, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  require(geosphere, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  require(animation, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  require(ggplot2, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  require(ggmap, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  
  data_dir <- "/homes/georgoff/georgoff.github.io/forest_malaria/laoPDR_GPS/"
}


# read in data
load(paste0(data_dir, "GPS_HRP_1st_Cycle_clean.RData"))
load(paste0(data_dir, "GPS_HRP_2nd_Cycle_clean.RData"))
data1 <- as.data.table(GPS_HRP_1st_Cycle_clean)
data2 <- as.data.table(GPS_HRP_2nd_Cycle_clean)
data <- rbind(data1, data2)

# select logger to track
names(data)[6] <- "gps_logger"
logger <- c(30)
data <- data[gps_logger %in% logger]

# sort by time
data <- data[order(time),]

# for(i in 2:nrow(data)) {
#   data[i, time_int := data[i, time] - data[i-1, time]]
#   # print(data[i, time] - data[i-1, time])
# }

# EXPLORE NON-LOGGED POINTS




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

# map <- get_map(location = c(lon = min(data$lon), lat = max(data$lat)))

map <- get_map(location = c(left = min(data$lon), bottom = min(data$lat),
                            right = max(data$lon), top = max(data$lat)),
               maptype = "hybrid")

data$gps_logger <- as.factor(data$gps_logger)

plot <- ggmap(map) +
  geom_point(data = data[1:100,], aes(x = lon, y = lat, colour = gps_logger))

plot

# data[, flagged := FALSE]
# data[dist_around < 0.01 & dist_back > 0.01, flagged := TRUE]

# DEFINE "ZONES"

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

plot_groups <- ggmap(map) +
  geom_point(data = data[1:100], aes(x = lon, y = lat, colour = group))

plot_groups

# alternative: pick a time duration. person must stay within distance threshold
# for that long



data_long_move <- data[dist_back > 1000,]

map_long_move <- get_map(location = c(left = min(data_long_move$lon), bottom = min(data_long_move$lat),
                            right = max(data_long_move$lon), top = max(data_long_move$lat)),
               maptype = "hybrid")

plot_long_move <- ggmap(map_long_move) +
  geom_point(data = data_long_move, aes(x = lon, y = lat, colour = gps_logger))

plot_long_move


saveHTML(expr = {
  # plot(data[, lat], data[, lon], type = "n")
  for(i in seq(1, nrow(data_long_move), by = 1)) {
    plot(data_long_move[1:i, lat], data_long_move[1:i, lon],
         xlim = c(min(data[, lat]), max(data[, lat])),
         ylim = c(min(data[, lon]), max(data[, lon])),
         type = "l")
    ani.pause(0.05)
  }
},
img.name = "test_animation")