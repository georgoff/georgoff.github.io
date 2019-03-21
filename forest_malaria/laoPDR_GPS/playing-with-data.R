# Initialization
rm(list = ls())

require(data.table, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

data_dir <- "/homes/georgoff/georgoff.github.io/forest_malaria/laoPDR_GPS/"

# read in data
load(paste0(data_dir, "GPS_HRP_1st_Cycle_clean.RData"))
load(paste0(data_dir, "GPS_HRP_2nd_Cycle_clean.RData"))
data1 <- as.data.table(GPS_HRP_1st_Cycle_clean)
data2 <- as.data.table(GPS_HRP_2nd_Cycle_clean)
data <- rbind(data1, data2)

# select logger to track
logger <- 7
data <- data[`GPS Logger` == logger]

# sort by time
data <- data[order(time),]

# plot(data[, lat], data[, lon], type = 'n')

# for(i in 1:nrow(data)) {
#   points(data[i, lat], data[i, lon])
#   
#   Sys.sleep(0.01)
#   Sys.sleep(0)
# }

# calculate distance traveled since last point
data[, distance := 0]

for(i in 2:nrow(data)) {
  data[i, distance := sqrt((data[i, lat] - data[i-1, lat])^2 + (data[i, lon] - data[i-1, lon])^2)]
}

# calculate time interval since last point
data[, time_int := data[1, time] - data[1, time]]

for(i in 2:nrow(data)) {
  data[i, time_int := data[i, time] - data[i-1, time]]
  # print(data[i, time] - data[i-1, time])
}

# EXPLORE NON-LOGGED POINTS

# filter out "bogus" GPS readings
## loop thru and calculate:
## 1) distance from previous coordinate
## 2) distance between previous coordinate and next coordinate




# DEFINE "ZONES"

## assume that, if a certain number of readings are within a certain distance of
## each other, the person is in a "zone"

# the distance that points must be within to be considered the same "zone"
distance_thres <- 0.001

# how many consecutive data points must be within the distance threshold
duration_thres <- 4



# alternative: pick a time duration. person must stay within distance threshold
# for that long