library(raster, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
library(sf, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
library(data.table, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

rm(list = ls())

forest_filename <- "/homes/georgoff/cambodia_hansen_any_forest_0pc_native.tif"
locs_filename <- "/homes/georgoff/gis_osm_places_free_1.csv"

##############################
# Read in data
##############################

forest_raster <- raster(forest_filename)
forest_points <- rasterToPoints(forest_raster)
forest_points <- as.data.table(forest_points)
setnames(forest_points, "cambodia_hansen_any_forest_0pc_native", "forest")
locs <- read.csv(locs_filename, colClasses = c("NULL", "NULL", "character", "numeric", "NULL", "numeric", "numeric"))
locs <- as.data.table(locs)

##############################
# Set parameters
##############################

# search radius, in degrees:
radius <- 0.001

##############################
# Compute forest percentage
# within specified radius
##############################

# isolate village locations:
village_locs <- locs[fclass == "village"]
village_locs$forest_prop <- 0

# loop through each set of village coordinates:
for(i in 1:length(village_locs$X)) {
  # extract village coordinates:
  x_co <- village_locs$X[i]
  y_co <- village_locs$Y[i]
  
  # subset forest data that is within square of size radius:
  forest_subset <- forest_points[x > x_co - radius &
                                   x < x_co + radius &
                                   y > y_co - radius &
                                   y < y_co + radius]
  
  village_locs$forest_prop[i] <- mean(forest_subset$forest)
  
  cat("i =", i, "of ", length(village_locs$X), " \r", file = "", sep = " ")
  flush.console()
}

# p <- plot_ly(x = village_locs$X,
#              y = village_locs$Y,
#              z = village_locs$forest_prop,
#              type = "heatmap",
#              height = 800, width = 960) %>%
#   layout(title = "Equilibrium Prevalence in Village as a Function of R_0 in Village and Forest",
#          titlefont = list(size = 16),
#          xaxis = list(title = "R_0 Value, Village",
#                       titlefont = list(size = 20)),
#          yaxis = list(title = "R_0 Value, Forest",
#                       titlefont = list(size = 20)))
# 
# p

p2 <- ggplot(data = village_locs, aes(x = X, y = Y)) +
  geom_point(aes(size = forest_prop))

p2