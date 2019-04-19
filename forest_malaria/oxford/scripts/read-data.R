rm(list = ls())

library(data.table)
library(ggplot2)

locs <- read.csv("/homes/georgoff/gis_osm_places_free_1.csv", colClasses = c("NULL", "NULL", "character", "numeric", "NULL", "numeric", "numeric"))

locs <- as.data.table(locs)

plot(locs[, X], locs[, Y])

p <- ggplot(data = locs, aes(x = X, y = Y, color = fclass)) +
  geom_point(data = locs[fclass == "village"])

p

locs2 <- list()
locs2$x <- locs$X
locs2$y <- locs$Y
locs2$z <- locs$population

new_r <- raster(locs2)
