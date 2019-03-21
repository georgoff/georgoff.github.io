# This code plots HRP GPS loggers collected during AcME

# Preliminaries in R
# Clear the R environment of variables  
rm(list=ls())

#set the working directory
setwd("/homes/georgoff/georgoff.github.io/forest_malaria/laoPDR_GPS/")

#load necessary R packages. If needed, install
# library(rgdal)
# library(raster)
# library(readr)
# library(plotKML)
# library(leaflet)
# library(plotGoogleMaps)
# library(tidyverse)
# library(scales)
# library(ggpubr)
# 
# library(chron)

list.of.packages <- c("rgdal", "raster", "readr", "plotKML",
                      "leaflet", "plotGoogleMaps", "tidyverse",
                      "scales", "ggpubr", "chron")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, lib = "/ihme/malaria_modeling/georgoff/Rlibs/")

#### Load the data
#### Load the data
# Champasak Administrative Boundary
Lao_Adm_1 <- getData("GADM", country = "LAO", level = 1)

load("GPS_HRP_1st_Cycle_clean.RData")
load("GPS_HRP_2nd_Cycle_clean.RData")

## Combine together
GPS_HRP <- rbind(GPS_HRP_1st_Cycle_clean,
                 GPS_HRP_2nd_Cycle_clean)

## Creat unique Logger/Cycle ID
GPS_HRP$ID <- paste0("Cycle ", GPS_HRP$Cycle, "-Logger ", GPS_HRP$`GPS Logger`)

#Sort by ID and time 
GPS_HRP <- (GPS_HRP
               %>% group_by(ID)
               %>% arrange(ID, time)
              )


# ##### Matching keys
# PN.team.GPS.logger <- c(43, 41, 44, 42,
#                         1, 2, 5, 3, 6, 4,
#                         32, 31,
#                         57, 58, 59)
# PN.number.team <- seq(1, 29, by=2)
# key.PN.team <- cbind(PN.team.GPS.logger, PN.number.team)
# PN.District <- c(rep("MP",8), rep("PT",12), rep("SB",4), rep("SK",6))
# PN.HCCA <- c("Nady", "Nady","Nady", "Nady", "Veunyang", "Veunyang", "Veunyang", "Veunyang",
#              "Kaelae", "Kaelae", "Phapho", "Phapho", "Phapho", "Phapho", "Phapho", "Phapho", "Pathoumphone", "Pathoumphone", "Pathoumphone", "Pathoumphone",
#              "Nakham", "Nakham", "Nakham", "Nakham",
#              "Hieng", "Hieng", "Hieng", "Hieng", "Hieng", "Hieng")
# 
# HRP.GPS.Logger <- c(7:30,33:40,45:56,60:63,65:67,69:70,71:85)
# HRP.GPS.Logger.Order <- c(1:length(HRP.GPS.Logger))

length(unique(GPS_HRP$ID))

## Non-reproducible: created Col palette with IWantHue
GPS.logger.colPalette <- c("#9bc233",
                            "#7951d0",
                            "#65ce4f",
                            "#b866e6",
                            "#3ea528",
                            "#993db1",
                            "#b9c024",
                            "#436bea",
                            "#d0b737",
                            "#4553c2",
                            "#699f27",
                            "#eb50bd",
                            "#45c66a",
                            "#bf349d",
                            "#39d29f",
                            "#de2c83",
                            "#3c9744",
                            "#e36ddb",
                            "#89c15d",
                            "#774eb8",
                            "#b9bf50",
                            "#957ced",
                            "#9d9f30",
                            "#6257b8",
                            "#eaa83b",
                            "#3c66c4",
                            "#b38b19",
                            "#618bed",
                            "#e2682a",
                            "#34c7dd",
                            "#df482e",
                            "#49cbca",
                            "#e32851",
                            "#309c6a",
                            "#e85eab",
                            "#3f751e",
                            "#d880e3",
                            "#6e892c",
                            "#884b9f",
                            "#7fc281",
                            "#a24192",
                            "#50b88f",
                            "#e5467a",
                            "#64cab2",
                            "#b9311e",
                            "#55a2e5",
                            "#db862f",
                            "#2a78bb",
                            "#f16755",
                            "#58b5e1",
                            "#a34719",
                            "#4463a6",
                            "#bf9848",
                            "#5e4393",
                            "#516615",
                            "#ad81db",
                            "#34763c",
                            "#ae3377",
                            "#61a27a",
                            "#b42d3b",
                            "#2c9d98",
                            "#de5059",
                            "#1a6447",
                            "#e771a1",
                            "#296437",
                            "#cb73bd",
                            "#415a1f",
                            "#785bae",
                            "#897c29",
                            "#494b93",
                            "#aab56e",
                            "#6462ad",
                            "#616117",
                            "#dc9ce0",
                            "#536c31",
                            "#8590d8",
                            "#b26a2e",
                            "#4585b3",
                            "#df7a5a",
                            "#2f7b63",
                            "#b2355e",
                            "#698e4e",
                            "#7d5496",
                            "#d4af74",
                            "#505b8f",
                            "#89611c",
                            "#b3a9ea",
                            "#6f5d1d",
                            "#ad80be",
                            "#5e622c",
                            "#98487e",
                            "#89834d",
                            "#766ea8",
                            "#eb9e72",
                            "#8c588c",
                            "#a6764b",
                            "#864b6f",
                            "#86522a",
                            "#e694b8",
                            "#a44d37",
                            "#ba6b90",
                            "#b35956",
                            "#92465f",
                            "#db8b88",
                            "#984950",
                            "#e57580")

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) 

# Create SP file
GPS_HRP_DF <- GPS_HRP
coordinates(GPS_HRP) <- ~ lon + lat
proj4string(GPS_HRP) <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')

for (i in 1: length(unique(GPS_HRP$ID))){
  data.frame.HRP.GPS.logger <- GPS_HRP[which(GPS_HRP$ID == sort(unique(GPS_HRP$ID))[i]),]
  ## Convert to spatial lines for track visualization
  x <- lapply(split(data.frame.HRP.GPS.logger, data.frame.HRP.GPS.logger$ID), function(x) Lines(list(Line(coordinates(x))), x$ID[1L]))
  lines <- SpatialLines(x, proj4string = CRS("+init=epsg:4326"))
  lines_ll <- sp::spTransform(lines, CRS("+init=epsg:4326")) # leaflet needs that for polylines
  data <- data.frame(ID = unique(data.frame.HRP.GPS.logger$ID))
  rownames(data) <- data$ID
  HRP.GPS.logger.tracks <- SpatialLinesDataFrame(lines_ll, data)
  
  #Add to map plot
  map <- map %>% addPolylines(data = HRP.GPS.logger.tracks,
                              color = GPS.logger.colPalette[i],
                              opacity = 1,
                              weight = 1)
  }

map