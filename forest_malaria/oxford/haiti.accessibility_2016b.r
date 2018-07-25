# The logic behind the facility weights is that, for a single cell's population, if given several choices of facility to attend (assume equidistant), the people will divide proportionally in response to the weight
# For example, if 3 facilites are in range (weighted 500, 300, and 200 respictively) and the cell had 100 people after reduction for TS, those people would split 0.5, 0.3., 0.2 (aka 50, 30, and 20)
# The key asusmptions here are that (a) within the (relatively small) facility footpints PR is unlikley to vary widely and (b) any cells that fall within the fooprints of multipl facilites could readily get to either
# However, that simple (linear) weighting function is modified by the distance function so that if the facilities within reach are not equidistant the closer one will receive more people
# In cases where only ony facility is within range, ALL people (after TS) are assigned to that facility regardless of distance

t.start <- Sys.time()

## Required Packages
require(gdistance)

## Variables
first.point <- 1
last.point <- 610 #775
left <- -74.575
right <- -71.5
bottom <- 17.8416666667
top <- 20.2166666667
GAUL.value <- 108
transition.matrix.exists.flag <- 1

## Input Files
friction.surface.filename <- 'E:\\accessibility\\GEE_output\\friction_surface_2017-01-05_final.tif'
population.filename <- 'E:\\data\\world\\population_density\\worldpop_gpwv4_mosaic_export_1k_MG_Reallocated.tif'
admin.filename <- 'F:\\00_Admin_June_2015\\global_raster_witout_disputed_areas_nibbled\\admin_0_no_disputes_1km_nibbled.tif'
ts.filename <- 'E:\\CHAI_Haiti\\treatment_seeking_GWR_model.tif'

## Output Files
T.filename <- 'E:\\CHAI_Haiti\\haiti.T.rds'
T.GC.filename <- 'E:\\CHAI_Haiti\\haiti.T.GC.rds'
output.path <- "E:\\CHAI_Haiti\\"
output.prefix <- "facility.access.haiti."
output.extension <- ".tif"
output.filename <- paste(output.path, output.prefix, as.character(first.point), "-", as.character(last.point), output.extension, sep="")
csv.filename <- "E:\\CHAI_Haiti\\facilities_processed.csv"

## Read in the points table, sturctured as [SORT, UNIQUEID, X_COORD, Y_COORD, MONTH, Weight1, Weight2]
point.filename <- 'E:\\CHAI_Haiti\\HTI_2014_15_16_dan_weight_with_header.csv'
#point.filename <- 'E:\\CHAI_Haiti\\2014_HF_cases_dan_weight_with_header.csv'
#point.filename <- 'E:\\CHAI_Haiti\\2015_HF_cases_dan_weight_with_header.csv'
pos.id <- 1 # the column in the table containing the ID value for the facility (simple is better - this will be used to name MANY output rasters)
pos.weight <- 7 # (7 is the RDT weight, 6 is the total weight) the column in the table containing the weight value for the facility
pos.xcoord <- 3 # the column in the table containing the longitude for the facility
pos.ycoord <- 4 # the column in the table containing the latitude for the facility

## Define the paths for the per-facility rasters
masked.cost.distances.path <- 'E:\\CHAI_Haiti\\facility_rasters\\masked_cost_distances\\'
masked.inverse.cost.distances.path <- 'E:\\CHAI_Haiti\\facility_rasters\\masked_inverse_cost_distances\\'
masked.weights.path <- "E:\\CHAI_Haiti\\facility_rasters\\masked_weights\\"
weight.sum.filename <- "E:\\CHAI_Haiti\\facility_rasters\\weight.sum.tif"
cost.sum.filename <- "E:\\CHAI_Haiti\\facility_rasters\\cost.sum.tif"
inverse.cost.sum.filename <- "E:\\CHAI_Haiti\\facility_rasters\\inverse.cost.sum.tif"

#  Define the spatial information from the friction surface
friction <- raster(friction.surface.filename)
fs1 <- crop(friction, extent(left, right, bottom, top))

if (transition.matrix.exists.flag == 1) {
  ## Read in the transition matrix object if it has been pre-computer
  T.GC <- readRDS(T.GC.filename)
} else {
  ## Make and geocorrect the transition matrix (i.e., the graph)
  T <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
  saveRDS(T, T.filename)
  T.GC <- geoCorrection(T)                    
  saveRDS(T.GC, T.GC.filename)
}

## Read in, align, and subset the population data, including restricting it to Swaziland
ex <- extent(fs1)
population <- raster(population.filename)
population.crop <- crop(population, extent(floor(left), ceiling(right), floor(bottom), ceiling(top))) # make a slightly oversized population subset for resampling
population.rs <- resample(population.crop, fs1, "ngb")
pop <- crop(population.rs, ex)
admin <- raster(admin.filename)
admin.crop <- crop(admin, extent(floor(left), ceiling(right), floor(bottom), ceiling(top))) # make a slightly oversized population subset for resampling
admin.rs <- resample(admin.crop, fs1, "ngb")
admin.raster <- crop(admin.rs, ex)
admin.raster[admin.raster != GAUL.value] <- 0
admin.raster[admin.raster == GAUL.value] <- 1
ts.raster <- raster(ts.filename)
ts.crop <- crop(ts.raster, extent(floor(left), ceiling(right), floor(bottom), ceiling(top))) # make a slightly oversized population subset for resampling
ts.rs <- resample(ts.crop, fs1, "ngb")
pop <- pop * admin.raster * ts.rs

#population.filename.out <- 'E:\\CHAI_Haiti\\pop.tif'
#writeRaster(pop, population.filename.out)

## Read in the facility points
points <- read.csv(file = point.filename)

# 2015 (data) MODEL for footrpints
# Get the vector of facility IDs and create the weights
facility.id.vec <- points[,pos.id] # 
weight.vec <- points[,pos.weight]
weight.vec <- weight.vec + 1
#footprint.vec <- weight.vec * 0.593 + 32.953 ## these coefficients are from the Swaziland data (coefficients of linear model between n_patients and footprint size) 

#  2016 (data) MODEL 1 footprints
# Mean number of minutes (Swaziland) = 20
# 95% number of minutes 65
#adaptive.range = 65.0 - 20.0
#footprint.vec <- 20.0 + ((weight.vec / sum(weight.vec)) * adaptive.range)

#  2016 (data) MODEL 2 footprints - based on a badly-fitting linear model of the Swazi data... nothing could really fit it well
# The biggest change is that in the ealier model most facility ranges were about 20 minutes while this one they will be at least 33 minutes... this is really similar to the 2015 model actually
# Mean number of minutes (Swaziland) = 20
# 95% number of minutes 65
footprint.max <- 98.2           # in units of travel-time, based on the 90th percentile of the Swazi data (excluding zeros, 95th percentile when including them)
footprint.slope <- 0.6315       # linear model when excluding facilities with zero cases (and thus zero distance)
footprint.intercept <- 33.406
footprint.vec <-  footprint.slope * weight.vec + footprint.intercept
footprint.vec[footprint.vec > footprint.max] <- footprint.max

# For looping through all facilities
temp <- dim(points)
n.facilities <- temp[1]
n.facility.cols <- temp[2]

# Iterate through the all facilities to calculate the overall accessibility surface and the per-facility surfaces
for (temp.point in 1:n.facilities){
  
    # temp.point <- 1
  
  ## First do the steps for the total accessibility surface
  temp.point.coords <- c(points[temp.point,pos.xcoord],points[temp.point,pos.ycoord])
  temp.raster <- accCost(T.GC, temp.point.coords)
  if (temp.point == 1) {
    output.raster <- temp.raster # Use the first point to establish the eventual output raster
  } else {
    output.raster <- min(output.raster, temp.raster)
  }
  
  ## Now do the steps for the per-facility rasters
  weight.value <- weight.vec[temp.point]
  footprint.value <- footprint.vec[temp.point]
  mask.raster <- temp.raster
  temp.raster[temp.raster == 0] <- 0.001 # the value of the origin cell, leaving this as zero could cause a divide by zero problem later
  mask.raster[temp.raster >= footprint.value] <- 0
  mask.raster[temp.raster < footprint.value] <- 1
  
  
  ## Apply the distance decay function based on the Swaziland data
  ## =2.71828183^(Q$2+Q$3*J2)
  ## = 2.718282^(-2.769253 + -0.065363 * Trave_Time_In_Minutes)
  ## Because this is a pixel-level calculation I have to make rasters for each of the coefficients
  e.value <- 2.718282
  a.value <- -2.769253
  b.value <- -0.065363
  cost.raster <- e.value^(a.value + b.value * temp.raster) * mask.raster
  
  weight.raster <- weight.value * mask.raster
  if (temp.point == 1) { 
    cost.raster.sum <- cost.raster
    weight.raster.sum <- weight.raster
  } else {
    cost.raster.sum <- cost.raster.sum + cost.raster
    weight.raster.sum <- weight.raster.sum + weight.raster  
  }
  
  # Get the facility ID and use it to define the output filenames
  facility.id = facility.id.vec[temp.point]
  cost.filename <- paste(masked.cost.distances.path, facility.id, output.extension, sep="")
  weight.filename <- paste(masked.weights.path, facility.id, output.extension, sep="")
  
  ## Write out the per-facility rasters
  writeRaster(cost.raster, cost.filename)
  writeRaster(weight.raster, weight.filename)
  
}
gc()

## Write out the cost path raster showing travel time to the closest (by time) facility for all cells
writeRaster(output.raster, output.filename)

## Write out the accumulated rasters
writeRaster(cost.raster.sum, cost.sum.filename)
writeRaster(weight.raster.sum, weight.sum.filename)

# Iterate through the facilities, reading in the rasters associated with each, and calculating the inverse distance weight applied to travel time
for (temp.point2 in 1:n.facilities){
  facility.id = facility.id.vec[temp.point2]
  cost.filename <- paste(masked.cost.distances.path, facility.id, output.extension, sep="")
  cost.raster <- raster(cost.filename)
  mask.raster[cost.raster == 0] <- 0
  mask.raster[cost.raster != 0] <- 1 # make a mask to apply later
  cost.raster[cost.raster == 0] <- 1 # alter the cost.dist raster to avoid division by zero and thus changing some cells to unusable
  inverse.cost.raster <- 1/(cost.raster / cost.raster.sum)
  inverse.cost.raster = inverse.cost.raster * mask.raster # now reapply the mask to put zeros in the out of bounds cells
  if (temp.point2 == 1) { 
    inverse.cost.raster.sum <- inverse.cost.raster
  } else {
    inverse.cost.raster.sum <- inverse.cost.raster.sum + inverse.cost.raster 
  }
  inverse.cost.filename <- paste(masked.inverse.cost.distances.path, facility.id, output.extension, sep="")
  writeRaster(inverse.cost.raster, inverse.cost.filename)
}
writeRaster(inverse.cost.raster.sum, inverse.cost.sum.filename)

# Iterate through the facilities, reading in the rasters associated with each, and calculating the catchment population associated with it
for (temp.point3 in 1:n.facilities){
  # temp.point3 <- 701
  facility.id = facility.id.vec[temp.point3]
  inverse.cost.filename <- paste(masked.inverse.cost.distances.path, facility.id, output.extension, sep="")
  weight.filename <- paste(masked.weights.path, facility.id, output.extension, sep="")
  
  inverse.cost.raster <- raster(inverse.cost.filename)
  weight.raster <- raster(weight.filename)

  mask.raster1 <- inverse.cost.raster
  mask.raster1[inverse.cost.raster == 0] <- 0
  mask.raster1[inverse.cost.raster != 0] <- 1 # make a mask to apply later
  inverse.cost.raster[inverse.cost.raster == 0] <- 1 # alter the cost.dist raster to avoid division by zero and thus changing some cells to unusable
  
  temp.pop1 <- (inverse.cost.raster / inverse.cost.raster.sum) * mask.raster1 * pop # the population split for this facility, for cells in range, based on IDW alone
  
  mask.raster2 <- weight.raster
  mask.raster2[weight.raster == 0] <- 0
  mask.raster2[weight.raster != 0] <- 1 # make a mask to apply later
  weight.raster[weight.raster == 0] <- 1 # alter the weight raster to avoid division by zero and thus changing some cells to unusable
  
  temp.pop2 <- (weight.raster / weight.raster.sum) * mask.raster2 * pop # the population split for this facility, for cells in range, based on facility weight alone

  combined.pop <- (temp.pop1 + temp.pop2) / 2
  #plot(combined.pop)
  
  catchment.pop <- cellStats(combined.pop, stat = sum)
  points[temp.point3,(n.facility.cols + 1)] <- catchment.pop
}
gc()

## WRITE .CSVs
write.csv(points, csv.filename, row.names=F)

t.end <- Sys.time()
t.run <- t.end - t.start

#print(c("Run time in seconds : ", t.run), quote=F)
print(t.run)


