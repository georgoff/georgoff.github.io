########################################################################
#
# Install necessary packages
#
########################################################################

load_required_packages <- function(ihme_cluster) {
  
  if (ihme_cluster) {
    # if on the cluster, load libraries from my J temp repository
    cluster_lib_loc <- "/snfs1/temp/georgoff/Rlibs/"
    
    library(raster, lib.loc = cluster_lib_loc)
    library(ggplot2, lib.loc = cluster_lib_loc)
    library(data.table, lib.loc = cluster_lib_loc)
    library(rgdal, lib.loc = cluster_lib_loc)
  }
  
  if (!ihme_cluster) {
    # if not on the cluster, check if packages are installed and install
    # those that aren't
    list.of.packages <- c("raster", "ggplot2", "data.table", "rgdal")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    
    library(raster)
    library(ggplot2)
    library(data.table)
    library(rgdal)
  }
  
}

########################################################################
#
# Read in locations
#
########################################################################

read_locs <- function(locs_filename, place_types = "village") {
  locs <- as.data.table(read.csv(locs_filename, colClasses = c("NULL", "NULL", "character", "numeric", "NULL", "numeric", "numeric")))
  locs <- locs[fclass %in% place_types]
  
  return(locs)
}

########################################################################
#
# Print out plots of all forest cover rasters
#
########################################################################

plot_all_rasters <- function(data_directory, print_to_pdf = FALSE, pdf_filepath = NULL) {
  
  raster_directory <- paste0(data_directory, "ForestRasterData/")
  
  files <- list.files(raster_directory)
  
  num_files <- length(files)  
  
  i <- 1
  
  if (print_to_pdf) {
    pdf(pdf_filepath)
  }
  
  for(file in files) {
    temp_raster <- raster(paste0(raster_directory, file))
    plot(temp_raster)
    title(file, cex.main = 0.65)
    
    # print progress:
    cat(i, " of ", num_files, "\n")
    
    i <- i+1
  }
  
  if (print_to_pdf) {
    dev.off()
  }

}

########################################################################
#
# Add up proportions of all forest cover types in every cell
#
########################################################################

sum_forest_types <- function(data_directory, year_to_use = 2013, plot_results = FALSE,
                             crop_rasters = FALSE, crop_limits = NULL,
                             save_all_rasters_as_RDS = FALSE, all_rasters_RDS_filepath = NULL,
                             save_all_rasters_as_PDF = FALSE, all_rasters_PDF_filepath = NULL) {
  
  # crop_limits = [left, right, bottom, top]
  
  raster_directory <- paste0(data_directory, "ForestRasterData/")
  
  files <- list.files(raster_directory)
  
  num_files <- length(files)
  
  # subset files to selected year:
  forest_percent <- files[grepl(as.character(year_to_use), files) & grepl("percentage", files)]
  
  # subset to each type of forest cover:
  EN_raster_file <- forest_percent[grepl("Evergreen_Needleleaf", forest_percent)]
  EB_raster_file <- forest_percent[grepl("Evergreen_Broadleaf", forest_percent)]
  DN_raster_file <- forest_percent[grepl("Deciduous_Needleleaf", forest_percent)]
  DB_raster_file <- forest_percent[grepl("Deciduous_Broadleaf", forest_percent)]
  MIX_raster_file <- forest_percent[grepl("Mixed_Forest", forest_percent)]
  
  # import rasters:
  EN_raster <- raster(paste0(raster_directory, EN_raster_file))
  EB_raster <- raster(paste0(raster_directory, EB_raster_file))
  DN_raster <- raster(paste0(raster_directory, DN_raster_file))
  DB_raster <- raster(paste0(raster_directory, DB_raster_file))
  MIX_raster <- raster(paste0(raster_directory, MIX_raster_file))
  
  # add all the rasters together:
  all_rasters <- overlay(EN_raster, EB_raster, DN_raster, DB_raster, MIX_raster, fun = sum)
  
  if (crop_rasters) {
    all_rasters <- crop(all_rasters, extent(crop_limits))
  }
  
  if (save_all_rasters_as_RDS) {
    saveRDS(all_rasters, all_rasters_RDS_filepath)
  }
  
  if (save_all_rasters_as_PDF) {
    pdf(all_rasters_PDF_filepath)
  }
  
  if (plot_results) {
    plot(all_rasters)
    title("Proportion of Forest Coverage")
  }
  
  if (save_all_rasters_as_PDF) {
    dev.off()
  }
  
  return(all_rasters)
}

########################################################################
#
# Create raster of entire area subset to forest coverage threshold
#
########################################################################

subset_to_forest_coverage_level <- function(data_directory, forest_coverage_threshold,
                                            plot_results = FALSE,
                                            year_to_use = 2013,
                                            save_as_PDF = FALSE, PDF_filename = NULL, ...) {
  
  raster_directory <- paste0(data_directory, "ForestRasterData/")
  
  forest_prop_raster <- sum_forest_types(raster_directory = raster_directory, year_to_use = year_to_use, plot_results = FALSE, ...)
  
  forest_prop_points <- rasterToPoints(forest_prop_raster)
  forest_prop_points <- as.data.table(forest_prop_points)
  forest_prop_points <- forest_prop_points[layer > forest_coverage_threshold]
  
  if (save_as_PDF) {
    pdf(PDF_filename)
  }
  
  if(plot_results) {
    p <- ggplot(data = forest_prop_points, aes(x = x, y = y)) +
      geom_raster(aes(fill = layer)) +
      scale_fill_gradientn(colours=c("white", "green")) +
      ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%"))
    
    plot(p)
  }
  
  if (save_as_PDF) {
    dev.off()
  }
  
  return(forest_prop_points)
}

########################################################################
#
# Develop algorithm for clustering/defining forests
#
########################################################################

# use this command for testing the function:

test <- forest_map(data_directory = data_directory, forest_coverage_threshold = 60, country_borders = TRUE,
           countries_to_use = c("Cambodia", "Lao", "Vietnam", "Thailand", "Myanmar"),
           crop = TRUE, crop_limits = crop_limits,
           include_locs = TRUE, locs_filename = paste0(data_directory, "gis_osm_places_free_1.csv"), place_types = c("village"))



forest_map <- function(data_directory, forest_coverage_threshold, year_to_use = 2013,
                       crop = FALSE, crop_limits = NULL,
                       country_borders = FALSE, countries_to_use = NULL,
                       include_locs = FALSE, locs_filename = NULL, place_types = NULL) {
  
  forest_grouping <- sum_forest_types(data_directory = data_directory, year_to_use = year_to_use,
                                      crop_rasters = crop, crop_limits = crop_limits)
  forest_grouping <- rasterToPoints(forest_grouping)
  forest_grouping <- as.data.table(forest_grouping)
  
  # create a new column for group number:
  forest_grouping$group <- NA
  
  search_group <- 0
  
  for (pixel in 1:nrow(forest_grouping)) {
    if (is.na(forest_grouping$group[pixel]) & forest_grouping$layer[pixel] > forest_coverage_threshold) {
      # new group found
      search_group <- search_group + 1
      forest_grouping$group[pixel] <- search_group
      forest_grouping <- mark_neighbors(start_pixel = pixel, search_group = search_group, forest_grouping = forest_grouping,
                                        forest_coverage_threshold = forest_coverage_threshold)
    }
  }
  
  p <- ggplot(data = forest_grouping[layer > forest_coverage_threshold], aes(x = x, y = y)) +
    # TODO: add option for displaying coverage percentage and picking color
    
    # plot raster of forested pixels:
    geom_raster(aes(fill = layer)) +
    scale_fill_gradientn(colours=c("#669933", "#336600")) +
    
    ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%")) +
    xlab("Longitude") +
    ylab("Latitude") +
    
    # display points for each forested pixel, colored by group:
    geom_point(data = forest_grouping[!is.na(forest_grouping$group)],
               aes(colour = factor(group)))

  
  if (country_borders) {
    shp_tables <- compile_shp_files(shp_directory = paste0(data_directory, "AdminShapefiles/"), countries_to_use = countries_to_use, crop = crop,
                                    crop_limits = crop_limits)
    
    for (i in 1:length(shp_tables)) {
      p <- p + geom_path(data = shp_tables[[i]], aes(x = long, y = lat, group = group), color = "#CCCCCC", na.rm = FALSE)
    }
  }

  if (include_locs) {
    p <- p + geom_point(data = read_locs(locs_filename = locs_filename, place_types = place_types)[X > crop_limits[1] & X < crop_limits[2] & Y > crop_limits[3] & Y < crop_limits[4]],
                        aes(x = X, y = Y), color = "#66FFFF")
  }
  
  # TODO: add option for picking theme
  p <- p + theme_dark() + theme(legend.position = "none")
  
  print(p)
  return(forest_grouping)
}

locate_neighbors <- function(pixel, forest_grouping) {
  # returns vector of indices of neighboring pixels, including edge cases
  
  neighbor_W_index <- pixel - 1
  neighbor_E_index <- pixel + 1
  neighbor_N_index <- pixel - length(unique(forest_grouping$x))
  neighbor_S_index <- pixel + length(unique(forest_grouping$x))
  
  neighbor_NW_index <- neighbor_N_index - 1
  neighbor_NE_index <- neighbor_N_index + 1
  neighbor_SW_index <- neighbor_S_index - 1
  neighbor_SE_index <- neighbor_S_index + 1
  
  # NW corner:
  if (forest_grouping$y[pixel] == max(forest_grouping$y) & forest_grouping$x[pixel] == min(forest_grouping$x)) {
    # exclude pixels north and west of search pixel
    neighbor_indices <- c(neighbor_E_index, neighbor_S_index,
                          neighbor_SE_index)
    return(neighbor_indices)
  }
  
  # NE corner:
  if (forest_grouping$y[pixel] == max(forest_grouping$y) & forest_grouping$x[pixel] == max(forest_grouping$x)) {
    # exclude pixels north and east of search pixel
    neighbor_indices <- c(neighbor_W_index, neighbor_S_index,
                          neighbor_SW_index)
    return(neighbor_indices)
  }
  
  # SW corner:
  if (forest_grouping$y[pixel] == min(forest_grouping$y) & forest_grouping$x[pixel] == min(forest_grouping$x)) {
    # exclude pixels south and west of search pixel
    neighbor_indices <- c(neighbor_E_index, neighbor_N_index,
                          neighbor_NE_index)
    return(neighbor_indices)
  }
  
  # SE corner:
  if (forest_grouping$y[pixel] == min(forest_grouping$y) & forest_grouping$x[pixel] == max(forest_grouping$x)) {
    # exclude pixels south and east of search pixel
    neighbor_indices <- c(neighbor_W_index, neighbor_N_index,
                          neighbor_NW_index)
    return(neighbor_indices)
  }
  
  # N border:
  if (forest_grouping$y[pixel] == max(forest_grouping$y)) {
    # exclude pixels north of search pixel
    neighbor_indices <- c(neighbor_W_index, neighbor_E_index, neighbor_S_index,
                          neighbor_SW_index, neighbor_SE_index)
    return(neighbor_indices)
  }
  
  # S border:
  if (forest_grouping$y[pixel] == min(forest_grouping$y)) {
    # exclude pixels south of search pixel
    neighbor_indices <- c(neighbor_W_index, neighbor_E_index, neighbor_N_index,
                          neighbor_NW_index, neighbor_NE_index)
    return(neighbor_indices)
  }
  
  # W border:
  if (forest_grouping$x[pixel] == min(forest_grouping$x)) {
    # exclude pixels west of search pixel
    neighbor_indices <- c(neighbor_N_index, neighbor_E_index, neighbor_S_index,
                          neighbor_NE_index, neighbor_SE_index)
    return(neighbor_indices)
  }
  
  # east border:
  if (forest_grouping$x[pixel] == max(forest_grouping$x)) {
    # exclude pixels east of search pixel
    neighbor_indices <- c(neighbor_W_index, neighbor_N_index, neighbor_S_index,
                          neighbor_NW_index, neighbor_SW_index)
    return(neighbor_indices)
  }
  
  # otherwise:
  else {
    neighbor_indices <- c(neighbor_W_index, neighbor_E_index, neighbor_S_index, neighbor_N_index,
                          neighbor_NW_index, neighbor_NE_index, neighbor_SW_index, neighbor_SE_index)
    return(neighbor_indices)
  }
}

mark_neighbors <- function(start_pixel, search_group, forest_grouping, forest_coverage_threshold) {
  # for troubleshooting:
  # cat("i am starting at pixel ", start_pixel, "\n")
  
  # find neighbors of start pixel:
  neighbors <- locate_neighbors(start_pixel, forest_grouping)
  # narrow down to neighbors that are above the coverage threshold and are not yet grouped:
  neighbors <- neighbors[is.na(forest_grouping$group[neighbors]) & forest_grouping$layer[neighbors] > forest_coverage_threshold]
  
  # mark neighboring pixels as part of the search group:
  forest_grouping$group[neighbors] <- search_group
  
  # recursively search and mark neighbors of selected pixel:
  for (new_pixel in neighbors) {
    forest_grouping <- mark_neighbors(start_pixel = new_pixel, search_group = search_group, forest_grouping = forest_grouping,
                                      forest_coverage_threshold = forest_coverage_threshold)
  }
  
  return(forest_grouping)
}

compile_shp_files <- function(shp_directory, countries_to_use, crop = FALSE, crop_limits = NULL) {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  x <- c("long", "lat", "order", "hole", "piece", "id", "group")
  colnames(df) <- x
  compiled_shp_files <- as.data.table(df)
  list_of_shp_points <- list(compiled_shp_files)
  list_index <- 1
  
  for (count in 1:length(countries_to_use)) {
    # read in the shapefile for the selected country:
    shp <- as.data.table(fortify(readOGR(paste0(shp_directory, countries_to_use[count], "_Ad0.shp"), verbose = FALSE)))
    
    # for troubleshooting:
    # cat("shp now contains ", countries_to_use[count], "\n")
    
    if (crop) {
      
      # subset shapefile to crop area:
      shp <- shp[long > crop_limits[1] & long < crop_limits[2] & lat > crop_limits[3] & lat < crop_limits[4]]
      
      # for troubleshooting:
      # cat("we have now cropped shp. its new length is ", nrow(shp), "\n")
    }

    if (nrow(shp) > 0) {
      list_of_shp_points[[list_index]] <- shp
      list_index <- list_index + 1
    }

  }
  
  for (country in 1:length(list_of_shp_points)) {

      for (i in 1:(nrow(list_of_shp_points[[country]])-1)) {
        
        # test if "order" of next row == "order" of this row. if not, there is a break
        # and an NA needs to be introduced for correct graphing
        if (list_of_shp_points[[country]]$order[i] != list_of_shp_points[[country]]$order[i+1] - 1) {
          list_of_shp_points[[country]]$lat[i] <- NA
        }
      }
    }
  
  return(list_of_shp_points)
}

########################################################################
#
# Calculate travel time from each village to nearest forested pixel
#
########################################################################

library(ggforce, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")

# make sure to assign a "home forest" to each village
# include "backup" forests?
# for each forest, display the villages that use it (that consider it their "home forest")

# crop_limits <- c(105.8, 106, 11.5, 11.8)
# crop_limits <- c(105, 106, 11, 12)
crop_limits <- c(106.5, 107.5, 13.5, 14.15)
# crop_limits <- c(106.5, 107.5, 11.5, 13)

# crop_limits <- c(102, 108, 9, 15)

test <- forest_map(data_directory = data_directory, forest_coverage_threshold = 90, country_borders = TRUE,
                   countries_to_use = c("Cambodia", "Lao", "Vietnam", "Thailand", "Myanmar"),
                   crop = TRUE, crop_limits = crop_limits,
                   include_locs = TRUE, locs_filename = paste0(data_directory, "gis_osm_places_free_1.csv"), place_types = c("village"))

forest_pixels <- test[!is.na(group)]

# import the friction surface:
require(gdistance, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
friction.surface.filename <- "/homes/georgoff/forest_data/friction_surface_2015_v1.tif"
point.filename <- "/homes/georgoff/georgoff.github.io/forest_malaria/data/village_points.csv" # Just 2 columns.  Structured as [X_COORD, Y_COORD] aka [LONG, LAT].  Use a header.

# Read in the points table
points <- as.data.table(read.csv(file = point.filename))
points <- points[X_COORD > crop_limits[1] & X_COORD < crop_limits[2] &
                   Y_COORD > crop_limits[3] & Y_COORD < crop_limits[4]]

# Fetch the number of points
temp <- dim(points)
n.points <- temp[1]

#  Define the spatial template
friction <- raster(friction.surface.filename)
fs1 <- crop(friction, extent(crop_limits[1], crop_limits[2], crop_limits[3], crop_limits[4]))
fs1_points <- as.data.table(rasterToPoints(fs1))

# Make and geocorrect the transition matrix (i.e., the graph)
cat("STEP A \n")
T <- transition(fs1, function(x) 1/mean(x), 8) # RAM intensive, can be very slow for large areas
# saveRDS(T, T.filename)
cat("STEP B \n")
T.GC <- geoCorrection(T)                    
# saveRDS(T.GC, T.GC.filename)

pdf("/homes/georgoff/temp/boii.pdf")

points$forest_x <- NA
points$forest_y <- NA
points$forest_group <- NA
points$travel_time <- NA

# for(point in 1) {
for(point in 1) {
  
  cat("STEP 1 \n")
  
  temp.points <- points[point]


  
  # Convert the points into a matrix
  cat("STEP 2 \n")
  xy.data.frame <- data.frame()
  xy.data.frame[1,1] <- temp.points[,1]
  xy.data.frame[1,2] <- temp.points[,2]
  xy.matrix <- as.matrix(xy.data.frame)
  
  # Run the accumulated cost algorithm to make the final output map. This can be quite slow (potentially hours).
  cat("STEP 3 \n")
  temp.raster <- accCost(T.GC, xy.matrix)
  
  # Write the resulting raster
  # writeRaster(temp.raster, paste0(output.filename, "_", point, ".tif"), overwrite = TRUE)
  
  cat("STEP 4 \n")
  points_raster <- as.data.table(rasterToPoints(temp.raster))
  
  # plot(temp.raster)
  # points(temp.points)
  
  cat(point, "\n")
  
  # cycle through all village points (lol we're already cycling through them)
  # cycle through all forested pixels
  # find minimum travel-time-pixel within radius of selected forested pixel
  # assign that travel time to that forest
  # find minimum forest travel time
  # assign that forest to selected village point
  
  
  
  
  
  ##### METHOD 1 #####
  
  
  

  # radius <- 0.02
  # 
  # forest_pixels$travel_time <- NA
  # 
  # cat("STEP 5 \n")
  # for (forest_pixel in 1:nrow(forest_pixels)) {
  #   cat("A")
  #   # travel_time_circle <- points_raster[(x - forest_pixels$x[forest_pixel])^2 + (y - forest_pixels$y[forest_pixel])^2 <= radius^2]
  #   travel_time_circle <- points_raster[x > forest_pixels$x[forest_pixel] - radius &
  #                                         x < forest_pixels$x[forest_pixel] + radius &
  #                                         y > forest_pixels$y[forest_pixel] - radius &
  #                                         y < forest_pixels$y[forest_pixel] + radius]
  #   # print(travel_time_circle)
  #   cat("B")
  #   forest_pixels$travel_time[forest_pixel] <- min(travel_time_circle$layer)
  # 
  #   # p <- ggplot(data = forest_pixels[layer > forest_coverage_threshold], aes(x = x, y = y)) +
  #   #   # TODO: add option for displaying coverage percentage and picking color
  #   # 
  #   #   # plot raster of forested pixels:
  #   #   # geom_raster(aes(fill = layer)) +
  #   #   # scale_fill_gradientn(colours=c("#669933", "#336600")) +
  #   # 
  #   #   geom_raster(data = fs1_points, aes(fill = friction_surface_2015_v1)) +
  #   #   scale_fill_gradientn(colours = c("white", "grey")) +
  #   # 
  #   #   ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%")) +
  #   #   xlab("Longitude") +
  #   #   ylab("Latitude") +
  #   # 
  #   #   # display points for each forested pixel, colored by group:
  #   #   geom_point(data = forest_pixels,
  #   #              aes(colour = factor(group))) +
  #   # 
  #   #   # geom_point(data = forest_pixels[forest_pixel],
  #   #              # aes(size = 20)) +
  #   # 
  #   #   geom_point(data = temp.points, aes(x = X_COORD, y = Y_COORD),
  #   #              shape = 17) +
  #   # 
  #   #   geom_point(data = travel_time_circle, aes(x = x, y = y)) +
  #   # 
  #   #   geom_point(data = )
  #   # 
  #   #   # geom_circle(data = forest_pixels[forest_pixel], aes(x0 = x, y0 = y, r = radius))
  #   # 
  #   # 
  #   # p <- p + theme_classic() + theme(legend.position = "none")
  #   # 
  #   # print(p)
  #   # 
  #   # Sys.sleep(2)
  # }
  # 
  # # points(forest_pixels)
  # 
  # cat("STEP 6 \n")
  # points$forest_x[point] <- forest_pixels$x[forest_pixels$travel_time == min(forest_pixels$travel_time)]
  # points$forest_y[point] <- forest_pixels$y[forest_pixels$travel_time == min(forest_pixels$travel_time)]
  # points$forest_group[point] <- forest_pixels$group[forest_pixels$travel_time == min(forest_pixels$travel_time)]
  # points$travel_time[point] <- forest_pixels$travel_time[forest_pixels$travel_time == min(forest_pixels$travel_time)]
  
  
  
  
  
  
  ##### METHOD 2 #####
  
  
  
  
  # sort travel time points raster in order of increasing travel time
  # cycle through each travel time pixel
  # build square around coordinates with size 0.0083333
  # check if any forested pixels are within square

  # square_size <- 0.0083333
  square_size <- 0.0416666

  sorted_raster <- setorder(points_raster, layer)

  for (i in 1:nrow(sorted_raster)) {
    this_pixel_x <- sorted_raster$x[i]
    # cat("x = ", this_pixel_x, "\n")
    this_pixel_y <- sorted_raster$y[i]
    # cat("y = ", this_pixel_y, "\n")
    search_square <- fs1_points[x > this_pixel_x - 0.5*square_size &
                                  x < this_pixel_x + 0.5*square_size &
                                  y > this_pixel_y - 0.5*square_size &
                                  y < this_pixel_y + 0.5*square_size]
    
    forest_in_square <- forest_pixels[x > this_pixel_x - 0.5*square_size &
                                        x < this_pixel_x + 0.5*square_size &
                                        y > this_pixel_y - 0.5*square_size &
                                        y < this_pixel_y + 0.5*square_size]
    # print("forest_in_square = ", forest_in_square)
    # cat(nrow(forest_in_square), "\n")
    
    p <- ggplot() +
      


      geom_raster(data = fs1_points, aes(x, y, fill = friction_surface_2015_v1, alpha = 0.8)) +
      scale_fill_gradientn(colours = c("white", "grey")) +
      
      ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%")) +
      xlab("Longitude") +
      ylab("Latitude") +
      
      # display points for each forested pixel, colored by group:
      geom_point(data = forest_pixels, aes(x, y, colour = factor(group))) +
      
      # geom_point(data = forest_pixels[forest_pixel],
      # aes(size = 20)) +
      
      geom_point(data = temp.points, aes(x = X_COORD, y = Y_COORD),
                 shape = 17) + 
      
      geom_raster(data = forest_pixels, aes(x, y, fill = layer, alpha = 0.5)) +
      
      geom_point(aes(x = this_pixel_x, y = this_pixel_y)) +
      
      geom_point(data = search_square, aes(x, y), color = "orange", alpha = 0.2) +
      
      geom_point(data = forest_in_square, aes(x, y), color = "red", shape = 8)
    
    # geom_circle(data = forest_pixels[forest_pixel], aes(x0 = x, y0 = y, r = radius))


    p <- p + theme_classic() + theme(legend.position = "none")

    print(p)
    
    Sys.sleep(2)
      

    if (nrow(forest_in_square) > 1) {stop("multiple forests detected")}

    if (nrow(forest_in_square) > 0) {
      points$forest_x[point] <- forest_in_square$x[1]
      points$forest_y[point] <- forest_in_square$y[1]
      points$forest_group[point] <- forest_in_square$group[1]
      points$travel_time[point] <- sorted_raster[x == this_pixel_x & y == this_pixel_y, layer]

      break
    }
  }

  
  
  
}

dev.off()

p <- ggplot(data = test[layer > forest_coverage_threshold], aes(x = x, y = y)) +
  # TODO: add option for displaying coverage percentage and picking color
  
  # plot raster of forested pixels:
  # geom_raster(aes(fill = layer)) +
  # scale_fill_gradientn(colours=c("#669933", "#336600")) +
  
  geom_raster(data = fs1_points, aes(fill = friction_surface_2015_v1)) +
  scale_fill_gradientn(colours = c("white", "grey")) +
  
  ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%")) +
  xlab("Longitude") +
  ylab("Latitude") +
  
  # display points for each forested pixel, colored by group:
  geom_point(data = test[!is.na(test$group)],
             aes(colour = factor(group))) +
  
  geom_point(data = points, aes(x = X_COORD, y = Y_COORD, color = factor(forest_group)),
             shape = 17) +
  
  geom_segment(data = points, aes(x = points$X_COORD, y = points$Y_COORD,
                                  xend = points$forest_x, yend = points$forest_y),
               alpha = 0.2) +
  
  geom_text(data = points, aes(x = points$X_COORD, y = points$Y_COORD, label = travel_time))
  

p <- p + theme_classic() + theme(legend.position = "none")

p
