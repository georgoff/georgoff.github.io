########################################################################
#
# Install necessary packages
#
########################################################################

load_required_packages <- function() {
  library(raster, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  library(ggplot2, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  library(data.table, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  library(rgdal, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
  library(ggmap, lib.loc = "/ihme/malaria_modeling/georgoff/Rlibs/")
}

load_required_packages()

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
  
  forest_percent <- files[grepl(as.character(year_to_use), files) & grepl("percentage", files)]
  
  EN_raster_file <- forest_percent[grepl("Evergreen_Needleleaf", forest_percent)]
  EB_raster_file <- forest_percent[grepl("Evergreen_Broadleaf", forest_percent)]
  DN_raster_file <- forest_percent[grepl("Deciduous_Needleleaf", forest_percent)]
  DB_raster_file <- forest_percent[grepl("Deciduous_Broadleaf", forest_percent)]
  MIX_raster_file <- forest_percent[grepl("Mixed_Forest", forest_percent)]
  
  EN_raster <- raster(paste0(raster_directory, EN_raster_file))
  EB_raster <- raster(paste0(raster_directory, EB_raster_file))
  DN_raster <- raster(paste0(raster_directory, DN_raster_file))
  DB_raster <- raster(paste0(raster_directory, DB_raster_file))
  MIX_raster <- raster(paste0(raster_directory, MIX_raster_file))
  
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

# forest_map(data_directory = data_directory, forest_coverage_threshold = 75, country_borders = TRUE,
#            countries_to_use = c("Cambodia", "Lao", "Vietnam", "Thailand", "Myanmar"),
#            crop = TRUE, crop_limits = crop_limits)

forest_map <- function(data_directory, forest_coverage_threshold, year_to_use = 2013,
                       crop = FALSE, crop_limits = NULL,
                       country_borders = FALSE, countries_to_use = NULL) {
  
  forest_grouping <- sum_forest_types(data_directory = data_directory, year_to_use = year_to_use,
                                      crop_rasters = crop, crop_limits = crop_limits)
  forest_grouping <- rasterToPoints(forest_grouping)
  forest_grouping <- as.data.table(forest_grouping)
  
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
    # geom_raster(aes(fill = layer)) +
    # scale_fill_gradientn(colours=c("white", "green")) +
    ggtitle(paste0("Forest Coverage Greater Than ", as.character(forest_coverage_threshold), "%")) +
    xlab("Longitude") +
    ylab("Latitude") +
    geom_point(data = forest_grouping[!is.na(forest_grouping$group)],
               aes(colour = factor(group)))

  
  if (country_borders) {
    shp_tables <- compile_shp_files(shp_directory = paste0(data_directory, "AdminShapefiles/"), countries_to_use = countries_to_use, crop = crop,
                                    crop_limits = crop_limits)
    
    for (i in 1:length(shp_tables)) {
      p <- p + geom_path(data = shp_tables[[i]], aes(x = long, y = lat, group = group), color = "white", na.rm = FALSE)
    }
  }

  # TODO: add option for picking theme
  p <- p + theme_dark() + theme(legend.position = "none")
  
  print(p)
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
  # cat("i am starting at pixel ", start_pixel, "\n")
  
  neighbors <- locate_neighbors(start_pixel, forest_grouping)
  # narrow down to neighbors that are above the coverage threshold and are not yet grouped:
  neighbors <- neighbors[is.na(forest_grouping$group[neighbors]) & forest_grouping$layer[neighbors] > forest_coverage_threshold]
  
  # mark neighboring pixels as part of the search group:
  forest_grouping$group[neighbors] <- search_group
  
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
    shp <- as.data.table(fortify(readOGR(paste0(shp_directory, countries_to_use[count], "_Ad0.shp"), verbose = FALSE)))
    # cat("shp now contains ", countries_to_use[count], "\n")
    if (crop) {
      shp <- shp[long > crop_limits[1] & long < crop_limits[2] & lat > crop_limits[3] & lat < crop_limits[4]]
      # cat("we have now cropped shp. its new length is ", nrow(shp), "\n")
    }

    if (nrow(shp) > 0) {
      list_of_shp_points[[list_index]] <- shp
      list_index <- list_index + 1
    }

  }
  
  for (country in 1:length(list_of_shp_points)) {
    # cat("I'm working on ", list_of_shp_points, ". Its length is ", nrow(list_of_shp_points[[country]]))
    # if (nrow(list_of_shp_points[[country]]) > 1) {
    #   for (i in 1:(nrow(list_of_shp_points[[country]])-1)) {
    #     if (list_of_shp_points[[country]]$order[i] != list_of_shp_points[[country]]$order[i+1] - 1) {
    #       list_of_shp_points[[country]]$lat[i] <- NA
    #     }
    #   }
      
      for (i in 1:(nrow(list_of_shp_points[[country]])-1)) {
        if (list_of_shp_points[[country]]$order[i] != list_of_shp_points[[country]]$order[i+1] - 1) {
          list_of_shp_points[[country]]$lat[i] <- NA
        }
      }
    }
  
  return(list_of_shp_points)
}
