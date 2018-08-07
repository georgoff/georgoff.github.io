files <- list.files("/homes/georgoff/forest_data/ForestMalariaData/ForestRasterData/")

num_files <- length(files)
i <- 1

pdf("/homes/georgoff/forest_data/forest-types.pdf")

for(file in files) {
  temp_raster <- raster(paste0("/homes/georgoff/forest_data/ForestMalariaData/ForestRasterData/", file))
  plot(temp_raster)
  title(file, cex.main = 0.65)
  
  cat(i, " of ", num_files, "\n")
  
  i <- i+1
}

dev.off()