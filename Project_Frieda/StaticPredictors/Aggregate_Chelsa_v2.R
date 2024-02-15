rm(list=ls())

library(sf)

library(terra)

# Set your working directory to the folder containing CHELSA .tif files
setwd("~/PhD_Projects/StaticPredictors/Data/Chelsa_Bio_PET/")

library(raster)

# List all the .tif files in the directory
tif_files <- list.files(pattern = "\\.tif$")

# Create an empty list to store aggregated rasters
aggregated_rasters <- list()

# Now, raster_stack contains the aggregated climate layers
grid_world <- vect("~/PhD_Projects/StaticPredictors/Data/AOOGrid_10x10kmShp/AOOGrid_10x10km.shp")


# Loop through each .tif file and aggregate it to the target resolution
for (file in tif_files) {
  # Read the raster
  raster_layer <- raster(file)
  
  # Aggregate the raster to the target resolution
  aggregated_raster <- aggregate(raster_layer, fact = c(100), fun = mean)
  aggregated_raster <- rast(aggregated_raster)
  l_values <- extract(aggregated_raster, grid_world, fun = mean)
  # Store the aggregated raster in the list
  aggregated_rasters[[file]] <- l_values
}


