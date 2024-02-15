# Load required libraries
library(raster)
library(sf)
library(terra)

# Set your working directory to the folder containing CHELSA .tif files
setwd("~/PhD_Projects/StaticPredictors/Data/Chelsa_Bio_PET/")

# List all the .tif files in the directory
tif_files <- list.files(pattern = ".tif")

##
tif_files <- tif_files[1]
##
project_crs <- '+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs'
# Use lapply to read and stack all .tif files
raster_list <- lapply(tif_files, rast)
raster_list2 <- lapply(raster_list, function(i){
  terra::project(raster_list[[i]], project_crs)
})
raster_stack <- stack(raster_list)

# Use lapply to resample each layer in the stack




resampled_list <- lapply(1:nlayers(raster_stack), function(i) {
  aggregate(raster_list[[i]], fact = 10, fun = mean, na.rm = TRUE)
})
resampled_stack <- stack(resampled_list)



#########
grid_world <- vect("~/PhD_Projects/StaticPredictors/Data/AOOGrid_10x10kmShp/AOOGrid_10x10km.shp")




l_values <- terra::extract(resampled_stack[[1]], grid_world, fun = mean) # all values per grid cell

#######



# Get the coordinates of the resampled stack
coords <- coordinates(resampled_stack[[1]])

# Create a raster grid based on the coordinates
raster_grid <- rasterFromXYZ(coords, res = 0.9)

# Create a SpatialGridDataFrame with the desired resolution
grid <- makegrid(resampled_stack[[1]], n = resolution)

# Extract values at each grid cell
extracted_data <- extract(resampled_stack, grid)

# Convert the extracted data to a data frame
extracted_df <- as.data.frame(extracted_data)

# Add the coordinates to the data frame
coordinates(extracted_df) <- grid

# Save the extracted data to a CSV file
write.csv(extracted_df, file = "extracted_data.csv", row.names = FALSE)

# Display the first few rows of the extracted data
head(extracted_df)
