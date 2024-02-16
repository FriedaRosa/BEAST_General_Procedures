# Load required libraries
library(raster)
library(sf)

# Set your working directory to the folder containing CHELSA .tif files
setwd("~/PhD_Projects/StaticPredictors/Data/Chelsa_Bio_PET/")

# List all the .tif files in the directory
tif_files <- list.files(pattern = ".tif")

# Create an empty raster stack to store all layers
raster_stack <- stack()

# Loop through each .tif file and add it to the raster stack
for (file in tif_files) {
  raster_stack <- stack(raster_stack, raster(file))
}



# Define the target resolution in meters
target_resolution <- c(10)  # 10x10km

# Aggregate the raster stack to the target resolution
raster_aggregated <- aggregate(raster_stack, fact = target_resolution, fun = mean)

# Now, raster_aggregated contains the gridded climate layer with cells aggregated to 10x10km

# Proceed to extract the averaged values from the raster for each cell
# I'll provide the code for this once we confirm the grid size is correct


# Define the spatial resolution (10x10km)
resolution <- 10000  # meters

# Loop through each layer in the stack and resample to 10x10km
resampled_stack <- stack()
for (i in 1:nlayers(raster_stack)) {
  resampled_layer <- aggregate(raster_stack[[i]], fact = 10, fun = mean, na.rm = TRUE)
  resampled_stack <- stack(resampled_stack, resampled_layer)
}

# Create a SpatialGridDataFrame with the desired resolution
grid <- makegrid(resampled_stack, n = resolution)

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
