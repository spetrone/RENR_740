library(fireexposuR)

# load the terra library for spatial data functions
library(terra)


install.packages("raster")
library(raster)

# Load the raster data
r <- raster("~/MF/RENR_740/RENR-740_Case_Study/fuel_grids/ab_fuel_100m_2014.tif")

# Get the minimum and maximum values of the raster
min_value <- cellStats(r, stat = 'min')
max_value <- cellStats(r, stat = 'max')

# Normalize the raster: (Value - Min) / (Max - Min)
normalized_raster <- (r - min_value) / (max_value - min_value)

# Save the normalized raster to a new file
writeRaster(normalized_raster, "~/MF/RENR_740/RENR-740_Case_Study/fuel_grids/ab_fuel_100m_2014_norm.tif", format = "GTiff")
#> Warning: package 'terra' was built under R version 4.4.2
#> terra 1.8.15
vignette("prep-input-data")
# read example hazard data



hazard <- terra::rast( "~/MF/RENR_740/RENR-740_Case_Study/fuel_grids/ab_fuel_100m_2014_norm.tif")
summary(hazard)
# read example polygon geometry for area of interest boundary
geom <- "~/MF/RENR_740/RENR-740_Case_Study/fuel_grids/perimeter.shp"

# use geometry to make an area of interest polygon
aoi <- terra::vect(geom)
# Assign a new CRS to the shapefile (optional)
print(aoi)

# compute long-range ember exposure by setting transmission distance to "l"
exposure <- fire_exp(hazard, tdist = "l")

# map the full extent of the exposure raster with a continuous scale
fire_exp_map_cont(exposure)

# map exposure classes within the area of interest with a base map
fire_exp_map_class(exposure, aoi, classify = "landscape", zoom_level = 13)

# compute directional exposure toward the value with default parameters
dir_exposure <- fire_exp_dir(exposure, aoi)

#------------------- map together ----------------------------
# Load your perimeter shapefile (vector data)
shapefile_path <- "~/MF/RENR_740/RENR-740_Case_Study/fuel_grids/perimeter.shp"
perimeter <- vect(shapefile_path)

# fire exposure raster layer
raster_layer <- fire_exp_map_class(exposure, aoi, classify = "landscape", zoom_level = 13)

plot.new()
# Plot the raster layer
plot(raster_layer, main = "Map with Perimeter")

# Add the perimeter (polygon) on top of the raster
plot(perimeter, add = TRUE, col = "red", border = "black", lwd = 2)

# Add a legend if needed
legend("topright", legend = "Perimeter", fill = "red", border = "black")
# Check the CRS of both the raster and the perimeter
print(crs(raster_layer))
print(crs(perimeter))

# If the CRS doesn't match, reproject the perimeter to match the raster CRS
if (crs(raster_layer) != crs(perimeter)) {
  perimeter <- project(perimeter, crs(raster_layer))
}