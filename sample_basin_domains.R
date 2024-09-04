 # Natasha Torres
 # 09/03/2024

# Code to augment bounding box of shapefiles by 50 meters (20 total)

# load library
library(terra)

# load shapefile
polygons <- vect("data/niko_shp/KNP_sample_basins.shp")
# plot(polygons)

# create an output directory if it doesn't exist
output_dir <- "outputs/new_shp"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# loop through each polygon
for (i in 1:nrow(polygons)) {
  # extract the individual polygon
  polygon <- polygons[i, ]
  
  # calculate the bounding box
  bbox <- ext(polygon)
  
  # create a SpatVector from the bounding box
  bbox_polygon <- as.polygons(bbox, crs(polygon))
  
  # add a 50-meter buffer to the bounding box
  buffered_bbox <- buffer(bbox_polygon, 50)
  
  # crop the original polygon with the buffered bounding box
  cropped_polygon <- crop(polygon,buffered_bbox)
  
  # create a filename for the output shapefile
  filename <- sprintf("polygon_%03d.shp", i)
  
  # save the buffered polygon as a new shapefile
  output_path <- file.path(output_dir, filename)
  writeVector(cropped_polygon, output_path, overwrite = TRUE)
}

# Check the result
test <- vect("outputs/new_shp/polygon_040.shp")
plot(test)
