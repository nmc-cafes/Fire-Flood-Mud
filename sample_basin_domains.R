 # Natasha Torres
 # 09/03/2024

# Code to augment bounding box of shapefiles by 50 meters (20 total)

# load library
library(terra)
library(here)

# load shapefile
fires <- c("KNP","Caldor","CedarCreek","CubCreek2","Dixie")
for(fire in fires){
  cat(fire,"\n")
  polygons <- vect(here(fire,paste0(fire,"_sample_basins.shp")))
  # plot(polygons)
  
  # loop through each polygon
  for (i in 1:nrow(polygons)) {
    basin_name <- paste0(substr(fire, start = 1, stop = 3),i)
    cat("   ",basin_name,"\n")
    
    # create an output directory if it doesn't exist
    output_dir <- here(fire,"Sample_Basins",basin_name)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    }
    
    # extract the individual polygon
    polygon <- polygons[i, ]
    
    # calculate the bounding box
    bbox <- ext(polygon)
    
    # create a SpatVector from the bounding box
    bbox_polygon <- as.polygons(bbox, crs(polygon))
    
    # add a 50-meter buffer to the bounding box
    buffered_bbox <- buffer(bbox_polygon, 50, joinstyle="mitre")
    
    # create a filename for the output shapefile
    filename <- paste0(basin_name,".shp")

    # save the buffered polygon as a new shapefile
    output_path <- here(output_dir,paste0(basin_name,".shp"))
    writeVector(buffered_bbox, output_path, overwrite = TRUE)
  }
}


# Check the result
test_fire <- "Caldor"
test_num <- 5
test_name <- paste0(substr(test_fire, start = 1, stop = 3),test_num)
test_basins <- vect(here(test_fire,paste0(test_fire,"_sample_basins.shp")))
test_basin <- test_basins[test_num, ]
test_bbox <- vect(ext(test_basin), crs=crs(test_basin))
test_domain <- vect(here(test_fire,"Sample_Basins",test_name,paste0(test_name,".shp")))
plot(test_domain)
plot(test_basin, add=T)
plot(test_bbox, lty="dashed", add=T)

