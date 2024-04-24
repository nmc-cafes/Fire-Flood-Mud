library(here)
library(tidyverse)
library(terra)
library(tidyterra)

## Functions
clip_to_fire <- function(x, perimeter, EPSG){
  x <- project(x, paste0("EPSG:",EPSG))
  x_crop <- crop(x, perimeter)
  x_mask <- mask(x_crop, perimeter)
  return(x_mask)
}


slope_mask <- function(fire_name){
  DEM <- rast(here(fire_name,paste0(fire_name,"_DEM.tif")))
  slope <- terrain(DEM, v="slope", neighbors=8, unit="degrees")
  rcl_mat <- matrix(c(0,23,NA,
                      23, Inf, 1),
                    nrow = 2,
                    byrow =  T)
  slope_rcl <- classify(slope, rcl_mat, right = F)
  return(slope_rcl)
}

drainage_proximity <- function(fire_name, streams, buffer_outer, buffer_inner, EPSG, overwrite = T){
  print("      Generating outer buffer...")
  drainage_buffer_outer <- buffer(streams, buffer_outer)
  drainage_buffer_outer <- aggregate(drainage_buffer_outer, dissolve=T)
  print("      Generating inner buffer...")
  drainage_buffer_inner <- buffer(streams, buffer_inner)
  drainage_buffer_inner <- aggregate(drainage_buffer_inner, dissolve=T)
  print("      Removing inner polygon...")
  drainage_buffer <- erase(drainage_buffer_outer, drainage_buffer_inner)
  print("      Saving file...")
  writeVector(drainage_buffer,
              here(fire_name,paste0(fire_name,"_drainage_prox_",buffer_inner,"-",buffer_outer,".shp")),
              overwrite = overwrite)
  return(drainage_buffer)
}

high_severity <- function(fire_name, perimeter, EPSG){
  severity <- rast(here(fire_name, paste0(fire_name,"_Severity.tif")))
  severity_clip <- clip_to_fire(severity, perimeter, EPSG)
  rcl_mat = matrix(c(0,2,NA,
                     3,4,1,
                     5,Inf, NA),
                   nrow = 3,
                   byrow = T)
  severity_classify <- classify(severity, rcl_mat, right=NA)
  return(severity_classify)
}

filter_distance <- function(raster, size, min_dist, max_iters=1e6){
  reso <- res(raster)[1]
  CRS <- crs(raster)
  dat <- as.matrix(raster, wide=T)
  dat <- apply(dat, 2, rev)
  r <- min_dist/reso
  # Construct a rejection kernel based on the radius
  x <- seq(-r, r)
  y <- seq(-r, r)
  xx <- expand.grid(x=x,y=y)$x
  yy <- expand.grid(x=x,y=y)$y
  rejection_kernel <- sqrt(xx^2 + yy^2)
  rejection_kernel[rejection_kernel <= r] <- 0
  rejection_kernel[rejection_kernel > r] <- 1
  rejection_kernel <- matrix(rejection_kernel, nrow = length(x), byrow = T)
  
  rejection_grid <- dat
  rejection_grid[is.na(rejection_grid)] <- -99
  samples <- data.frame("x"=rep(NA,size),"y"=rep(NA,size))
  nc <- 1
  iters <- 1
  while(nc <= size){
    i <- sample(seq(r, nrow(dat)-r),1)
    j <- sample(seq(r, ncol(dat)-r),1)
    if(rejection_grid[i,j]==1){
      if(all(rejection_grid[(i-r):(i+r),(j-r):(j+r)]!=0)){
        rejection_grid[(i-r):(i+r),(j-r):(j+r)] <- rejection_grid[(i-r):(i+r),(j-r):(j+r)] * rejection_kernel
        samples$x[nc] <- j
        samples$y[nc] <- i
        nc <- nc+1
      }
    }
    iters <- iters+1
    progress_bar(iters,max_iters)
    if(iters > max_iters){
      cat('Maximum number of iterations reached.\n')
      cat("number of points found:", nc-1,"\n")
      break
    }
  }
  samples$x <- samples$x * reso + xmin(raster)
  samples$y <- samples$y * reso + ymin(raster)
  vector <- vect(samples, geom=c("x", "y"), crs=CRS, keepgeom=FALSE)
  return(vector)
}

EPSG <- 5070
all_streams <- vect(here("Streams_NorthAmerica","riv_pfaf_7_MERIT_Hydro_v07_Basins_v01_bugfix1.shp"))

## Process fire perimeters 
# ca_fires <- c("DIXIE","CALDOR","KNP COMPLEX")
PERIMETERS_DONE = TRUE
if(PERIMETERS_DONE==FALSE){
  fires <- c("DIXIE","CALDOR","KNP COMPLEX","CUB CREEK 2","CEDAR CREEK")
  mtbs <- vect(here("mtbs_perimeter_data","mtbs_perims_DD.shp"))
  mtbs$Ig_Date <- as.Date(mtbs$Ig_Date)
  recent_fires <- mtbs[mtbs$Ig_Date > as.Date("2020-01-01")]
  focal_fires <- recent_fires[startsWith(recent_fires$Event_ID,"WA") | startsWith(recent_fires$Event_ID,"CA")]
  for(fire in fires){
    focal_fire <- focal_fires[focal_fires$Incid_Name==fire]
    if(nrow(focal_fire)==1){
      focal_fire <- project(focal_fire, paste0("EPSG:",EPSG))
      plot(focal_fire)
      writeVector(focal_fire,here("Fire_Perimeters",paste0(fire,".shp")),overwrite=T)
    } else
      print("multiple fires of same name in same state: ", fire)
  }
}

# NOTE: Manually move each perimeter from Fire_Perimeters to the directories
# of their respective fires, and rename them with the directory name, followed
# by _perimeter.shp

fires <- c("Dixie","Caldor","KNP","CubCreek2","CedarCreek")

## Process DEMs
DEM_DONE = TRUE
if(DEM_DONE == FALSE){
  for(fire in fires){
    print(fire)
    perimeter <- vect(here(fire,paste0(fire,"_perimeter.shp")))
    file_list <- list.files(here(fire,"DEM"))
    tile_list <- list()
    for(i in 1:length(file_list)){
      tile_list[i] <- rast(here(fire,"DEM",file_list[i]))
    }
    print("   -collecting")
    collection <- sprc(tile_list)
    print("   -merging")
    merged <- mosaic(collection)
    print("   -clipping")
    merged_clip <- clip_to_fire(merged, perimeter, EPSG)
    print("   -plotting")
    plot(merged_clip)
    print("   -writing")
    writeRaster(merged_clip, here(fire,paste0(fire,"_DEM.tif")), overwrite = T)
  }
}

## Select Sites
for(fire_name in fires){
  print(fire_name)
  perimeter <- vect(here(fire_name,paste0(fire_name,"_perimeter.shp")))
  # limit to within 800m of (but at least 200m from) a drainage
  print("   drainages")
  streams_clip <- clip_to_fire(all_streams, perimeter, EPSG)
  drainage_zone <- drainage_proximity(fire_name, streams_clip, 800, 200, 5070)
  drainage_zone <- vect(here(fire_name,paste0(fire_name,"_drainage_prox_200-800.shp")))
  # must be at least 500m from the edge of the fire perimeter
  print("   border")
  perimeter_buffer <- buffer(perimeter, -500)
  drainages_fire <- clip_to_fire(drainage_zone, perimeter_buffer, EPSG)
  # slope must be at least 23 deg
  print("   slope")
  slope_23 <- slope_mask(fire_name)
  # severity must be medium or high
  print("   severity")
  severity <- high_severity(fire_name, perimeter_buffer, EPSG)
  # combine all 4 criteria
  print("   combine")
  drainage_23 <- clip_to_fire(slope_23, drainages_fire, EPSG)
  drainage_23_project <- project(drainage_23, severity, method = "near")
  drainage_23_severe <- severity * drainage_23_project
  # randomly sample sites, making sure they are at least 1km apart
  print("   sample")
  sample_sites_spaced <- filter_distance(drainage_23_severe, size = 5, min_dist = 1000)
  if(nrow(sample_sites_spaced)==5){
    print("   write")
    writeVector(sample_sites_spaced, here(fire_name,paste0(fire_name,"_sample_sites_NJT.shp")),overwrite=T)
  } else{
    print("Not enough sample sites. Consider increasing spatSample size")
  }
}

