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

percent_rcl <- function(rst, threshold){
  rcl_mat <- matrix(c(0,threshold,NA,
                      threshold,1.0,1),
                    nrow=2,
                    byrow=T)
  rst_cls <- classify(rst, rcl_mat, include.lowest=T)
  return(rst_cls)
}

drainage_proximity <- function(fire_name, fire_perimeter, streams, buffer_inner, EPSG, save = F, overwrite = T){
  print("      Generating inner buffer...")
  drainage_buffer_inner <- buffer(streams, buffer_inner)
  drainage_buffer_inner <- aggregate(drainage_buffer_inner, dissolve=T)
  print("      Removing areas near drainages...")
  drainage_buffer <- erase(perimeter, drainage_buffer_inner)
  if(save){
    print("      Saving file...")
    writeVector(drainage_buffer,
                here(fire_name,paste0(fire_name,"_drainage_prox_",buffer_inner,".shp")),
                overwrite = overwrite)
  }
  return(drainage_buffer)
}

severity_pct <- function(fire_name, perimeter, severity_class, EPSG){
  severity <- rast(here(fire_name, paste0(fire_name,"_Severity.tif")))
  severity_clip <- clip_to_fire(severity, perimeter, EPSG)
  if(severity_class=="high"){
    rcl_mat = matrix(c(0,3,0,
                       4,4,1,
                       5,Inf, 0),
                     nrow = 3,
                     byrow = T)
  } else if(severity_class=="moderate"){
    rcl_mat = matrix(c(0,2,0,
                       3,3,1,
                       4,Inf, 0),
                     nrow = 3,
                     byrow = T)
  } else if(severity_class=="low"){
    rcl_mat = matrix(c(0,0,0,
                       1,2,1,
                       3,Inf, 0),
                     nrow = 3,
                     byrow = T)
  }
  severity_classify <- classify(severity, rcl_mat, right=NA)
  sev_pct <- focal(severity_classify, w=17, fun = "mean", na.policy = "omit", na.rm=T)
  names(sev_pct) <- severity_class
  return(sev_pct)
}

stratify_severity <- function(fire_name, perimeter_buffer, threshold, epsg, write=NULL){
  high_sev_pct <- severity_pct(fire_name, perimeter_buffer, "high", epsg)
  mod_sev_pct <- severity_pct(fire_name, perimeter_buffer, "moderate", epsg)
  low_sev_pct <- severity_pct(fire_name, perimeter_buffer, "low", epsg)
  high_severity <- percent_rcl(high_sev_pct, threshold)
  moderate_severity <- percent_rcl(mod_sev_pct, threshold)
  low_severity <- percent_rcl(low_sev_pct, threshold)
  severity_stack <- c(high_severity,moderate_severity,low_severity)
  if(!is.null(write)){
    writeRaster(severity_stack, here(fire_name, write), overwrite=T)
  }
  return(severity_stack)
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

sample_stratified <- function(rst_stk, size){
  if(size%%6!=0){
    stop("size must be divisible by 6")
  }
  size_strat <- size/6
  j <- 1
  for(sev in c("high","moderate","low")){
    for(slope in c("steep","shallow")){
      layer <- paste(sev,slope,sep="-")
      sites <- filter_distance(rst_stk[layer], size=size_strat, min_dist=1000)
      sites$severity=sev
      sites$slope=slope
      if(j==1){
        sample_sites <- sites
      } else{
        sample_sites <- rbind(sample_sites, sites)
      }
      j <- j+1
    }
  }

  return(sample_sites)
}

EPSG <- 5070

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

## Select Sites
all_streams <- vect(here("Streams_NorthAmerica","riv_pfaf_7_MERIT_Hydro_v07_Basins_v01_bugfix1.shp"))
for(fire_name in fires){
  print(fire_name)
  perimeter <- vect(here(fire_name,paste0(fire_name,"_perimeter.shp")))
  # limit to within 800m of (but at least 200m from) a drainage
  print("   drainages")
  streams_clip <- clip_to_fire(all_streams, perimeter, EPSG)
  upslope_zone <- drainage_proximity(fire_name, perimeter, streams_clip, 200, 5070, save = T)
  # must be at least 500m from the edge of the fire perimeter
  print("   border")
  perimeter_buffer <- buffer(perimeter, -500)
  upslope_fire <- clip_to_fire(upslope_zone, perimeter_buffer, EPSG)
  # stratify across severity
  print("   severity")
  severity_stack <- stratify_severity(fire_name, perimeter_buffer, epsg=EPSG, threshold = 0.5)
  # combine all 4 criteria
  print("   combine")
  upslope <- clip_to_fire(slope_stack, upslope_fire, EPSG)
  upslope_project <- project(upslope, severity_stack, method = "near")
  steep_severity <- severity_stack * upslope_project$steep
  names(steep_severity) <- c("high-steep","moderate-steep","low-steep")
  shallow_severity <- severity_stack * upslope_project$shallow
  names(shallow_severity) <- c("high-shallow","moderate-shallow","low-shallow")
  upslope_severity <- c(steep_severity,shallow_severity)
  # randomly sample sites, making sure they are at least 1km apart
  print("   sample")
  size <- 18
  sample_sites <- sample_stratified(upslope_severity, size)
  sample_sites$site_name <- NA
  for(i in 1:length(sample_sites)){
    sample_sites$site_name[i] <- paste0(fire_name,i)
  }
  if(nrow(sample_sites)==size){
    print("   write")
    writeVector(sample_sites, here(fire_name,paste0(fire_name,"_sample_sites_NEW2.shp")),overwrite=T)
  } else{
    print("Not enough sample sites. Consider increasing spatSample size")
  }
}


