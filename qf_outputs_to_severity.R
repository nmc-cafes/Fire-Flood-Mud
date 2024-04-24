"
Read in quicfire outputs as rasters and match them to MTBS severity

Things to loop through
  - Fires
  - Sites
  - Plot size
  - QF outputs
"

library(here)
library(terra)
library(tidyverse)
library(tidyterra)

#### DEFINE FUNCTIONS ####
ext_buff <- function(rst, buffer = -50){
  new_ext <- ext(rst)
  new_ext[1] <- new_ext[1] - buffer
  new_ext[2] <- new_ext[2] + buffer
  new_ext[3] <- new_ext[3] - buffer
  new_ext[4] <- new_ext[4] + buffer
  return(new_ext)
}

output_to_rst <- function(output, out_arr, plot_bounds){
  out_mat <- as.matrix(out_arr)
  out_rst <- rast(nrow = dim(out_mat)[1],
                  ncol = dim(out_mat)[2],
                  crs = crs(plot_bounds),
                  extent = ext(plot_bounds),
                  vals = out_mat,
                  names = output)
  out_rst <- flip(out_rst, direction="vertical")
  return(out_rst)
}

pts_to_pol <- function(mtbs_pts){
  mtbs_buf <- buffer(mtbs_pts, width = sqrt(30^2+30^2), quadsegs = 1)
  mtbs_xy <- crds(mtbs_pts)
  mtbs_pol <- spin(mtbs_buf, 45, mtbs_xy[,1], mtbs_xy[,2])
  return(mtbs_pol)
}


#### ASSEMBLE DATASET ####
fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
sizes <- c("duet")
outputs <- c("mass_burnt_pct",
             "surface_consumption",
             "surface_remaining",
             "canopy_consumption",
             "canopy_remaining",
             "max_power",
             "residence_time_power",
             "residence_time_consumption",
             "max_reaction_rate")


first_fire <- T
rm(out_vect,size_vect,site_vect,fire_vect)
for(fire in fires){
  cat(fire,"\n")
  mtbs <- rast(here(fire,paste0(fire,"_dNBR.tif")))
  names(mtbs) <- "dNBR"
  severity <- rast(here(fire, paste0(fire,"_Severity.tif")))
  names(severity) <- "severity"
  dem <- rast(here(fire, paste0(fire,"_DEM.tif")))
  slope <- terrain(dem, v="slope", unit="degrees")
  slope <- terra::project(slope,mtbs,method="bilinear")
  names(slope) <- "slope"
  sites <- list.dirs(here("Arrays",fire), full.names = F, recursive = F)
  for(i in 1:length(sites)){
    sites[i] <- str_split(sites[i], "_")[[1]][2]
  }
  first_site <- T
  for(site in sites){
    cat(site,"\n")
    first_size <- T
    for(size in sizes){
      cat(size,"\n")
      plot_bounds <- vect(here(fire,
                               "Sample_Sites",
                               site,
                               paste0("500m"),
                               paste0(site,"_bounds_500m.shp")))
      mtbs_crop <- crop(mtbs, ext(plot_bounds))
      mtbs_pts <- as.points(mtbs_crop)
      sev_pts <- terra::extract(severity, mtbs_pts, bind=T, ID=F)
      site_pts <- terra::extract(slope, sev_pts, bind=T, ID=F)
      site_pol <- pts_to_pol(site_pts)
      site_pol <- crop(site_pol, ext_buff(plot_bounds))
      first_output <- T
      for(output in outputs){
        cat("\t",output,"\n")
        out_arr <- read.table(here("Duet_Arrays",
                                   fire, 
                                   paste0(fire,"_",site,"_",size),
                                   "Arrays",
                                   paste0(output,".txt")))
        out_rst <- output_to_rst(output, out_arr, plot_bounds)
        out_crop <- crop(out_rst, ext_buff(plot_bounds))
        if(output=="canopy_consumption"){
          plot(out_crop)
        }
        if(first_output){
          out_vect <- terra::extract(out_crop,
                                     site_pol,
                                     fun=mean,
                                     na.rm=T,
                                     bind=T)
        } else{
          out_vect <- terra::extract(out_crop,
                                     out_vect,
                                     fun=mean,
                                     na.rm=T,
                                     bind=T)
        }
        first_output <- F
      }
      out_vect$size <- size
      out_vect$site <- site
      out_vect$fire <- fire
      if(first_size){
        size_vect <- out_vect
      } else{
        size_vect <- bind_spat_rows(size_vect, out_vect)
      }
      first_size <- F
    }
    if(first_site){
      site_vect <- size_vect
    } else{
      site_vect <- bind_spat_rows(site_vect,size_vect)
    }
    first_site <- F
  }
  if(first_fire){
    fire_vect <- site_vect
  } else{
    fire_vect <- bind_spat_rows(fire_vect, site_vect)
  }
  first_fire <- F
}

all_data <- as_tibble(fire_vect)
write.csv(all_data, here("all_data_duet.csv"), row.names = F)
