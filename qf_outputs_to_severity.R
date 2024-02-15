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
ext_buff <- function(rst, buffer = -25){
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
fires <- c("KNP")
sites <- c("KaweahMiddle")
sizes <- c(500)
outputs <- c("mass_burnt_pct",
             "surface_consumption",
             "canopy_consumption",
             "max_power",
             "residence_time_power",
             "residence_time_consumption",
             "max_reaction_rate")


first_fire <- T
for(fire in fires){
  mtbs <- rast(here(fire,
                    paste0(fire,"_dNBR.tif")))
  names(mtbs) <- "dNBR"
  first_site <- T
  for(site in sites){ #placeholder
    first_size <- T
    for(size in sizes){
      plot_bounds <- vect(here(fire,
                               "Sample_Sites",
                               site,
                               paste0(size,"m"),
                               paste0(site,"_bounds_",size,"m.shp")))
      mtbs_crop <- crop(mtbs, ext(plot_bounds))
      mtbs_pts <- as.points(mtbs_crop)
      mtbs_pol <- pts_to_pol(mtbs_pts)
      first_output <- T
      for(output in outputs){
        out_arr <- read.table(here("QF_runs",
                                   fire, 
                                   paste0(fire,"_",site,"_",size,"m"),
                                   "Arrays",
                                   paste0(output,".txt")))
        out_rst <- output_to_rst(output, out_arr, plot_bounds)
        out_crop <- crop(out_rst, ext_buff(out_rst))
        if(first_output){
          out_vect <- terra::extract(out_crop,
                                     mtbs_pol,
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
      out_vect$size = size
      if(first_size){
        size_vect <- out_vect
      } else{
        size_vect <- bind_rows(size_vect, out_vect)
      }
      first_size <- F
    }
    size_vect$site = site
    if(first_site){
      site_vect <- size_vect
    } else{
      site_vect <- bind_rows(site_vect,size_vect)
    }
    first_site <- F
  }
  site_vect$fire <- fire
  if(first_fire){
    fire_vect <- site_vect
  } else{
    fire_vect <- bind_rows(fire_vect, site_vect)
  }
  first_fire <- F
}

all_data <- as_tibble(fire_vect)
write.csv(all_data, here("all_data.csv"), row.names = F)
