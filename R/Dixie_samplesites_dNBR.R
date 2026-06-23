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

#### ASSEMBLE DATASET ####
site_df <- read.csv(here("Sample_Sites_NEW2.csv"))

mtbs <- rast(here("Fire_Data","Dixie","Dixie_dNBR.tif"))
mtbs[mtbs==-32768] <- NA
mtbs <- disagg(mtbs, fact=15)
names(mtbs) <- "dNBR"

severity <- rast(here("Fire_Data","Dixie", "Dixie_Severity.tif"))
severity <- disagg(severity, fact=15)
names(severity) <- "severity"

dem <- rast(here("Fire_Data","Dixie", "Dixie_DEM.tif"))
dem <- project(dem, "EPSG:5070", res = c(2,2), method="bilinear")
names(dem) <- "DEM"

sites <- list.dirs(here("Arrays","Dixie"), full.names = F, recursive = F)
for(i in 1:length(sites)){
  sites[i] <- str_split(sites[i], "_")[[1]][2]
}
for(site in sites){
  cat(site,"\n")
  plot_bounds <- vect(here("Fire_Data",
                           "Dixie",
                           "Sample_Sites",
                           site,
                           paste0(site,"_bounds_500m.shp")))
  mtbs_crop <- crop(mtbs, ext(plot_bounds))
  sev_crop <- crop(severity, ext(plot_bounds))
  dem_crop <- crop(dem, ext(plot_bounds))
  ext(dem_crop) <- ext(sev_crop)
  stack <- c(mtbs_crop, sev_crop, dem_crop)
  writeRaster(stack, here("Fire_Data","Dixie","severity_data_for_Alex",paste0(site,".tif")))
}
