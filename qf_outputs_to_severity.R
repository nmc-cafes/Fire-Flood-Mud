"
Read in quicfire outputs as rasters and match them to MTBS severity
"

library(here)
library(terra)
library(tidyverse)
library(tidyterra)

ext_buff <- function(rst, buffer = -25){
  new_ext <- ext(rst)
  new_ext[1] <- new_ext[1] - buffer
  new_ext[2] <- new_ext[2] + buffer
  new_ext[3] <- new_ext[3] - buffer
  new_ext[4] <- new_ext[4] + buffer
  return(new_ext)
}

surf_consump <- read.table(here("QF_runs/KNP/KNP_KaweahMiddle_500m/Arrays/surface_consumption.txt"))
surf_consump_mat <- as.matrix(surf_consump)

plot_bounds <- vect(here("KNP/Sample_Sites/KaweahMiddle/500m/KaweahMiddle_bounds_500m.shp"))

surf_consump_rst <- rast(nrow = dim(surf_consump_mat)[1],
                         ncol = dim(surf_consump_mat)[2],
                         crs = crs(plot_bounds),
                         extent = ext(plot_bounds),
                         vals = surf_consump_mat,
                         names = "surface_consumption")
surf_consump_rst <- flip(surf_consump_rst, direction="vertical")
surf_consump_crop <- crop(surf_consump_rst, ext_buff(surf_consump_rst))
surf_consump_agg <- aggregate(surf_consump_crop, fact = 30/res(surf_consump_crop)[1], fun=mean, na.rm=T)
surf_consump_pts <- as.points(surf_consump_agg)

mtbs <- rast(here("KNP","KNP_dNBR.tif"))
mtbs_crop <- crop(mtbs, ext(surf_consump_rst))

dat_vect <- terra::extract(mtbs_crop, surf_consump_pts, xy=T, ID=F, bind=T)
dat_df <- as_tibble(dat_vect)

ggplot() +
  geom_point(data = dat_df, aes(surface_consumption, KNP_dNBR)) + 
  theme_bw()
