########
## Threshold sensitivity analysis
library(here)
library(terra)
library(tidyverse)
library(tidyterra)
library(mgcv)

fire <- "Dixie"
bounds <- vect(here(fire,paste0(fire,"_Perimeter.shp")))
basins <- vect(here(fire,paste0(fire,"_basins_sensitivity_sbs.shp")))
centroid <- centroids(bounds)
centroid <- project(centroid, "EPSG:4326")
geom(centroid) # x and y used in NOAA Atlas 14 for 1-year 15min rainfall (multiply by 4)

dnbr <- rast(here(fire,paste0(fire,"_dNBR.tif")))
names(dnbr) <- "dNBR"
basins <- terra::extract(dnbr, basins, fun=mean, bind=T)

ggplot() +
  geom_point(data=basins, aes(severe_per, dNBR)) +
  geom_smooth(data=basins, 
              aes(severe_per, dNBR), 
              se=F) +
  theme_bw()

lm_dnbr <- gam(dNBR ~ s(severe_per, bs = "cs"), data=as.data.frame(basins))

lm_pred <- tibble(severe_per = c(10,11,12,13,14,15,16,17,18,19))
lm_pred$dNBR_pred <- predict(lm_dnbr, lm_pred, interval="none")
lm_pred

dixie_threshold <- 13
knp_threshold <- 5
caldor_threshold <- 16

threshold <- mean(c(dixie_threshold,knp_threshold,caldor_threshold))
