library(tidyverse)
library(terra)
library(here)
library(sf)

fire_name <- "Dixie"
sample_sites <- st_read(here("Fire_Data",fire_name,paste0(fire_name,"_sample_sites_NJT.shp")))
dnbr <- rast(here("Fire_Data",fire_name,paste0(fire_name,"_dNBR.tif")))

dnbr_pts <- st_as_sf(as.points(dnbr))

sample_sites_dnbr <- sample_sites
for(i in nrow(sample_sites)){
  near_idx <- st_nearest_feature(sample_sites_dnbr[i,], dnbr_pts)
  nearest <- dnbr_pts[near_idx,]
  sample_sites_dnbr <- sample_sites_dnbr %>%
    mutate(X = st_coordinates(.)[,1],
           Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  nearest <- nearest %>%
    mutate(X = st_coordinates(.)[,1],
           Y = st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  sample_sites_dnbr$X[i] <- nearest$X[i]
  sample_sites_dnbr$Y[i] <- nearest$Y[i]
}

sample_sites_dnbr <- st_as_sf(sample_sites_dnbr, coords = c("X","Y"))
sample_sites_dnbr %>%
  st_set_crs(crs(sample_sites))

ggplot() +
  geom_sf(data = sample_sites[1,], color = "blue") +
  geom_sf(data = sample_sites_dnbr[1,], color = "red") +
  theme_bw()
