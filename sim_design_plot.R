library(here)
library(terra)
library(tidyterra)

dem <- rast(here("Caldor","Caldor_DEM.tif"))
dem <- project(dem, "EPSG:5070")

site <- vect(here("Caldor","Sample_Sites","Caldor5","Caldor5_bounds_500m.shp"))
site_dem <- crop(dem,site)
plot(site_dem)
contour(site_dem, add=T)
site_contour <- as.contour(site_dem)

ggplot() +
  geom_spatraster(data=site_dem) +
  geom_spatvector(data=site_contour, color = "gray60") +
  scale_fill_terrain_c(direction=-1) +
  theme_bw()
