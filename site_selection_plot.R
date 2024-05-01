library(here)
library(terra)
library(tidyverse)
library(tidyterra)
library(ggthemes)

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

perimeter <- vect(here("CedarCreek","CedarCreek_perimeter.shp"))
drainage <- vect(here("CedarCreek","CedarCreek_drainage_prox_200.shp"))
sites <- vect(here("CedarCreek","CedarCreek_sample_sites_NEW.shp"))
severity <- rast(here("CedarCreek", "Severity_Class_510m.tif"))
severity <- project(severity, crs(perimeter))
all_streams <- vect(here("Streams_NorthAmerica","riv_pfaf_7_MERIT_Hydro_v07_Basins_v01_bugfix1.shp"))
all_streams <- project(all_streams, perimeter)
streams <- crop(all_streams, perimeter)
streams <- mask(streams, perimeter)

buff <- buffer(perimeter, -500)
drain_buff_mask <- terra::intersect(drainage, buff)
drain_buff_inverse <- erase(drainage, buff)

steep <- slope_mask("CedarCreek")
steep <- mask(steep, buff)
# steep_mask <- mask(steep, drain_buff_mask)
steep_mask <- steep_mask %>% mutate(slope = as.numeric(slope))
# steep_mask[steep_mask==1] <- 100

steep_mask_30m <- project(steep_mask, severity, method = "near")
severity_in <- mask(severity,steep_mask_30m)
low_in <- aggregate(as.polygons(severity_in$low),dissolve=T)
mod_in <- aggregate(as.polygons(severity_in$moderate),dissolve=T)
high_in <- aggregate(as.polygons(severity_in$high),dissolve=T)
severity_out <- mask(severity,steep_mask_30m, inverse=T)
severity_out <- mask(severity_out, buff)
low_out <- aggregate(as.polygons(severity_out$low),dissolve=T)
mod_out <- aggregate(as.polygons(severity_out$moderate),dissolve=T)
high_out <- aggregate(as.polygons(severity_out$high),dissolve=T)

orange <- colorblind_pal()(8)[2]
blue <- colorblind_pal()(8)[3]
yellow <- colorblind_pal()(8)[5]

site_selection <- ggplot() +
  geom_spatvector(data = perimeter, color = "black", linewidth = 0.7, fill = NA) +
  geom_spatvector(data = buff, color = "black", fill = NA) +
  geom_spatvector(data = low_in, color = NA, fill=blue) +
  geom_spatvector(data = mod_in, color = NA, fill=orange) +
  geom_spatvector(data = high_in, color = NA, fill="purple4") +
  geom_spatvector(data = low_out, color = NA, fill=blue, alpha = 0.25) +
  geom_spatvector(data = mod_out, color = NA, fill=orange, alpha = 0.25) +
  geom_spatvector(data = high_out, color = NA, fill="purple4", alpha = 0.25) +
  geom_spatvector(data = streams, color = "blue") +
  geom_spatvector(data = sites, pch = 22, fill = yellow, color = "black", size = 3) +
  labs(title = "Cedar Creek (WA)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())
site_selection

ggsave(here("Plots","site_selection_NEW.jpg"), site_selection, height = 18, width = 15.5, units = "cm")
