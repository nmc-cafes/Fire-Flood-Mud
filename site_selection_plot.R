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
drainage <- vect(here("CedarCreek","CedarCreek_drainage_prox_200-800.shp"))
sites <- vect(here("CedarCreek","CedarCreek_sample_sites_NJT.shp"))
# all_streams <- vect(here("Streams_NorthAmerica","riv_pfaf_7_MERIT_Hydro_v07_Basins_v01_bugfix1.shp"))
# all_streams <- project(all_streams, perimeter)
streams <- crop(all_streams, perimeter)
streams <- mask(streams, perimeter)

buff <- buffer(perimeter, -500)
drain_buff_mask <- terra::intersect(drainage, buff)
drain_buff_inverse <- erase(drainage, buff)

steep <- slope_mask("CedarCreek")
steep <- mask(steep, buff)
steep_mask <- mask(steep, drain_buff_mask)
steep_mask <- steep_mask %>% mutate(slope = as.numeric(slope))
steep_mask[steep_mask==1] <- 100

orange <- colorblind_pal()(8)[2]

site_selection <- ggplot() +
  geom_spatvector(data = perimeter, color = "black", linewidth = 0.7, fill = NA) +
  geom_spatvector(data = buff, color = "black", fill = "gray95") +
  geom_spatraster(data = steep) +
  geom_spatvector(data = streams, color = "blue") +
  geom_spatvector(data = drain_buff_mask, color = "skyblue2", linetype = "dashed", linewidth = 0.7, fill = "skyblue") +
  geom_spatvector(data = drain_buff_inverse, color = "skyblue2", linetype = "dashed", linewidth = 0.7, fill = NA) +
  geom_spatraster(data = steep_mask) +
  scale_fill_gradient(low="gray80", high = "skyblue4", na.value="transparent") +
  geom_spatvector(data = sites, pch = 22, fill = orange, color = "black", size = 3) +
  labs(title = "Cedar Creek (WA)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank())


ggsave(here("Plots","site_selection.jpg"), site_selection, height = 18, width = 15.5, units = "cm")
