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

perimeter <- vect(here("CubCreek2","CubCreek2_perimeter.shp"))
drainage <- vect(here("CubCreek2","CubCreek2_drainage_prox_200.shp"))
sites <- vect(here("CubCreek2","CubCreek2_sample_sites_NEW2.shp"))
severity <- rast(here("CubCreek2", "Severity_Class_510m.tif"))
severity <- project(severity, crs(perimeter))
all_streams <- vect(here("Streams_NorthAmerica","riv_pfaf_7_MERIT_Hydro_v07_Basins_v01_bugfix1.shp"))
all_streams <- project(all_streams, perimeter)
streams <- crop(all_streams, perimeter)
streams <- mask(streams, perimeter)

buff <- buffer(perimeter, -500)
drain_buff_mask <- terra::intersect(drainage, buff)
perimeter_masked <- erase(perimeter, drain_buff_mask)

severity_stack <- rast(here("CubCreek2","severity_stack.tif"))
homogeneity_stack <- rast(here("CubCreek2","homogeneity_stack.tif"))

to_vect <- function(x){
  x[x==0] <- NA
  y = as.polygons(x, aggregate=T)
  return(y)
} 

high_homo <- to_vect(severity_stack$high*homogeneity_stack$homogeneous)
high_hetero <- to_vect(severity_stack$high*homogeneity_stack$heterogeneous)
mod_homo <- to_vect(severity_stack$moderate*homogeneity_stack$homogeneous)
mod_hetero <- to_vect(severity_stack$moderate*homogeneity_stack$heterogeneous)
low_homo <- to_vect(severity_stack$low*homogeneity_stack$homogeneous)
low_hetero <- to_vect(severity_stack$low*homogeneity_stack$heterogeneous)
high_homo$severity <- "High"
high_hetero$severity <- "High"
mod_homo$severity <- "Moderate"
mod_hetero$severity <- "Moderate"
low_homo$severity <- "Low"
low_hetero$severity <- "Low"
high_homo$heterogeneity <- "Homogeneous"
high_hetero$heterogeneity <- "Heterogeneous"
mod_homo$heterogeneity <- "Homogeneous"
mod_hetero$heterogeneity <- "Heterogeneous"
low_homo$heterogeneity <- "Homogeneous"
low_hetero$heterogeneity <- "Heterogeneous"

sev_classes <- rbind(high_homo,high_hetero,mod_homo,mod_hetero,low_homo,low_hetero)
sev_classes <- sev_classes %>%
  mutate(severity = factor(severity, levels = c("High","Moderate","Low")),
         heterogeneity = factor(heterogeneity, levels = c("Heterogeneous","Homogeneous")))

orange <- colorblind_pal()(8)[2]
blue <- colorblind_pal()(8)[3]
yellow <- colorblind_pal()(8)[5]

my_colors <- c("purple4",orange, blue)

site_selection <- ggplot() +
  geom_spatvector(data = perimeter, color = "black", linewidth = 0.7, fill = "gray85") +
  geom_spatvector(data = sev_classes, aes(fill=severity,alpha=heterogeneity), color = NA) +
  scale_fill_manual(values = my_colors) +
  scale_alpha_discrete(range = c(0.7,1)) +
  geom_spatvector(data = perimeter_masked, color = "black", fill = "white") +
  geom_spatvector(data = sites, pch = 22, fill = NA, color = "black", size = 3) +
  labs(title = "Cub Creek 2 (WA)",
       fill = "Severity\nClass",
       alpha = "Heterogeneity\nClass") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = c(0.15,0.2),
        legend.background = element_rect(fill=NA)) + 
  guides(fill  = guide_legend(order = 1),
         alpha = guide_legend(order = 2))
site_selection

ggsave(here("Plots","site_selection_CubCreek.jpg"), site_selection, height = 18, width = 15.5, units = "cm")
