library(terra)
library(here)
library(tidyverse)
library(tidyterra)
library(ggspatial)
library(maptiles)

slope_mask <- function(fire_name){
  DEM <- rast(here(fire_name,paste0(fire_name,"_DEM.tif")))
  slope <- terrain(DEM, v="slope", neighbors=8, unit="degrees")
  rcl_mat <- matrix(c(0,23,1,
                      23, Inf, NA),
                    nrow = 2,
                    byrow =  T)
  slope_rcl <- classify(slope, rcl_mat, right = F)
  return(slope_rcl)
}

crop_severity <- function(rst, x_min, y_min, x_max, y_max){
  x_add = (ext(rst)[2] - ext(rst)[1])*x_min
  y_add = (ext(rst)[4] - ext(rst)[3])*y_min
  x_sub = (ext(rst)[2] - ext(rst)[1])*x_max
  y_sub = (ext(rst)[4] - ext(rst)[3])*y_max
  new_ext <- ext(rst)
  new_ext[1] <- ext(rst)[1] + x_add
  new_ext[2] <- ext(rst)[2] - x_sub
  new_ext[3] <- ext(rst)[3] + y_add
  new_ext[4] <- ext(rst)[4] - y_sub
  bbox <- vect(new_ext)
  rst_cropped <- crop(rst, bbox)
  return(rst_cropped)
}

perimeter <- vect(here("KNP","KNP_perimeter.shp"))
drainage <- vect(here("KNP","KNP_drainage_prox_200.shp"))
sites <- vect(here("KNP","KNP_sample_basins_sbs.shp"))
slope <- slope_mask("KNP")
slope_vect <- as.polygons(slope)
severity <- rast(here("KNP", "KNP_SBS.tif"))
severity <- project(severity, "EPSG:5070", method="near")
severity <- severity %>%
  filter(KNP_SBS %in% c(1,2,3,4)) %>%
  mutate(KNP_SBS = factor(KNP_SBS, labels = c("Unburned", "Low Severity",
                                                        "Moderate Severity", "High Severity")))

severity_cropped <- crop_severity(severity, 0.125, 0.025, 0, 0)


mtbs_colors <-c("#006400","#7fffd4","#ffff00","#ff0000")

sites <- sites %>% mutate(severe = factor(severe, levels = c(0,1), labels = c("Low Hazard","High Hazard")))


openmap11 <- get_tiles(vect(ext(severity_cropped),crs=crs(severity_cropped)),
                       provider = "OpenTopoMap", zoom=11, crop = TRUE)
esrimap11 <- get_tiles(vect(ext(severity_cropped),crs=crs(severity_cropped)), 
                       provider = "Esri.NatGeoWorldMap", zoom=11, crop = TRUE)


basin_selection <- ggplot() +
  geom_spatraster_rgb(data = esrimap11) +
  geom_spatraster(data = severity_cropped) +
  geom_spatvector(data = perimeter, fill=NA) +
  # scale_fill_manual(values = mtbs_colors, na.value = NA) +
  geom_spatvector(data = slope_vect, fill="white", color=NA) +
  geom_spatvector(data = sites, fill=NA, aes(color=severe), linewidth=0.5) +
  scale_color_manual(values = c("blue", "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_scale() +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.82,0.83),
        axis.text = element_blank(),
        axis.ticks = element_blank())
basin_selection
ggsave("basin_selection_fig_basemap.png", basin_selection, path = here("Plots"), width = 12, height = 16, units = "cm")


zoom <- function(vct, rst, size, x_off, y_off){
  r <- size/2
  c <- centroids(vct)
  x_min <- ext(c)[1] + x_off - r
  x_max <- ext(c)[1] + x_off + r
  y_min <- ext(c)[3] + y_off - r
  y_max <- ext(c)[3] + y_off + r
  extent <- ext(c(x_min,x_max,y_min,y_max))
  bbox <- vect(extent)
  rst_crop <- crop(rst, bbox)
  return(rst_crop)
}

sev_zoomed <- zoom(perimeter, severity, 10500, -1150, -3000)
slope_zoomed <- zoom(perimeter, slope_vect, 10500, -1150, -3000)
perim_zoomed <- zoom(perimeter, perimeter, 10500, -1150, -3000)
sites_zoomed <- crop(sites, ext(sev_zoomed))

basin_zoom <- ggplot() +
  geom_spatraster(data = sev_zoomed) +
  geom_spatvector(data = perim_zoomed, fill=NA) +
  geom_spatvector(data = slope_zoomed, fill="white", color=NA) +
  geom_spatvector(data = sites_zoomed, fill=NA, aes(color=severe), linewidth=0.75) +
  scale_color_manual(values = c("blue", "black")) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  annotation_scale() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.ticks = element_blank())
basin_zoom
ggsave("basin_selection_zoom.png", basin_zoom, path = here("Plots"), width = 17, height = 17, units = "cm")
