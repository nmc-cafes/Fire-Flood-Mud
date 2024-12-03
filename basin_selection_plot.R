library(terra)
library(here)
library(tidyverse)
library(tidyterra)

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

perimeter <- vect(here("KNP","KNP_perimeter.shp"))
drainage <- vect(here("KNP","KNP_drainage_prox_200.shp"))
sites <- vect(here("KNP","KNP_sample_basins.shp"))
slope <- slope_mask("KNP")
slope_vect <- as.polygons(slope)
severity <- rast(here("KNP","KNP_Severity.tif")) %>%
  filter(KNP_Severity %in% c(1,2,3,4,5)) %>%
  mutate(KNP_Severity = factor(KNP_Severity, labels = c("Unburned", "Low Severity",
                                                        "Moderate Severity", "High Severity",
                                                        "Increased Greenness")))


mtbs_colors <-c("#006400","#7fffd4","#ffff00","#ff0000","#7fff00")

basin_selection <- ggplot() +
  geom_spatraster(data = severity) +
  geom_spatvector(data = perimeter, fill=NA) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  geom_spatvector(data = slope_vect, fill="white", color=NA) +
  geom_spatvector(data = sites, fill=NA, color="black", linewidth=0.5) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(0.75,0.835),
        legend.key = element_rect(color="black"))
basin_selection
ggsave("basin_selection_fig.png", basin_selection, path = here("Plots"), width = 10, height = 13.5, units = "cm")
