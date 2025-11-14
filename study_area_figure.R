library(here)
library(tidyverse)
library(terra)
library(tidyterra)
library(ggthemes)
library(patchwork)
library(ggspatial)

read_severity <- function(path){
  rst <- rast(path)
  rst[rst==0] <- NA
  rst[rst==6] <- NA
  og_name <- names(rst)
  names(rst) <- "severity"
  rst <- rst %>%
    mutate(severity = factor(severity,
                             labels = c(
                               "Unburned",
                               "Low Severity",
                               "Moderate Severity",
                               "High Severity",
                               "Increased Greenness"
                             )))
  names(rst) <- og_name
  return(rst)
}

read_SBS <- function(path){
  rst <- rast(path)
  rst <- project(rst, "EPSG:5070", method="near")
  rst[rst==0] <- NA
  rst[rst==6] <- NA
  rst[rst==15] <- NA
  og_name <- names(rst)
  names(rst) <- "SBS"
  rst <- rst %>%
    mutate(SBS = factor(SBS,
                             labels = c(
                               "Unburned",
                               "Low Severity",
                               "Moderate Severity",
                               "High Severity"
                             )))
  names(rst) <- og_name
  return(rst)
}

states <- vect(here("cb_2018_us_state_20m","cb_2018_us_state_20m.shp"))
conus <- states %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico"))
conus <- project(conus, "EPSG:5070")
firestates <- conus %>% filter(NAME %in% c("California","Washington"))

west_ext <- ext(conus)
west_ext[2] <- ((ext(conus)[2]-ext(conus)[1])*0.2) + ext(conus)[1]
west_ext[3] <- ((ext(conus)[4]-ext(conus)[3])*0.5) + ext(conus)[3]
west_ext[4] <- ext(conus)[4] - ((ext(conus)[4]-ext(conus)[3])*0.02)

knp <- vect(here("KNP","KNP_perimeter.shp"))
caldor <- vect(here("Caldor","Caldor_perimeter.shp"))
dixie <- vect(here("Dixie","Dixie_perimeter.shp"))
cedar <- vect(here("CedarCreek","CedarCreek_perimeter.shp"))
cub <- vect(here("CubCreek2","CubCreek2_perimeter.shp"))
fires <- vect(c(knp,caldor,dixie,cedar,cub))

#inset map
inset <- ggplot() +
  geom_spatvector(data = conus, color="gray50", fill = NA) +
  geom_spatvector(data = firestates, color = "black", fill=NA) +
  geom_spatvector(data = fires, aes(fill = Incid_Name), color=NA) +
  scale_x_continuous(limits = c(west_ext[1], west_ext[2])) +
  scale_y_continuous(limits = c(west_ext[3], west_ext[4])) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(legend.position="none")

# knp_sev <- read_severity(here("KNP","KNP_severity.tif"))
# caldor_sev <- read_severity(here("Caldor","Caldor_severity.tif"))
# dixie_sev <- read_severity(here("Dixie","Dixie_severity.tif"))
# cedar_sev <- read_severity(here("CedarCreek","CedarCreek_severity.tif"))
# cub_sev <- read_severity(here("CubCreek2","CubCreek2_severity.tif"))
knp_sev <- read_SBS(here("KNP","KNP_SBS.tif"))
caldor_sev <- read_SBS(here("Caldor","Caldor_SBS.tif"))
dixie_sev <- read_SBS(here("Dixie","Dixie_SBS.tif"))
cedar_sev <- read_SBS(here("CedarCreek","CedarCreek_SBS.tif"))
cub_sev <- read_SBS(here("CubCreek2","CubCreek2_SBS.tif"))

mtbs_colors <-c("#006400","#7fffd4","#ffff00","#ff0000","#7fff00")

knp_plot <- ggplot() + 
  geom_spatraster(data=knp_sev) + 
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

caldor_plot <- ggplot() + 
  geom_spatraster(data=caldor_sev) + 
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

dixie_plot <- ggplot() + 
  geom_spatraster(data=dixie_sev) + 
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

cedar_plot <- ggplot() + 
  geom_spatraster(data=cedar_sev) + 
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

cub_plot <- ggplot() + 
  geom_spatraster(data=cub_sev) + 
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave("allfires.png", inset, path = here("Plots","study_area_fig"), height = 5.5, width = 4)
# ggsave("knp.png", knp_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
# ggsave("caldor.png", caldor_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
# ggsave("dixie.png", dixie_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
# ggsave("cedar.png", cedar_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
# ggsave("cub.png", cub_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
ggsave("knp_SBS.png", knp_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
ggsave("caldor_SBS.png", caldor_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
ggsave("dixie_SBS.png", dixie_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
ggsave("cedar_SBS.png", cedar_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
ggsave("cub_SBS.png", cub_plot, path = here("Plots","study_area_fig"), height = 4, width = 4)
