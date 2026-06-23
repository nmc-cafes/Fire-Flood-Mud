library(here)
library(ggstar)
library(tidyverse)
library(terra)
library(tidyterra)
library(ggthemes)
library(patchwork)
library(ggspatial)
library(basemaps)
library(maptiles)
library(ggnewscale)

map_token <- "V2tHpgnaN9td5HYNOV9C"

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

read_ig <- function(path, dixie=F){
  vct <- vect(path)
  vct <- project(vct, "EPSG:5070")
  vct$DateCurren <- as.Date(vct$DateCurren)
  if(dixie){
    dix_ig_pols <- vct[vct$DateCurren=="2021-07-17",]
    ig_pols <- dix_ig_pols[2,]
  } else{
    ig_pols <- vct[vct$DateCurren==min(vct$DateCurren),]
  }
  ig <- centroids(ig_pols)
  ig_df <- as.data.frame(ig, geom="XY")
  return(ig_df)
}

states <- vect(here("cb_2018_us_state_20m","cb_2018_us_state_20m.shp"))
conus <- states %>% filter(!NAME %in% c("Alaska","Hawaii","Puerto Rico"))
conus <- project(conus, "EPSG:3857")
firestates <- conus %>% filter(NAME %in% c("California","Washington"))

west_ext <- ext(conus)
west_ext[1] <- ext(conus)[1] - ((ext(conus)[2]-ext(conus)[1])*0.005)
west_ext[2] <- ((ext(conus)[2]-ext(conus)[1])*0.125) + ext(conus)[1]
west_ext[3] <- ((ext(conus)[4]-ext(conus)[3])*0.4) + ext(conus)[3]
west_ext[4] <- ext(conus)[4] - ((ext(conus)[4]-ext(conus)[3])*0.005)
west_ext_vect <- vect(west_ext, crs=crs(conus))
plot(conus)
plot(west_ext_vect, add=T)

knp <- vect(here("KNP","KNP_perimeter.shp"))
caldor <- vect(here("Caldor","Caldor_perimeter.shp"))
dixie <- vect(here("Dixie","Dixie_perimeter.shp"))
cedar <- vect(here("CedarCreek","CedarCreek_perimeter.shp"))
cub <- vect(here("CubCreek2","CubCreek2_perimeter.shp"))
fires <- vect(c(knp,caldor,dixie,cedar,cub))
fires <- project(fires, "EPSG:3857")

set_defaults(map_service = "maptiler", map_type = "aquarelle", map_token = map_token, map_res=1)

#inset map
inset <- ggplot() +
  basemap_gglayer(west_ext_vect) +
  scale_fill_identity() +
  new_scale_fill() +
  geom_spatvector(data = fires, aes(fill = Incid_Name), color="black") +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_colorblind() +
  theme_bw() +
  theme(legend.position="none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
inset
ggsave("allfires2.png", inset, path = here("Plots","study_area_fig"), height = 10, width = 5)

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

knp_ig <- read_ig(here("KNP","Fire_Progression","KNP_COMPLEX_2021_PROGRESSION_CORRECTED.shp"))
caldor_ig <- read_ig(here("Caldor","Fire_Progression","CALDOR_2021_PROGRESSION_CORRECTED.shp"))
dixie_ig <- read_ig(here("Dixie","Fire_Progression","DIXIE_2021_PROGRESSION_CORRECTED.shp"), dixie=T)
cedar_ig <- read_ig(here("CedarCreek","Fire_Progression","CEDAR_CREEK_2021_PROGRESSION_CORRECTED.shp"))
cub_ig <- read_ig(here("CubCreek2","Fire_Progression","CUB_CREEK_2_2021_PROGRESSION_CORRECTED.shp"))

mtbs_colors <-c("#006400","#7fffd4","#ffff00","#ff0000","#7fff00")

knp_plot <- ggplot() +
  geom_spatraster(data=knp_sev) +
  geom_star(data=knp_ig,
            aes(x=x,y=y),
            starshape=3,
            fill="black",
            color="black",
            size=3) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

caldor_plot <- ggplot() + 
  geom_spatraster(data=caldor_sev) +
  geom_star(data=caldor_ig,
            aes(x=x,y=y),
            starshape=3,
            fill="black",
            color="black",
            size=3) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

dixie_plot <- ggplot() + 
  geom_spatraster(data=dixie_sev) + 
  geom_star(data=dixie_ig,
            aes(x=x,y=y),
            starshape=3,
            fill="black",
            color="black",
            size=3) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

cedar_plot <- ggplot() + 
  geom_spatraster(data=cedar_sev) +
  geom_star(data=cedar_ig,
            aes(x=x,y=y),
            starshape=3,
            fill="black",
            color="black",
            size=3) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

cub_plot <- ggplot() + 
  geom_spatraster(data=cub_sev) +
  geom_star(data=cub_ig,
            aes(x=x,y=y),
            starshape=3,
            fill="black",
            color="black",
            size=3) +
  scale_fill_manual(values = mtbs_colors, na.value = NA) +
  annotation_scale() +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.position = "none")

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
