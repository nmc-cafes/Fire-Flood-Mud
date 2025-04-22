########
## Threshold sensitivity analysis
library(here)
library(terra)
library(tidyverse)
library(tidyterra)
library(mgcv)
library(sf)

layers <- st_layers(here("RUSLE_K_Factor_Google.kml"))
kml <- list()
for(i in 1:nrow(layers)){
  if(layers$geomtype[[i]] == "3D Polygon"){
    kml[[i]] <- vect(here("RUSLE_K_Factor_Google.kml"), layer=layers$name[i])
  }
}

kfact_raw <- vect(kml)

kfact <- kfact_raw %>%
  select(Name) %>%
  rename(KFactor = Name) %>%
  mutate(KFactor = as.numeric(KFactor))
kfact <- project(kfact, "EPSG:5070")

fires <- c("Caldor","Dixie","KNP")
kfact_df <- tibble(fire_name = fires, avg_kfact = rep(NA, length(fires)))
for(fire in fires){
  perim <- vect(here(fire,paste0(fire,"_Perimeter.shp")))
  fire_kfact <- terra::intersect(kfact, perim)
  fire_kfact$area_m2 <- expanse(fire_kfact, unit = "m")
  fire_kfact$weighted_value <- fire_kfact$area_m2 * fire_kfact$KFactor
  weighted_avg <- sum(fire_kfact$weighted_value, na.rm = TRUE) / sum(fire_kfact$area_m2, na.rm = TRUE)
  kfact_df[kfact_df$fire_name==fire,]$avg_kfact <- weighted_avg
}
kfact_df

caldor <- vect(here("Caldor","Caldor_Perimeter.shp"))
dixie <- vect(here("Dixie","Dixie_Perimeter.shp"))
knp <- vect(here("KNP","KNP_Perimeter.shp"))
cal_fires <- vect(svc(caldor,dixie,knp))

fires_kfact <- terra::intersect(kfact, cal_fires)
fires_kfact$area_m2 <- expanse(fires_kfact, unit = "m")
fires_kfact$weighted_value <- fires_kfact$area_m2 * fires_kfact$KFactor
weighted_avg <- sum(fires_kfact$weighted_value, na.rm = TRUE) / sum(fires_kfact$area_m2, na.rm = TRUE)

# from NOAA Atlas 14
caldor_rain <- c(8,6)
dixie_rain <- c(6,8,8,9,10,8,6,8)
knp_rain <- c(7,10,10)
avg_rain <- mean(c(caldor_rain, dixie_rain, knp_rain))

###
fires <- c("Caldor","Dixie","KNP","CedarCreek","CubCreek2")
basins_list <- list()
for(i in 1:length(fires)){
  bounds <- vect(here(fires[i],paste0(fires[i],"_Perimeter.shp")))
  basins <- vect(here(fires[i],paste0(fires[i],"_basins_sensitivity_sbs.shp")))
  # centroid <- centroids(bounds)
  # centroid <- project(centroid, "EPSG:4326")
  # geom(centroid) # x and y used in NOAA Atlas 14 for 1-year 15min rainfall (multiply by 4)
  
  dnbr <- rast(here(fires[i],paste0(fires[i],"_dNBR.tif")))
  names(dnbr) <- "dNBR"
  basins <- terra::extract(dnbr, basins, fun=mean, bind=T)
  basins_list[[i]] <- basins
}
basins <- do.call(rbind, basins_list)


ggplot() +
  geom_point(data=basins, aes(severe_per, dNBR)) +
  geom_smooth(data=basins, 
              aes(severe_per, dNBR), 
              se=F) +
  theme_bw()

# gam_dnbr <- gam(dNBR ~ s(severe_per, bs = "cs"), data=as.data.frame(basins))
lm_dnbr <- lm(dNBR ~ severe_per, data=as.data.frame(basins))
lm_pred <- tibble(severe_per = c(22,23,24,25,26))
lm_pred$dNBR_pred <- predict(lm_dnbr, lm_pred, interval="none")
lm_pred

### Use M1 Model spreadsheet
threshold <- 25
