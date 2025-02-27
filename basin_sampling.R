library(here)
library(terra)
library(tidyterra)
library(tidyverse)
library(ggthemes)

"
New sampling procedure:
For each fire, use QGIS to delineate basins from a DEM. I used the GRASS tool r.watershed, with a
maximum size of watershed extent of 1000. Then, in R, find the area of the bounding box around all
basin polygons, and remove any that are smaller than 25 ha or larger than 100 ha. Next, find the
percent of each basin that is steeper than 23 degrees. To hold the amount of steep slopes relatively
constant, retain only basins between the 40-60th percentile of steep slope percent. Then, in those
remaining basins, find the percent of the area with both steep slopes and moderate-to-high fire
severity. Define a cutoff for that percent, and classify each basin into a binary group above or 
below that cutoff. Last, take a random sample of 20 basins, stratified by the severity percent class.
The resulting set of basins should a) be between 25-100 ha of simulation domain area, b) have 
relatively consistent percent area with steep slopes, and c) include an equal number of basins above
and below the chosen classification threshold for steep and severe slopes.
"
fires <- c("KNP","Caldor","CedarCreek","CubCreek2","Dixie")
for(fire in fires){
  cat(paste0("\n",fire,"\n"))
  cat("   Calculating basin areas")
  basins <- vect(here(fire,paste0(fire,"_Basins.shp")))
  basins$area <- expanse(basins, unit = "ha")
  
  basins <- basins %>%
    filter(area > 15,
           area < 100)
  
  # boxplot(basins$area)
  # summary(basins$area)
  nbasins <- nrow(basins)
  cat("\n  Calculating domain area for")
  for(i in 1:nbasins){
    if(i==1 | i%%100==0){
      cat(paste0("\n    Basin ", i, "/", nbasins))
    }
    bbox <- ext(basins[i])
    bbox_vect <- vect(bbox, crs=crs(basins))
    basins$area_ext[i] <- expanse(bbox_vect, unit="ha")
  }
  
  # boxplot(basins$area_ext)
  # summary(basins$area_ext)
  
  dem <- rast(here(fire,paste0(fire,"_DEM.tif")))
  slope <- terrain(dem, v="slope", unit = "degrees")
  
  basins_clean <- basins %>%
    filter(area_ext > 25,
           area_ext < 100)
  
  nbasins <- nrow(basins_clean)
  cat("\n  Calculating slope for")
  for(i in 1:nrow(basins_clean)){
    if(i==1 | i%%100==0){
      cat(paste0("\n    Basin ", i, "/", nbasins))
    }
    basin_slope <- mask(crop(slope, basins_clean[i]), basins_clean[i])
    basin_steep <- tibble("slope" = values(basin_slope, wide=F)) %>%
      filter(!is.na(slope)) %>%
      summarize(steep_percent = mean(slope>23)*100) %>%
      pull(steep_percent)
    basins_clean$steep_percent[i] <- basin_steep
  }
  
  # boxplot(basins_clean$steep_percent)
  # summary(basins_clean$steep_percent)
  # 
  # ggplot() +
  #   geom_spatvector(data=basins_clean, aes(fill = steep_percent)) +
  #   scale_fill_viridis_c() +
  #   theme_bw()
  
  basins_steep <- basins_clean %>%
    filter(steep_percent > 50)
  
  # ggplot() +
  #   geom_spatvector(data=basins_steep, aes(fill = steep_percent)) +
  #   scale_fill_viridis_c() +
  #   theme_bw()
  
  severity <- rast(here(fire,paste0(fire,"_SBS.tif")))
  severity <- project(severity, "EPSG:5070")
  names(severity) <- "severity"

  nbasins <- nrow(basins_steep)
  cat("\n  Calculating severity for")
  for(i in 1:nrow(basins_steep)){
    if(i==1 | i%%100==0){
      cat(paste0("\n    Basin ", i, "/", nbasins))
    }
    basin_severity <- mask(crop(severity, basins_steep[i]), basins_steep[i])
    basin_slope <- mask(crop(slope, basins_steep[i]), basins_steep[i])
    basin_slope_pt <- as.points(basin_slope) %>%
      filter(!is.na(slope))
    basin_slope_pt <- terra::extract(basin_severity, basin_slope_pt, bind=T)
    all_10m <- nrow(basin_slope_pt)
    basin_steep_severe <- basin_slope_pt %>%
      filter(slope > 23) %>%
      filter(severity%in%c(3,4))
    steep_severe_10m <- nrow(basin_steep_severe)
    steep_severe_percent <- (steep_severe_10m/all_10m)*100
    basins_steep$severe_percent[i] <- steep_severe_percent
  }
  
  # ggplot() +
  #   geom_spatvector(data=basins_steep, aes(fill = severe_percent)) +
  #   scale_fill_viridis_c() +
  #   theme_bw()
  
  writeVector(basins_steep, here(fire, paste0(fire,"_basins_sensitivity_sbs.shp")), overwrite=T)
}

## Determine severity cutoff using threshold_sensitivity_analysis.R

severity_cutoff = 11
for(fire in fires){
  basins_steep <- vect(here(fire, paste0(fire,"_basins_sensitivity_sbs.shp")))
  basins_final <- basins_steep %>%
    mutate(severe = if_else(severe_per > severity_cutoff, 1, 0)) %>%
    mutate(severe = factor(severe))

  basins_sampled <- basins_final %>% slice_sample(n=10, by=severe)
  writeVector(basins_sampled, here(fire, paste0(fire,"_sample_basins_sbs.shp")), overwrite=T)
}

##############
fire <- "CedarCreek"
basins_sampled <- vect(here(fire,paste0(fire,"_sample_basins_sbs.shp")))

ggplot() +
  geom_spatvector(data=basins_sampled, aes(fill = severe)) +
  scale_fill_colorblind() +
  theme_bw()

summary(basins_sampled$area)

basins_df <- as.data.frame(basins_sampled)
basins_df %>%
  mutate(Basin = factor(DN)) %>%
  pivot_longer(area_ext:severe_per,
               names_to = "var",
               values_to = "val") %>%
  ggplot() +
  geom_bar(stat = "identity",
           aes(x=Basin,val)) +
  coord_flip() +
  facet_wrap(.~var, nrow=1, scales = "free_x") +
  theme_bw()

basins_df %>%
  ggplot() +
  geom_point(aes(steep_perc,severe_per, size=area_ext)) +
  theme_bw()

bounds <- vect(here(fire,paste0(fire,"_Perimeter.shp")))
ggplot() +
  geom_spatvector(data=bounds, fill=NA) +
  geom_spatvector(data=basins_sampled,
                  aes(fill=severe_per)) +
  scale_fill_gradient2() +
  theme_bw()



