library(here)
library(tidyverse)
library(terra)
library(sf)

fires <- c("Caldor","Dixie","KNP","CedarCreek","CubCreek2")
states <- data.frame("state"=c(rep("CA",3),rep("WA",2)),
                     "fire"=fires)
fire_list <- list()
for(i in 1:length(fires)){
  sites <- read_sf(here("Fire_Data",fires[i],paste0(fires[i],"_sample_basins_sbs.shp")))
  sites <- st_centroid(sites) 
  sites$Fire_Name <- fires[i]
  sites$Fire_State <- states[states$fire==fires[i],]$state
  sites$site_name <- NA
  for(j in 1:nrow(sites)){
    sites$site_name[j] <- paste0(substr(fires[i], start = 1, stop = 3),j)
  }
  fire_list[[i]] <- sites
}

sample_sites <- bind_rows(fire_list)

sample_sites <- sample_sites[!st_is_empty(sample_sites),,drop=F]

sample_sites_df <- sample_sites %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  rename(Site_Name = site_name) %>%
  select(Fire_State,Fire_Name,Site_Name,X,Y)

write.csv(sample_sites_df, here("Sample_Basins_SBS.csv"), row.names=F)
