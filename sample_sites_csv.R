library(here)
library(tidyverse)
library(sf)

fires <- c("Caldor","Dixie","KNP","CedarCreek","Muckamuck")
states <- data.frame("state"=c(rep("CA",3),rep("WA",2)),
                     "fire"=fires)
fire_list <- list()
for(i in 1:length(fires)){
  sites <- read_sf(here(fires[i],paste0(fires[i],"_sample_sites_NJT.shp")))
  sites$Fire_Name <- fires[i]
  sites$Fire_State <- states[states$fire==fires[i],]$state
  fire_list[[i]] <- sites
}

sample_sites <- bind_rows(fire_list) %>%
  select(-FID)

sample_sites <- sample_sites[!st_is_empty(sample_sites),,drop=F]

sample_sites_df <- sample_sites %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(Fire_State,Fire_Name,Site_Name,Fire_Date,X,Y)

write.csv(sample_sites_df, here("Sample_Sites_NJT.csv"), row.names=F)
