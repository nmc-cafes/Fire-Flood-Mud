library(here)
library(tidyverse)
library(terra)
library(sf)

fires <- c("Caldor","Dixie","KNP","CedarCreek","CubCreek2")
states <- data.frame("state"=c(rep("CA",3),rep("WA",2)),
                     "fire"=fires)
fire_list <- list()
for(i in 1:length(fires)){
  sites <- read_sf(here(fires[i],paste0(fires[i],"_sample_sites_NEW.shp")))
  sites$Fire_Name <- fires[i]
  sites$Fire_State <- states[states$fire==fires[i],]$state
  fire_list[[i]] <- sites
}

sample_sites <- bind_rows(fire_list)

sample_sites <- sample_sites[!st_is_empty(sample_sites),,drop=F]

sample_sites_df <- sample_sites %>%
  mutate(X = st_coordinates(.)[,1],
         Y = st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  rename(Site_Name = site_name,
         Severity_Class = severity) %>%
  select(Fire_State,Fire_Name,Site_Name,Severity_Class,X,Y)

caps_names <- c("CALDOR","DIXIE","KNP_COMPLEX","CEDAR_CREEK","CUB_CREEK_2")
dfs <- list()

for(i in 1:length(fires)){
  prog <- vect(here(fires[i],
                    "Fire_Progression", 
                    paste0(caps_names[i], "_2021_PROGRESSION_CORRECTED.shp")))
  
  sites <- vect(here(fires[i],
                     paste0(fires[i],"_sample_sites_NEW.shp")))
  
  prog <- project(prog,sites)
  df <- data.frame("Fire_Name" = rep(NA,nrow(sites)),
                   "Site_Name" = rep(NA,nrow(sites)),
                   "Severity_Class" = rep(NA,nrow(sites)),
                   "Fire_Date" = rep(NA,nrow(sites)))
  
  for(j in 1:nrow(sites)){
    all_dates <- terra::extract(prog,sites[j,])
    all_dates$DateCurren <- as.Date(all_dates$DateCurren)
    burn_day <- all_dates[all_dates$DateCurren == min(all_dates$DateCurren),]$DateCurren
    df$Fire_Name[j] <- fires[i]
    df$Site_Name[j] <- sites[j,]$site_name
    df$Severity_Class[j] <- sites[j,]$severity
    df$Fire_Date[j] <- as.character(burn_day[1])
  }
  dfs[[i]] <- df
}
fire_dates <- bind_rows(dfs)

final_df <- left_join(sample_sites_df,fire_dates,by=c("Fire_Name","Site_Name","Severity_Class"))

write.csv(final_df, here("Sample_Sites_NEW.csv"), row.names=F)
