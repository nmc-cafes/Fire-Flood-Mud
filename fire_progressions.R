library(here)
library(terra)
library(tidyverse)
library(tidyterra)

fire_names <- c("CedarCreek","CubCreek2","Dixie","Caldor","KNP")
caps_names <- c("CEDAR_CREEK","CUB_CREEK_2","DIXIE","CALDOR","KNP_COMPLEX")
dfs <- list()

for(i in 1:length(fire_names)){
  prog <- vect(here(fire_names[i],
                    "Fire_Progression", 
                    paste0(caps_names[i], "_2021_PROGRESSION_CORRECTED.shp")))
  
  sites <- vect(here(fire_names[i],
                     paste0(fire_names[i],"_sample_sites_NJT.shp")))
  
  prog <- project(prog,sites)
  df <- data.frame("Fire_Name" = rep(NA,nrow(sites)),
                   "Site_Name" = rep(NA,nrow(sites)),
                   "Fire_Date" = rep(NA,nrow(sites)))
  
  for(j in 1:nrow(sites)){
    all_dates <- terra::extract(prog,sites[j,])
    all_dates$DateCurren <- as.Date(all_dates$DateCurren)
    burn_day <- all_dates[all_dates$DateCurren == min(all_dates$DateCurren),]$DateCurren
    df$Fire_Name[j] <- fire_names[i]
    df$Site_Name[j] <- sites[j,]$Site_Name
    df$Fire_Date[j] <- as.character(burn_day[1])
  }
  dfs[[i]] <- df
}
sample_sites <- bind_rows(dfs)

