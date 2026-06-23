library(here)
library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(geojson)
library(geojsonsf)

dixie <- vect(here("Dixie","Dixie_sample_basins.shp"))
dixie_df <- as.data.frame(dixie)
dixie_df <- tibble(dixie_df)

dix9 <- dixie[9]
writeVector(dix9, here("Dixie","Sample_Basins","Dix9","Dix9_basin.shp"))


aoi <- st_as_sf(dix9)
aoi <- st_transform(aoi, 3857)
aoi_json <- sf_geojson(aoi, simplify = FALSE)
geo_write(aoi_json, file = here("Dixie","Sample_Basins","Dix9","Dix9_basin.geojson"))



