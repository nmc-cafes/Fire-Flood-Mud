library(tidyverse)
library(skimr)
library(sf)

setwd("../Fire-Flood-Mud/KNP")
demog <- read.csv("allplotarchiveout.csv")
# skim(demog)

# unique(demog$PLOT)
CCR <- demog %>% 
  filter(PLOT=="CCRPIPO") %>%
  select(TAGNUMBER,SppCode,DBH6,YEAR6,XCoord,YCoord,DBHMort) %>%
  mutate(DBH = if_else(is.na(DBH6), DBHMort, DBH6))

CCR_sf <- st_as_sf(CCR, coords = c("XCoord","YCoord"), crs = 26911)
write_sf(CCR_sf, "CCRPIPO.shp")

# remove spatial outlier
CCR <- CCR %>% filter(!XCoord==max(XCoord))
CCR_sf <- st_as_sf(CCR, coords = c("XCoord","YCoord"), crs = 26911)
write_sf(CCR_sf, "CCRPIPO_trees.shp")

# get bounding box
CCR_bbox <- st_as_sfc(st_bbox(CCR_sf))
write_sf(CCR_bbox, "CCRPIPO_bounds.shp")
