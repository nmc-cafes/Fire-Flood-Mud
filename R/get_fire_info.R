library(here)
library(terra)
library(tidyverse)
library(tidyterra)

caldor <- vect(here("Fire_Data","Caldor","Caldor_perimeter.shp"))
dixie <- vect(here("Fire_Data","Dixie","Dixie_perimeter.shp"))
knp <- vect(here("Fire_Data","KNP","KNP_perimeter.shp"))
cedar <- vect(here("Fire_Data","CedarCreek","CedarCreek_perimeter.shp"))
cub <- vect(here("Fire_Data","CubCreek2","CubCreek2_perimeter.shp"))

fires <- c(caldor,dixie,knp,cedar,cub)
for(i in 1:length(fires)){
  print(expanse(fires[i])/1000000)
}

cal_dem <- rast(here("Fire_Data","Caldor","Caldor_DEM.tif"))
dix_dem <- rast(here("Fire_Data","Dixie","Dixie_DEM.tif"))
knp_dem <- rast(here("Fire_Data","KNP","KNP_DEM.tif"))
ced_dem <- rast(here("Fire_Data","CedarCreek","CedarCreek_DEM.tif"))
cub_dem <- rast(here("Fire_Data","CubCreek2","CubCreek2_DEM.tif"))

elev_range <- function(perim, dem){
  msk <- mask(dem, perim)
  mn <- min(values(msk), na.rm=T)
  mx <- max(values(msk), na.rm=T)
  cat(mn, mx)
}

elev_range(caldor, cal_dem)
elev_range(dixie, dix_dem)
elev_range(knp, knp_dem)
elev_range(cedar, ced_dem)
elev_range(cub, cub_dem)

percent_steep <- function(perim, dem){
  slope <- terrain(dem)
  msk <- mask(slope, perim)
  vals <- values(msk, mat=F, na.rm=T)
  pct <- length(vals[vals>23])/length(vals)
  cat(pct)
}

percent_steep(caldor, cal_dem)
percent_steep(dixie, dix_dem)
percent_steep(knp, knp_dem)
percent_steep(cedar, ced_dem)
percent_steep(cub, cub_dem)

cal_sbs <- rast(here("Fire_Data","Caldor","Caldor_SBS.tif"))
cal_sbs <- project(cal_sbs, cal_dem, method="near")
dix_sbs <- rast(here("Fire_Data","Dixie","Dixie_SBS.tif"))
dix_sbs <- project(dix_sbs, dix_dem, method="near")
knp_sbs <- rast(here("Fire_Data","KNP","KNP_SBS.tif"))
knp_sbs <- project(knp_sbs, knp_dem, method="near")
ced_sbs <- rast(here("Fire_Data","CedarCreek","CedarCreek_SBS.tif"))
ced_sbs <- project(ced_sbs, ced_dem, method="near")
cub_sbs <- rast(here("Fire_Data","CubCreek2","CubCreek2_SBS.tif"))
cub_sbs <- project(cub_sbs, cub_dem, method="near")

percent_at_severity <- function(perim, sbs, severity){
  sev <- list(
    "unburned" = 1,
    "low" = 2,
    "moderate" = 3,
    "high" = 4
  )
  sbs[sbs==15] <- NA
  sbs[sbs==0] <- NA
  msk <- mask(sbs, perim)
  vals <- values(msk, mat=F, na.rm=T)
  pct <-  length(vals[vals==sev[[severity]]])/length(vals)
  return(pct)
}

sev <- list(
  "unburned" = c(),
  "low" = c(),
  "moderate" = c(),
  "high" = c()
)

cal_sev <- sev
for(severity in names(cal_sev)){
  cal_sev[[severity]] <- percent_at_severity(caldor, cal_sbs, severity)
}
dix_sev <- sev
for(severity in names(dix_sev)){
  dix_sev[[severity]] <- percent_at_severity(dixie, dix_sbs, severity)
}
knp_sev <- sev
for(severity in names(knp_sev)){
  knp_sev[[severity]] <- percent_at_severity(knp, knp_sbs, severity)
}
ced_sev <- sev
for(severity in names(ced_sev)){
  ced_sev[[severity]] <- percent_at_severity(cedar, ced_sbs, severity)
}
cub_sev <- sev
for(severity in names(cub_sev)){
  cub_sev[[severity]] <- percent_at_severity(cub, cub_sbs, severity)
}

percent_severe_on_steep <- function(perim, sbs, dem){
  slope <- terrain(dem)
  slope_msk <- mask(slope, perim)
  steep <- classify(slope_msk, 
                    rcl = matrix(c(0,23,0,23,Inf,1), 
                                 nrow=2,
                                 byrow = T))
  sev_msk <- mask(sbs, perim)
  mod_to_high <- sev_msk
  mod_to_high[mod_to_high%in%c(1,2,15)] <- 0
  mod_to_high[mod_to_high>0] <- 1
  steep_sev <- steep*mod_to_high
  ss_vals <- values(steep_sev, mat=F, na.rm=T)
  sev_vals <- values(mod_to_high, mat=F, na.rm=T)
  pct <- length(ss_vals[ss_vals==1])/length(sev_vals[sev_vals==1])
  cat(pct)
}

cal_ss <- percent_severe_on_steep(caldor, cal_sbs, cal_dem)
dix_ss <- percent_severe_on_steep(dixie, dix_sbs, dix_dem)
knp_ss <- percent_severe_on_steep(knp, knp_sbs, knp_dem)
ced_ss <- percent_severe_on_steep(cedar, ced_sbs, ced_dem)
cub_ss <- percent_severe_on_steep(cub, cub_sbs, cub_dem)

bps_wa <- rast(here("LANDFIRE","LF2016_BPS_WA","LC16_BPS_200.tif"))
cedar_proj <- project(cedar, bps_wa)
cub_proj <- project(cub, bps_wa)

ced_bps <- mask(crop(bps_wa, cedar_proj),cedar_proj)
cub_bps <- mask(crop(bps_wa, cub_proj), cub_proj)

bps_ca <- rast(here("LANDFIRE","LF2016_BPS_CA","LC16_BPS_200.tif"))
caldor_proj <- project(caldor, bps_ca)
dixie_proj <- project(dixie, bps_ca)
knp_proj <- project(knp, bps_ca)

cal_bps <- mask(crop(bps_ca, caldor_proj),caldor_proj)
dix_bps <- mask(crop(bps_ca, dixie_proj), dixie_proj)
knp_bps <- mask(crop(bps_ca, knp_proj), knp_proj)

get_dominant_bps <- function(rst, fire){
  rst_cats <- cats(rst)[[1]]
  rst_vals <- values(rst, mat=F, na.rm=T)
  rst_df <- tibble(VALUE = rst_vals) %>%
    left_join(rst_cats, by=join_by("VALUE"))
  n_row <- nrow(rst_df)
  rst_out <- rst_df %>% 
    count(BPS_NAME) %>% 
    arrange(-n) %>% 
    slice(1:2) %>% 
    mutate(fire = fire,
           n_pct = n/n_row) %>% 
    rename(veg_type = BPS_NAME)
  return(rst_out)
}

ced_dom <- get_dominant_bps(ced_bps, "cedar")
cub_dom <- get_dominant_bps(cub_bps, "cub")
cal_dom <- get_dominant_bps(cal_bps, "caldor")
dix_dom <- get_dominant_bps(dix_bps, "dixie")
knp_dom <- get_dominant_bps(knp_bps, "knp")

dom_veg <- bind_rows(ced_dom, cub_dom, cal_dom, dix_dom, knp_dom)

