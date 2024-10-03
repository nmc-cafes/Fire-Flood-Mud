library(terra)
library(here)
library(tidyverse)

celsius_to_fahrenheit <- function(temp_C) {
  temp_F <- (temp_C * 9/5) + 32
  return(temp_F)
}

calc_rh_raster <- function(temp_raster_C, vpd_raster_hPa) {
  # Convert VPD from hPa to kPa
  vpd_raster_kPa <- vpd_raster_hPa / 10
  # Define a function to calculate RH for each pixel
  rh_calculation <- function(temp_C, vpd_kPa) {
    # Step 1: Calculate saturation vapor pressure (e_s) in kPa
    e_s <- 0.6108 * exp((17.27 * temp_C) / (temp_C + 237.3))
    # Step 2: Calculate actual vapor pressure (e)
    e <- e_s - vpd_kPa
    # Step 3: Calculate relative humidity (RH)
    rh_percent <- (e / e_s) * 100
    return(rh_percent)
  }
  # Apply the calculation across the raster layers
  rh_raster <- terra::lapp(c(temp_raster_C, vpd_raster_kPa), fun = rh_calculation)
  return(rh_raster)
}

fires <- c("Caldor","Dixie","KNP","CedarCreek","CubCreek2")
months <- c("08","08","09","07","07")
df_list <- list()

for(i in 1:length(fires)){
  cat(fires[i],"\n")
  tmax <- rast(here("Climate_Data",
                    paste0("PRISM_tmax_stable_4kmM3_2021",months[i],"_bil"),
                    paste0("PRISM_tmax_stable_4kmM3_2021",months[i],"_bil.bil")))
 tmin <- rast(here("Climate_Data",
                   paste0("PRISM_tmin_stable_4kmM3_2021",months[i],"_bil"),
                   paste0("PRISM_tmin_stable_4kmM3_2021",months[i],"_bil.bil")))
 vmax <- rast(here("Climate_Data",
                   paste0("PRISM_vpdmax_stable_4kmM3_2021",months[i],"_bil"),
                   paste0("PRISM_vpdmax_stable_4kmM3_2021",months[i],"_bil.bil")))
 vmin <- rast(here("Climate_Data",
                   paste0("PRISM_vpdmin_stable_4kmM3_2021",months[i],"_bil"),
                   paste0("PRISM_vpdmin_stable_4kmM3_2021",months[i],"_bil.bil")))
 tmax <- project(tmax, "EPSG:5070")
 tmin <- project(tmin, "EPSG:5070")
 vmax <- project(vmax, "EPSG:5070")
 vmin <- project(vmin, "EPSG:5070")
 
 rh_max <- calc_rh_raster(tmax, vmax)
 rh_min <- calc_rh_raster(tmin, vmin)
 
 bounds <- vect(here(fires[i],paste0(fires[i],"_perimeter.shp")))
 rh_max <- mask(crop(rh_max,bounds),bounds)
 rh_max <- mask(crop(rh_min,bounds),bounds)
 t_max <- mask(crop(tmax,bounds),bounds)
 t_min <- mask(crop(tmin,bounds),bounds)
 
 rh_max_mean <- mean(values(rh_max), na.rm=T)
 rh_min_mean <- mean(values(rh_min), na.rm=T)
 t_max_mean <- mean(values(t_max), na.rm=T)
 t_min_mean <- mean(values(t_min), na.rm=T)
 
 t_max_mean <- celsius_to_fahrenheit(t_max_mean)
 t_min_mean <- celsius_to_fahrenheit(t_min_mean)
 
 vals <- c(t_max_mean, t_min_mean, rh_max_mean, rh_min_mean)
 df <- tibble(fire = fires[i],
              metric = c(rep("temp_F",2),rep("rh_%",2)),
              bound = c("max","min","min","max"),
              value = vals)
 df_list[[i]] <- df
}

climate_ranges <- bind_rows(df_list)

climate_ranges %>% 
  ggplot() +
  geom_point(aes(fire,value,color=metric),
             position=position_dodge(width=0.35)) +
  scale_y_continuous(
    limits=c(0,100),
    name = "Temperatrue (C)",
    sec.axis = sec_axis( trans=~., name="Relative Humidity (%)")
  )+
  theme_bw()
