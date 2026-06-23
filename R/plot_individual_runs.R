library(here)
library(terra)
library(tidyverse)
library(tidyterra)
library(patchwork)

output_to_rst <- function(output, out_arr, plot_bounds){
  out_mat <- as.matrix(out_arr)
  out_rst <- rast(nrow = dim(out_mat)[1],
                  ncol = dim(out_mat)[2],
                  crs = crs(plot_bounds),
                  extent = ext(plot_bounds),
                  vals = out_mat,
                  names = output)
  out_rst <- flip(out_rst, direction="vertical")
  return(out_rst)
}

process_output <- function(fire, site, output, domain, basin){
  out_arr <- read.table(here("QF_results",
                             "SBS",
                             fire, 
                             site,
                             "Arrays",
                             paste0(output,".txt")))
  out_rst <- output_to_rst(output, out_arr, domain)
  # out_rst <- mask(out_rst, basin)
  return(out_rst)
}

process_severity <- function(severity, out_rst){
  out_rst_proj <- project(out_rst, crs(severity))
  dom_sev <- crop(severity,ext(out_rst_proj))
  dom_sev <- project(dom_sev, out_rst, method="near")
  dom_sev[is.na(dom_sev)] <- 1
  dom_sev[is.na(out_rst)] <- NA
  return(dom_sev)
}

process_dnbr <- function(dnbr, out_rst){
  dom_dnbr <- crop(dnbr,ext(out_rst))
  dom_dnbr <- project(dom_dnbr, out_rst, method="bilinear")
  dom_dnbr[is.na(out_rst)] <- NA
  return(dom_dnbr)
}

create_slope_mask <- function(slope, out_rst){
  dom_slope <- crop(slope, ext(out_rst))
  dom_slope <- project(dom_slope, out_rst)
  slope_mask <- out_rst
  slope_mask[dom_slope>23] <- 1
  slope_mask[dom_slope<=23] <- 0
  slope_mask[!slope_mask%in%c(0,1)] <- 0
  slope_mask[is.na(out_rst)] <- NA
  names(slope_mask) <- "steep"
  return(slope_mask)
}

fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
output <- "mass_burnt_pct"

j <- 5

dem <- rast(here("Fire_Data",fires[j], paste0(fires[j],"_DEM.tif")))
severity <- rast("Fire_Data",here(fires[j],paste0(fires[j],"_SBS.tif")))
dnbr <- rast(here("Fire_Data",fires[j],paste0(fires[j],"_dNBR.tif")))
slope <- terrain(dem, v="slope", unit="degree")
basins <- vect(here("Fire_Data",fires[j],paste0(fires[j],"_sample_basins_sbs.shp")))

# i <- 5
for(i in 1:20){
  site <- paste0(substr(fires[j],1,3),i)
  basin_path <- here("Fire_Data",fires[j],"Sample_Basins",site)
  cat("\t",site,"\n")
  domain <- vect(here("Fire_Data",fires[j],"Sample_Basins",site,paste0(site,".shp")))
  basin <- basins[i]
  
  out_rst <- process_output(fires[j],site,output,domain,basin)
  dom_sev <- process_severity(severity,  out_rst)
  if(length(unique(values(dom_sev)))==3){
    dom_sev <- dom_sev %>%
      mutate(KNP_SBS = factor(KNP_SBS, labels = c("Low Severity","Moderate Severity","HighSeverity")))
  }
  else if(length(unique(values(dom_sev)))==4){
    dom_sev <- dom_sev %>%
      mutate(KNP_SBS = factor(KNP_SBS, labels = c("Unburned","Low Severity","Moderate Severity","HighSeverity")))
  }
  slope_mask <- create_slope_mask(slope, out_rst)
  slope_mask[slope_mask==1] <- NA
  slope_vect <- as.polygons(slope_mask)
  dom_dnbr <- process_dnbr(dnbr, out_rst)
  
  # plot(out_rst)
  # plot(dom_sev)
  # plot(slope_mask)
  # plot(dom_dnbr)
  
  out <- ggplot() +
    geom_spatraster(data = out_rst) +
    geom_spatvector(data = slope_vect, fill = "white", alpha = 0.5, color = NA) +
    geom_spatvector(data = basin, fill=NA, linewidth = 1, color = "white") +
    scale_fill_viridis_c(option = "magma", direction = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(fill = "Mass Burnt (%)") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
  
  dNBR <- ggplot() +
    geom_spatraster(data = dom_dnbr) +
    geom_spatvector(data = slope_vect, fill = "white", alpha = 0.5, color = NA) +
    geom_spatvector(data = basin, fill=NA, linewidth = 1, color = "white") +
    scale_fill_viridis_c(option = "magma", direction = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(fill = "dNBR") +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom")
  
  thematic <- ggplot() +
    geom_spatraster(data = dom_sev) +
    geom_spatvector(data = slope_vect, fill = "white", alpha = 0.5, color = NA) +
    geom_spatvector(data = basin, fill=NA, linewidth = 1, color = "white") +
    scale_fill_viridis_d(option = "magma", direction = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_bw() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 2))
  
  print(out + dNBR + thematic)
}
