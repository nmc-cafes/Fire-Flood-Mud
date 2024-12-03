library(here)
library(tidyverse)
library(terra)

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

process_output <- function(fire, site, output, domain, slope){
  out_arr <- read.table(here("QF_results",
                             fire, 
                             site,
                             "Arrays",
                             paste0(output,".txt")))
  out_rst <- output_to_rst(output, out_arr, domain)
  dom_slope <- crop(slope, ext(out_rst))
  dom_slope <- project(dom_slope, out_rst)
  out_rst[dom_slope<23] <- NA
  out_rst <- mask(out_rst, basin)
  return(out_rst)
}

process_severity <- function(severity, out_rst){
  dom_sev <- crop(severity,ext(out_rst))
  dom_sev <- project(dom_sev, out_rst, method="near")
  dom_sev[is.na(out_rst)] <- NA
  return(dom_sev)
}

fires <- c("Caldor","CedarCreek","Dixie","KNP")
outputs <- c("canopy_consumption_pct",
             "canopy_consumption_tot",
             "canopy_remaining_pct",
             "canopy_residence_time",
             "energy_flux",
             "mass_burnt_pct",
             "max_power",
             "surface_consumption_pct",
             "surface_consumption_tot",
             "surface_remaining_pct",
             "surface_residence_time",
             "total_power")

alldata_list <- list()
for(j in 1:length(fires)){
  cat(fires[j],"\n")
  fire_list <- list()
  dem <- rast(here(fires[j], paste0(fires[j],"_DEM.tif")))
  severity <- rast(here(fires[j],paste0(fires[j],"_Severity.tif")))
  slope <- terrain(dem, v="slope", unit="degree")
  basins <- vect(here(fires[j],paste0(fires[j],"_sample_basins.shp")))
  for(i in 1:20){
    site <- paste0(substr(fires[j],1,3),i)
    cat("\t",site,"\n")
    domain <- vect(here(fires[j],"Sample_Basins",site,paste0(site,".shp")))
    basin <- basins[i]
    for(output in outputs){
      cat("\t\t",output,"\n")
      out_rst <- process_output(fires[j],site,output,domain,slope)
      if(output == outputs[1]){
        dom_sev <- process_severity(severity,  out_rst)
        sev_dat <- values(dom_sev, mat=F)
        sev_dat <- sev_dat[!is.na(sev_dat)]
        site_df <- tibble(severity = sev_dat)
      }
      out_dat <- values(out_rst, mat=F)
      out_dat <- out_dat[!is.na(out_dat)]
      site_df$new <- out_dat
      names(site_df)[names(site_df)=="new"] <- output
    }
    site_df$site <- site
    site_df$fire <- fires[j]
    fire_list[[i]] <- site_df
  }
  fire_df <- bind_rows(fire_list)
  alldata_list[[j]] <- fire_df
}

alldata_df <- bind_rows(alldata_list)

write.csv(alldata_df, here("QF_results","qf_results.csv"), row.names=F)
