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

process_output <- function(fire, site, output, domain, basin){
  out_arr <- read.table(here("QF_results",
                             "SBS",
                             fire, 
                             site,
                             "Arrays",
                             paste0(output,".txt")))
  out_rst <- output_to_rst(output, out_arr, domain)
  out_rst <- mask(out_rst, basin)
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

process_tree_mortality <- function(tree_mortality, out_rst){
  dom_tm <- crop(tree_mortality,ext(out_rst))
  dom_tm[is.na(dom_tm)] <- 0
  dom_tm <- project(dom_tm, out_rst, method="near")
  dom_tm[is.na(out_rst)] <- NA
  return(dom_tm)
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

# fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
fires <- c("CedarCreek","CubCreek2")
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
  dem <- rast(here("Fire_Data",fires[j], paste0(fires[j],"_DEM.tif")))
  severity <- rast(here("Fire_Data",fires[j],paste0(fires[j],"_SBS.tif")))
  dnbr <- rast(here("Fire_Data",fires[j],paste0(fires[j],"_dNBR.tif")))
  slope <- terrain(dem, v="slope", unit="degree")
  # basins <- vect(here("Fire_Data",fires[j],paste0(fires[j],"_sample_basins_sbs.shp")))
  basins <- vect(here("Fire_Data",fires[j],paste0(fires[j],"_corrected_basins.shp")))
  for(i in 1:3){
    site <- paste0(substr(fires[j],1,3),i,"_COR")
    basin_path <- here("Fire_Data",fires[j],"Sample_Basins_corrected",site)
    if( dir.exists(basin_path)){
      cat("\t",site,"\n")
      domain <- vect(here("Fire_Data",fires[j],"Sample_Basins_corrected",site,paste0(site,".shp")))
      basin <- basins[i]
      for(output in outputs){
        out_rst <- process_output(fires[j],site,output,domain,basin)
        if(output == outputs[1]){
          dom_sev <- process_severity(severity,  out_rst)
          sev_dat <- values(dom_sev, mat=F)
          sev_dat <- sev_dat[!is.na(sev_dat)]
          slope_mask <- create_slope_mask(slope, out_rst)
          slope_dat <- values(slope_mask, mat=F)
          slope_dat <- slope_dat[!is.na(slope_dat)]
          dom_dnbr <- process_dnbr(dnbr, out_rst)
          dnbr_dat <- values(dom_dnbr, mat=F)
          dnbr_dat <- dnbr_dat[!is.na(dnbr_dat)]
          # dom_tm <- process_tree_mortality(tree_mortality, out_rst)
          # tm_small <- values(dom_tm$predictions_small, mat=F)
          # tm_medium <- values(dom_tm$predictions_medium, mat=F)
          # tm_large <- values(dom_tm$predictions_large, mat=F)
          # tm_small <- tm_small[!is.na(tm_small)]
          # tm_medium <- tm_medium[!is.na(tm_medium)]
          # tm_large <- tm_large[!is.na(tm_large)]
          site_df <- tibble("severity" = sev_dat, 
                            "steep" = slope_dat,
                            # "mortality_small" = tm_small,
                            # "mortality_medium" = tm_medium,
                            # "mortality_large" = tm_large,
                            "dNBR" = dnbr_dat)
        }
        cat("\t\t",output,"\n")
        out_dat <- values(out_rst, mat=F)
        out_dat <- out_dat[!is.na(out_dat)]
        site_df$new <- out_dat
        names(site_df)[names(site_df)=="new"] <- output
      }
      site_df$site <- site
      site_df$fire <- fires[j]
      fire_list[[i]] <- site_df
    }
  }
  fire_df <- bind_rows(fire_list)
  alldata_list[[j]] <- fire_df
}

alldata_df <- bind_rows(alldata_list)

# write.csv(alldata_df, here("QF_results","SBS","qf_results.csv"), row.names=F)
write.csv(alldata_df, here("QF_results","SBS","qf_results_corrected.csv"), row.names=F)
# write.csv(alldata_df, here("QF_results","KNP_results.csv"), row.names=F)

