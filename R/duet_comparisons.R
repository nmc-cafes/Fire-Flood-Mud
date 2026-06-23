"
Comparing calibrated DUET to FastFuels
"
library(here)
library(tidyverse)
library(terra)

ext_buff <- function(rst, buffer = -25){
  new_ext <- ext(rst)
  new_ext[1] <- new_ext[1] - buffer
  new_ext[2] <- new_ext[2] + buffer
  new_ext[3] <- new_ext[3] - buffer
  new_ext[4] <- new_ext[4] + buffer
  return(new_ext)
}

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

################
outputs <- c("mass_burnt_pct",
             "surface_consumption",
             "canopy_consumption",
             "max_power",
             "residence_time_power",
             "residence_time_consumption",
             "max_reaction_rate")

run_list = c("Caldor_Camp2_500m","Caldor_Camp2_500m_duet")
option_list = c("no_duet","duet")

first_option = T
for(i in 1:length(option_list)){
  first_output <- T
  for(output in outputs){
    cat("\t",output,"\n")
    out_arr <- read.table(here("Arrays",
                               "Caldor", 
                               run_list[i],
                               "Arrays",
                               paste0(output,".txt")))
    out_vec <- unlist(as.vector(out_arr))
    names(out_vec) <- output
    if(first_output){
      opt_dat = data.frame(out_vec)
    } else{
      out_dat = data.frame(out_vec)
      opt_dat = cbind(all_dat,out_dat)
    }
    rm(out_vec)
    first_output <- F
  }
  opt_dat$option <- option_list[i]
  if(first_option){
    all_dat <- opt_dat
  } else{
    all_dat = rbind(all_dat,opt_dat)
  }
  first_option = F
}
