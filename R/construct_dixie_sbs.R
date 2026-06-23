library(here)
library(terra)


phase1 <- rast(here("Soil_Burn_Severity","Dixie_SBS","Dixie_sbs_Phase1","DIXIE_sbs.tif"))
phase2 <- rast(here("Soil_Burn_Severity","Dixie_SBS","Dixie_sbs_Phase2","DIXIE_sbs.tif"))
phase3 <- rast(here("Soil_Burn_Severity","Dixie_SBS","Dixie_sbs_Phase3","SBS_Phase3_1007.tif"))

phase1[phase1==15] <- 0
phase2[phase2==15] <- 0
phase3[phase3==15] <- 0

phase12 <- mosaic(phase1, phase2, fun=max)
phase123 <- mosaic(phase12, phase3, fun=max)

writeRaster(phase123, here("Dixie","Dixie_SBS.tif"))
