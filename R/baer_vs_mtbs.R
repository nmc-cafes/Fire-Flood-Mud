library(here)
library(terra)

fire <- "Caldor"

if(fire != "CedarCreek"){
  baer <- rast(here("Fire_Severity","BAER",paste0(fire,"_dNBR_BAER.tif")))
}
# provisional <- rast(here("Fire_Severity","provisional",fire,paste0(fire,"_dNBR_provisional.tif")))
mtbs <- rast(here("Fire_Data",fire,paste0(fire,"_dNBR.tif")))
perim <- vect(here("Fire_Data",fire,paste0(fire,"_perimeter.shp")))

baer_proj <- project(baer, mtbs)
baer_mask <- mask(baer_proj, perim)
# prov_proj <- project(provisional, mtbs)
# prov_mask <- mask(prov_proj, perim)
mtbs_mask <- mask(mtbs, perim)

diff <- baer_mask - mtbs_mask
# diff <- prov_mask - mtbs_mask
plot(diff)

std <- sd(values(diff), na.rm=T)

diff_mat <- matrix(c(-std,std,0,
                     -2*std,-std,1,
                     std,2*std,1,
                     -3*std,-2*std,2,
                     2*std,3*std,2,
                     -Inf,-3*std,3,
                     3*std,Inf,3),
                   ncol=3,
                   byrow = T)
diff_rct <- terra::classify(diff, diff_mat)
plot(diff_rct)
