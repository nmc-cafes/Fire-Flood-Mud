library(here)
library(tidyverse)
library(terra)
library(tidyterra)

fire <- "KNP"

sites <- vect(here("Fire_Data",fire, paste0(fire,"_sample_basins.shp")))
tbs <- rast(here("Fire_Data",fire, paste0(fire,"_Severity.tif")))
sbs_raw <- rast(here("Fire_Data",fire, paste0(fire,"_SBS.tif")))

sbs <- project(sbs_raw, tbs, method="near")

sites$ID <- seq(1,nrow(sites))
for(i in 1:nrow(sites)){
  sbs_i <- crop(sbs, sites[i])
  sbs_i <- mask(sbs_i, sites[i])
  sbs_i_val <- values(sbs_i, wide=F)
  all_sbs_i <- length(sbs_i_val[!is.na(sbs_i_val)])
  severe_sbs_i <- length(sbs_i_val[sbs_i_val%in%c(3,4)])
  severe_pct <- severe_sbs_i/all_sbs_i
  sites$sbs_pct[i] <- severe_pct*100
}

ggplot(sites,aes(severe_per,sbs_pct)) +
  geom_point(alpha = 0.5) +
  geom_text(aes(label = ID), hjust = -0.2, vjust = -0.2) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 30, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = 30, color = "blue", linetype = "dashed") +
  coord_equal(xlim = c(0,100), ylim = c(0,100)) +
  labs(x = "Thematic Severity Percent",
       y = "Soil Burn Severity Percent") +
  theme_bw()

for(j in c(11,13,19,20)){
  plot(crop(mask(sbs,sites[j]),sites[j]), main = "SBS")
  plot(crop(mask(tbs,sites[j]),sites[j]), main = "Thematic")
}


