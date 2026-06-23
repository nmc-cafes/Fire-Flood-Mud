library(here)

speeds <- data.frame(fire = rep(NA,5), gust = rep(NA,5), max_speed = rep(NA,5))
fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
for(i in 1:length(fires)){
  raws <- read.table(here("RAWS_data",paste0(fires[i],"_RAWS.txt")), header = T)
  raws <- raws[raws$Speed != -9999,]
  gust90 <- quantile(raws$Gust, 0.9, na.rm = T)
  gust75 <- quantile(raws$Gust, 0.75, na.rm = T)
  speeds$fire[i] <- fires[i]
  speeds$gust[i] <- gust75
  speeds$max_speed[i] <- max(raws$Speed, na.rm = T)
}

## KNP: Ash Mountain
## Caldor: Owens Camp
## Dixie: Humbug Summit
## CedarCreek: First Butte
## CubCreek2: First Butte

speeds
