library(terra)
library(here)
library(tidyverse)
library(tidyterra)
library(gifski)

# CALDOR
prog <- vect(here("Fire_Data",
                  "Caldor",
                  "Fire_Progression", 
                  "CALDOR_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

for(i in 1:nrow(prog)){
  plt <- ggplot() +
    geom_spatvector(data = prog[i]) +
    xlim(ext(prog)[1],ext(prog)[2]) +
    ylim(ext(prog)[3],ext(prog)[4]) +
    ggtitle(prog[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog[i]$DateCurren))
  ggsave(paste0("Caldor_",date,".png"),path = here("Fire_Data","Caldor","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","Caldor","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","Caldor","Fire_Progression","Caldor_Uncorrected.gif"),
       delay = 0.5)

ggplot() +
  geom_spatvector(data = prog[prog$DateCurren==as.Date("2021-08-31")], fill = "blue") +
  geom_spatvector(data = prog[prog$DateCurren==as.Date("2021-08-30")], fill = "red") +
  theme_bw()

prog$DateCurren <- as.Date(prog$DateCurren)
ggplot() +
  geom_spatvector(data = prog, aes(color = DateCurren), fill=NA) +
  scale_color_hypso_c() +
  theme_bw()

shift_x <- -3350
shift_y <- 2125
wrong <- prog[prog$DateCurren == "2021-08-30",]
right <- prog[prog$DateCurren != "2021-08-30",]

corrected <- shift(wrong, dx = shift_x, dy = shift_y)
correct <- rbind(corrected, right)

ggplot() +
  geom_spatvector(data = correct[correct$DateCurren==as.Date("2021-08-30")], fill = "red") +
  geom_spatvector(data = correct[correct$DateCurren==as.Date("2021-08-31")], fill = "blue") +
  theme_bw()

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "Caldor",
                 "Fire_Progression",
                 "CALDOR_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("Fire_Data",
                   "Caldor",
                   "Caldor_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# CEDAR CREEK
prog <- vect(here("Fire_Data",
                  "CedarCreek",
                  "Fire_Progression", 
                  "CEDAR_CREEK_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

dates <- unique(prog$DateCurren)
dates <- dates[!is.na(dates)]

vect_list <- list()
for(i in 1:length(dates)){
  sameday <- prog[prog$DateCurren == dates[i]]
  fireday <- aggregate(sameday, by = "DateCurren")
  vect_list[[i]] <- fireday
}
prog_agg <- vect(vect_list)

for(i in 1:nrow(prog_agg)){
  plt <- ggplot() +
    geom_spatvector(data = prog_agg[i]) +
    xlim(ext(prog_agg)[1],ext(prog_agg)[2]) +
    ylim(ext(prog_agg)[3],ext(prog_agg)[4]) +
    ggtitle(prog_agg[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog_agg[i]$DateCurren))
  ggsave(paste0("CedarCreek_",date,".png"),path = here("Fire_Data","CedarCreek","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","CedarCreek","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","CedarCreek","Fire_Progression","CedarCreek_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]
correct <- correct[correct$DateCurren >= "2021-07-15"]
correct <- correct[correct$DateCurren != "2021-08-08"]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "CedarCreek",
                 "Fire_Progression",
                 "CEDAR_CREEK_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("Fire_Data",
                   "CedarCreek",
                   "CedarCreek_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# MUCKAMUCK
prog <- vect(here("Fire_Data",
                  "Muckamuck",
                  "Fire_Progression", 
                  "MUCKAMUCK_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

for(i in 1:nrow(prog)){
  plt <- ggplot() +
    geom_spatvector(data = prog[i]) +
    xlim(ext(prog)[1],ext(prog)[2]) +
    ylim(ext(prog)[3],ext(prog)[4]) +
    ggtitle(prog[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog[i]$DateCurren))
  ggsave(paste0("Muckamuck_",date,".png"),path = here("Fire_Data","Muckamuck","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","Muckamuck","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","Muckamuck","Fire_Progression","Muckamuck_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)
correct <- prog[!is.na(prog$DateCurren)]
correct <- correct[correct$DateCurren != "2021-08-08"]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "Muckamuck",
                 "Fire_Progression",
                 "MUCKAMUCK_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("Fire_Data",
                   "Muckamuck",
                   "Muckamuck_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# CUB CREEK
prog <- vect(here("Fire_Data",
                  "CubCreek2",
                  "Fire_Progression", 
                  "CUB_CREEK_2_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

for(i in 1:nrow(prog)){
  plt <- ggplot() +
    geom_spatvector(data = prog[i]) +
    xlim(ext(prog)[1],ext(prog)[2]) +
    ylim(ext(prog)[3],ext(prog)[4]) +
    ggtitle(prog[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog[i]$DateCurren))
  ggsave(paste0("CubCreek2_",date,".png"),path = here("Fire_Data","CubCreek2","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","CubCreek2","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","CubCreek2","Fire_Progression","CubCreek2_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "CubCreek2",
                 "Fire_Progression",
                 "CUB_CREEK_2_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

# DIXIE
prog <- vect(here("Fire_Data",
                  "Dixie",
                  "Fire_Progression", 
                  "DIXIE_SUGAR_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

dates <- unique(prog$DateCurren)
dates <- dates[!is.na(dates)]

vect_list <- list()
for(i in 1:length(dates)){
  sameday <- prog[prog$DateCurren == dates[i]]
  fireday <- aggregate(sameday, by = "DateCurren")
  vect_list[[i]] <- fireday
}
prog_agg <- vect(vect_list)

for(i in 1:nrow(prog_agg)){
  plt <- ggplot() +
    geom_spatvector(data = prog_agg[i]) +
    xlim(ext(prog_agg)[1],ext(prog_agg)[2]) +
    ylim(ext(prog_agg)[3],ext(prog_agg)[4]) +
    ggtitle(prog_agg[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog_agg[i]$DateCurren))
  ggsave(paste0("Dixie_",date,".png"),path = here("Fire_Data","Dixie","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","Dixie","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","Dixie","Fire_Progression","Dixie_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "Dixie",
                 "Fire_Progression",
                 "DIXIE_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

# KNP
prog <- vect(here("Fire_Data",
                  "KNP",
                  "Fire_Progression", 
                  "KNP_COMPLEX_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

dates <- unique(prog$DateCurren)
dates <- dates[!is.na(dates)]

vect_list <- list()
for(i in 1:length(dates)){
  sameday <- prog[prog$DateCurren == dates[i]]
  fireday <- aggregate(sameday, by = "DateCurren")
  vect_list[[i]] <- fireday
}
prog_agg <- vect(vect_list)

for(i in 1:nrow(prog_agg)){
  plt <- ggplot() +
    geom_spatvector(data = prog_agg[i]) +
    xlim(ext(prog_agg)[1],ext(prog_agg)[2]) +
    ylim(ext(prog_agg)[3],ext(prog_agg)[4]) +
    ggtitle(prog_agg[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog_agg[i]$DateCurren))
  ggsave(paste0("KNP_",date,".png"),path = here("Fire_Data","KNP","Fire_Progression","Gif"))
}

png_files <- list.files(here("Fire_Data","KNP","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Fire_Data","KNP","Fire_Progression","KNP_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]
correct <- correct[correct$DateCurren != "2021-09-21"]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Fire_Data",
                 "KNP",
                 "Fire_Progression",
                 "KNP_COMPLEX_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)
