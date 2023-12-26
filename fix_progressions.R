library(terra)
library(here)
library(tidyverse)
library(tidyterra)
library(gifski)

# CALDOR
prog <- vect(here("Caldor",
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
  ggsave(paste0("Caldor_",date,".png"),path = here("Caldor","Fire_Progression","Gif"))
}

png_files <- list.files(here("Caldor","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Caldor","Fire_Progression","Caldor_Uncorrected.gif"),
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
            here("Caldor",
                 "Fire_Progression",
                 "CALDOR_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("Caldor",
                   "Caldor_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# CEDAR CREEK
prog <- vect(here("CedarCreek",
                  "Fire_Progression", 
                  "CEDAR_CREEK_2021_PROGRESSION.shp")) %>%
  project("EPSG:5070")

for(i in 1:nrow(prog)){
  plt <- ggplot() +
    geom_spatvector(data = prog[i]) +
    xlim(ext(prog)[1],ext(prog)[2]) +
    ylim(ext(prog)[3],ext(prog)[4]) +
    ggtitle(prog[i]$DateCurren) +
    theme_bw()
  date <- as.character(as.Date(prog[i]$DateCurren))
  ggsave(paste0("CedarCreek_",date,".png"),path = here("CedarCreek","Fire_Progression","Gif"))
}

png_files <- list.files(here("CedarCreek","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("CedarCreek","Fire_Progression","CedarCreek_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]
correct <- correct[correct$DateCurren != "2021-07-12"]
correct <- correct[correct$DateCurren != "2021-07-13"]
correct <- correct[correct$DateCurren != "2021-08-08"]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("CedarCreek",
                 "Fire_Progression",
                 "CEDAR_CREEK_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("CedarCreek",
                   "CedarCreek_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# MUCKAMUCK
prog <- vect(here("Muckamuck",
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
  ggsave(paste0("Muckamuck_",date,".png"),path = here("Muckamuck","Fire_Progression","Gif"))
}

png_files <- list.files(here("Muckamuck","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("Muckamuck","Fire_Progression","Muckamuck_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)
correct <- prog[!is.na(prog$DateCurren)]
correct <- correct[correct$DateCurren != "2021-08-08"]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("Muckamuck",
                 "Fire_Progression",
                 "MUCKAMUCK_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)

sites <- vect(here("Muckamuck",
                   "Muckamuck_sample_sites_NJT.shp"))
correct$DateCurren <- as.Date(correct$DateCurren)
ggplot() +
  geom_spatvector(data = correct, aes(color = DateCurren), fill=NA) +
  geom_spatvector(data = sites) + 
  scale_color_hypso_c() +
  theme_bw()

# CUB CREEK
prog <- vect(here("CubCreek2",
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
  ggsave(paste0("CubCreek2_",date,".png"),path = here("CubCreek2","Fire_Progression","Gif"))
}

png_files <- list.files(here("CubCreek2","Fire_Progression","Gif"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("CubCreek2","Fire_Progression","CubCreek2_Uncorrected.gif"),
       delay = 0.5)

prog$DateCurren <- as.Date(prog$DateCurren)

correct <- prog[!is.na(prog$DateCurren)]

correct$DateCurren <- as.character(correct$DateCurren)
writeVector(correct,
            here("CubCreek2",
                 "Fire_Progression",
                 "CUB_CREEK_2_2021_PROGRESSION_CORRECTED.shp"),
            overwrite=T)
