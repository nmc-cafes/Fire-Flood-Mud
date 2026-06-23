library(here)
library(terra)
library(tidyverse)
library(tidyterra)
library(scico)
library(patchwork)
library(gifski)
library(basemaps)
library(maptiles)

prog <- vect(here("Caldor",
                  "Fire_Progression",
                  "CALDOR_2021_PROGRESSION_CORRECTED.shp")) %>% 
  project("EPSG:5070")
prog <- prog %>%
  mutate(DateCurren = as.Date(DateCurren))

############

# dates_sort<- sort(unique(prog$DateCurren))
# prog$GrowthRate <- NA
# for(i in 1:length(dates_sort)){
#   date_curr <- dates_sort[i]
#   date_prev <- dates_sort[i-1]
#   if(i != 1){
#     days <- as.numeric(date_curr - date_prev)
#   } else{
#     days <- 1
#   }
#   tmp_curr <- prog[prog$DateCurren==date_curr,]
#   tmp_prev <- prog[prog$DateCurren==date_prev,]
#   if(i != 1){
#     area_prev <- sum(tmp_prev$GISAcres)
#   } else{
#     area_prev <- 0
#   }
#   area_curr <- sum(tmp_curr$GISAcres)
#   area_delt <- area_curr - area_prev
#   growth <- area_delt / days
#   prog[prog$DateCurren==date_curr]$GrowthRate <- growth
# }

############

# prog_sliced <- prog %>%
#   group_by(DateCurren) %>%
#   slice_max(GISAcres,
#             with_ties = F)
# 
# prog_sliced$GrowthRate <- NA
# for(i in 1:nrow(prog_sliced)){
#   if(i != 1){
#     days <- as.numeric(prog_sliced[i,]$DateCurren - prog_sliced[i-1,]$DateCurren)
#   } else{
#     days <- 1
#   }
#   if(i != 1){
#     area_prev <- expanse(prog_sliced[i-1])
#   } else{
#     area_prev <- 0
#   }
#   area_curr <- expanse(prog_sliced[i])
#   area_delt <- area_curr - area_prev
#   growth <- area_delt / days
#   prog_sliced$GrowthRate[i] <- growth
# }
# 
# prog_sliced <- filter(prog_sliced, GrowthRate >= 0)

# ggplot() +
#   geom_spatvector(data = prog_sliced,
#                   aes(fill=GrowthRate)) +
#   facet_wrap(.~DateCurren) +
#   scale_fill_scico(palette = "vik") +
#   theme_void()
# 
# ggplot() +
#   geom_line(data=prog_sliced,
#             aes(DateCurren,GrowthRate)) +
#   theme_minimal()
# ggplot() +
#   geom_line(data=prog_sliced,
#             aes(DateCurren,GISAcres)) +
#   geom_point(data=prog_sliced,
#             aes(DateCurren,GISAcres, color=GrowthRate)) +
#   scale_color_scico(palette = "vik") +
#   theme_minimal()

############
# prog_growth <- prog_sliced[1]
# for (i in 2:nrow(prog_sliced)) {
#   prog_temp <- erase(prog_sliced[i], prog_sliced[i-1])
#   prog_growth <- rbind(prog_growth, prog_temp)
# }

#############
dates_sort <- sort(unique(prog$DateCurren))
prog_growth <- prog[prog$DateCurren==dates_sort[1],]
for (i in 2:nrow(prog)) {
  date_curr <- dates_sort[i]
  date_prev <- dates_sort[i-1]
  prog_temp <- erase(prog[prog$DateCurren==date_curr,], prog[prog$DateCurren==date_prev,])
  prog_growth <- rbind(prog_growth, prog_temp)
}

dates_sort2 <- sort(unique(prog_growth$DateCurren))
prog_growth$GrowthRate <- NA
for(i in 1:length(dates_sort2)){
  date_curr <- dates_sort2[i]
  date_prev <- dates_sort2[i-1]
  if(i != 1){
    days <- as.numeric(date_curr - date_prev)
  } else{
    days <- 1
  }
  tmp_curr <- prog_growth[prog_growth$DateCurren==date_curr,]
  area_curr <- sum(expanse(tmp_curr))*0.00024711
  growth <- area_curr / days
  prog_growth[prog_growth$DateCurren==date_curr]$GrowthRate <- growth
}
prog_growth$GrowthRate[prog_growth$GrowthRate<0] <- 0

max_gr <- prog_growth %>% 
  slice_max(GrowthRate) %>% 
  select(DateCurren, GrowthRate) %>%
  mutate(GrowthRate = GrowthRate*0.0040468564)
next_week <- prog_growth %>% 
  filter(DateCurren > as.Date("2021-09-02"), DateCurren < as.Date("2021-10-01")) %>% 
  select(DateCurren, GrowthRate) %>%
  mutate(GrowthRate = GrowthRate*0.0040468564)

p1 <- ggplot() +
  geom_spatvector(data=prog_growth,
                  aes(fill=GrowthRate)) +
  scale_fill_scico(palette = "managua", direction=-1) +
  theme_void() +
  theme(legend.position = "none")

p2 <- ggplot() +
  geom_spatvector(data=prog_growth,
                  aes(fill=DateCurren)) +
  scale_fill_scico(palette = "imola") +
  theme_void() +
  theme(legend.position = "none")

p1 + p2 

############

daily_growth <- tibble(date = dates_sort2,
                       growth_rate = NA)

for(i in 1:nrow(unique(daily_growth))){
  daily_growth[daily_growth$date==dates_sort2[i],]$growth_rate <- prog_growth[prog_growth$DateCurren==dates_sort2[i],]$GrowthRate[1]
  }
daily_growth <- daily_growth %>% mutate(growth_rate = growth_rate*0.0040468564)
ggplot() +
  geom_line(data=daily_growth,
            aes(date,growth_rate)) +
  geom_point(data=daily_growth,
            aes(date,growth_rate, color=growth_rate)) +
  scale_color_scico(palette = "managua", direction=-1) +
  theme_minimal() +
  theme(legend.position = "none")

############
map_token <- "V2tHpgnaN9td5HYNOV9C"
set_defaults(map_service = "maptiler", map_type = "satellite", map_token = map_token)

prog_proj <- project(prog, "EPSG:3857")

fire <- "CedarCreek"
if(fire=="CedarCreek"){
  prog_proj[prog_proj$DateCurren=="2021-08-13"] # TODO: replace 8/13 and 8/14 with polygon from 8/12 (or add it in)
}
for(i in 1:nrow(prog)){
  plt <- ggplot() +
    basemap_gglayer(ext=vect(ext(prog_proj), crs=crs(prog_proj))) +
    scale_fill_identity() +
    geom_spatvector(data = prog_proj[i],
                    fill="darkred",
                    color="yellow",
                    alpha=0.5) +
    ggtitle(prog_proj[i]$DateCurren) +
    theme_void() +
    theme(legend.position = "none")
  date <- as.character(as.Date(prog_proj[i]$DateCurren))
  ggsave(paste0("CedarCreek_",date,".png"),path = here("CedarCreek","Fire_Progression","Gif_Corrected"))
}

png_files <- list.files(here("CedarCreek","Fire_Progression","Gif_Corrected"), full.names = T)
gifski(png_files = png_files, 
       gif_file = here("CedarCreek","Fire_Progression","CedarCreek_Corrected.gif"),
       delay = 0.5)


