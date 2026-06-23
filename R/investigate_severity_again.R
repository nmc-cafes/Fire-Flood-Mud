library(here)
library(tidyverse)
library(terra)

post <- read_csv(here("QF_results","SBS","qf_results_site_corrected.csv")) %>%
  select(fire, site, severity_pct) %>%
  mutate(severe = if_else(severity_pct>0.25,1,0)) %>%
  mutate(severe = as.factor(severe)) %>%
  mutate(pre_post = "post")

fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
fires_list <- list()
for(f in 1:length(fires)){
  basins <- vect(here("Fire_Data",fires[f],paste0(fires[f],"_sample_basins_sbs.shp")))
  basins_df <- as_tibble(basins)
  basins_clean <- basins_df %>%
    mutate(id = row.names(basins_df)) %>%
    mutate(site = paste0(substr(fires[f],1,3),id)) %>%
    mutate(fire = fires[f]) %>%
    select(fire, site, severe_per, severe)
  if(fires[f] %in% c("CedarCreek","CubCreek2")){
    basins_cor <- vect(here("Fire_Data","CedarCreek","CedarCreek_corrected_basins.shp"))
    basins_cor_df <- as_tibble(basins_cor)
    basins_cor_clean <- basins_cor_df %>%
      mutate(id = row.names(basins_cor_df)) %>%
      mutate(site = paste0(substr(fires[f],1,3),id,"_COR")) %>%
      mutate(fire = fires[f]) %>%
      select(fire, site, severe_per, severe)
    basins_clean <- bind_rows(basins_clean, basins_cor_clean)
  }
  fires_list[[f]] <- basins_clean
}
pre_raw <- bind_rows(fires_list)

set.seed(987654321)
random <- sample.int(20,1)
sites_to_replace <- c("Ced2","Ced14",paste0("Ced",random),"Cub14")

dat_replace <- pre_raw %>%
  filter(!site %in% sites_to_replace) %>%
  filter(!site %in% c("Cub2_COR","Cub3_COR"))
pre <- dat_replace %>%
  mutate(site = case_when(site=="Ced1_COR" ~ sites_to_replace[1],
                          site=="Ced2_COR" ~ sites_to_replace[2],
                          site=="Ced3_COR" ~ sites_to_replace[3],
                          site=="Cub1_COR" ~ sites_to_replace[4],
                          TRUE ~ site)) %>%
  rename(severity_pct = severe_per) %>%
  mutate(severe = as.factor(severe)) %>%
  mutate(severity_pct = severity_pct/100) %>%
  mutate(pre_post = "pre")

pre_post <- bind_rows(pre,post)


pre_post %>%
  filter(fire=="Caldor") %>%
  ggplot() +
  geom_point(aes(site, severity_pct, color = pre_post)) +
  geom_hline(yintercept = 0.25, color = "darkseagreen4", linetype="dashed") +
  theme_bw()

post %>%
  group_by(fire) %>%
  summarize(severe_25 = sum(severity_pct>0.25))

