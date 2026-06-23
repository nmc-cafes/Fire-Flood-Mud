library(here)
library(tidyverse)
library(terra)

fires <- c("Caldor","CedarCreek","CubCreek2","Dixie","KNP")
for (fire_name in fires){
  basins_sampled <- vect(here("Fire_Data",fire_name,paste0(fire_name,"_sample_basins_sbs.shp")))
  df_in <- tibble(basin = seq(1,20,1), sev_in = basins_sampled$severe_per)
  
  dat_site <- read.csv(here("QF_results","SBS","qf_results_site.csv"))
  df_out <- dat_site %>% 
    filter(fire==fire_name) %>%
    select(fire, site, severity_pct) %>%
    mutate(severity_pct = severity_pct*100,
           site = parse_number(site)) %>%
    rename(basin = site, sev_out = severity_pct)
  
  df_fire <- left_join(df_in,df_out,by=join_by(basin))
  if(fire_name == fires[1]){
    df <- df_fire
  } else{
    df <- bind_rows(df, df_fire)
  }
}

nrow(df)
head(df)

median(df$sev_out)
nrow(df[df$sev_out>30,])
nrow(df[df$sev_out<30,])

df %>%
  group_by(fire) %>%
  summarize(severe_25 = sum(sev_out>25))

## I need to remove 3 runs under 25% from Cub Creek and 1 run under 25 from Cedar Creek
## Then I need to simulate 3 over 25 from Cub and 1 over 25 from Cedar