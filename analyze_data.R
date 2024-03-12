library(tidyverse)
library(here)
library(GGally)
library(terra)
library(ggthemes)

dat <- read.csv(here("all_data.csv")) %>%
  mutate(severity = case_when(severity==1 ~ "unburned",
                              severity==2 ~ "low",
                              severity==3 ~ "moderate",
                              severity==4 ~ "high"),
         canopy_consumption = canopy_consumption - surface_consumption)

# explore!

dat_site <- dat %>%
  group_by(site, fire) %>%
  summarize(dNBR = mean(dNBR),
            high_sev_pct = mean(severity%in%c("moderate","high")*100),
            mass_burnt_pct = mean(mass_burnt_pct),
            surface_consumption = sum(surface_consumption),
            canopy_consumption = sum(canopy_consumption),
            max_power = mean(max_power),
            residence_time_power = mean(residence_time_power),
            residence_time_consumption = mean(residence_time_consumption))

dat_site %>%
  ggplot() +
  geom_bar(stat = "identity", aes(site,mass_burnt_pct)) +
  facet_wrap(.~fire, scales = "free") +
  labs(x="",
       y="Mass Burnt (%)") +
  coord_flip() +
  theme_bw()

sites_with_fire <- dat_site %>% filter(mass_burnt_pct > 20) %>% pull(site)

dat_fire <- dat %>% filter(site %in% sites_with_fire)
dat_site_fire <- dat_site %>% filter(site %in% sites_with_fire)

dat_site_fire %>%
  ggplot() +
  geom_point(aes(dNBR,mass_burnt_pct,color=fire)) +
  coord_flip() +
  theme_bw()

dat_site_fire_long <- dat_site_fire %>%
  pivot_longer(cols = 5:10,
               values_to = "val",
               names_to = "var")

dat_site_fire_long %>%
  ggplot() +
  geom_point(aes(val,dNBR)) +
  geom_smooth(aes(val,dNBR), method="lm") +
  facet_wrap(.~var, scales="free_x") +
  theme_bw()

dat_site_fire_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_pct)) +
  geom_smooth(aes(val,high_sev_pct), method="lm") +
  facet_wrap(.~var, scales="free_x") +
  scale_y_continuous(limits = c(0,100)) +
  theme_bw()

## only cells with slope > 23 deg

dat_steep <- dat %>% filter(slope >= 23)
dat_steep_site <- dat_steep %>%
  group_by(site, fire) %>%
  summarize(dNBR = mean(dNBR),
            high_sev_pct = mean(severity%in%c("moderate","high")*100),
            mass_burnt_pct = mean(mass_burnt_pct),
            surface_consumption = sum(surface_consumption),
            canopy_consumption = sum(canopy_consumption),
            max_power = mean(max_power),
            residence_time_power = mean(residence_time_power),
            residence_time_consumption = mean(residence_time_consumption))

dat_steep_fire <- dat_steep %>% filter(site %in% sites_with_fire)
dat_steep_site_fire <- dat_steep_site %>% filter(site %in% sites_with_fire)

dat_steep_site_fire_long <- dat_steep_site_fire %>%
  pivot_longer(cols = 5:10,
               values_to = "val",
               names_to = "var") %>%
  mutate(var = factor(var, 
                      levels = c("mass_burnt_pct",
                                 "surface_consumption",
                                 "canopy_consumption",
                                 "max_power",
                                 "residence_time_power",
                                 "residence_time_consumption"),
                      labels = c("Percent Mass Burnt",
                                 "Total Surface\nConsumption (kg/m^3)",
                                 "Total Canopy\nConsumption (kg/m^3)",
                                 "Average Max Power (W/m^3)",
                                 "Average Residence Time (s)\n- from power",
                                 "Average Residence Time (s)\n- from consumption")))

dat_steep_site_fire_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_pct,color=fire)) +
  geom_smooth(aes(val,high_sev_pct), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x="",
       y="Percent Burned at Moderate-to-High Severity",
       title = "Slope > 23 degrees",
       color = "Focal Fire") +
  theme_bw()

