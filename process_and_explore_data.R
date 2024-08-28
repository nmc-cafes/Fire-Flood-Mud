library(tidyverse)
library(here)
library(GGally)
library(terra)
library(ggthemes)

dat <- read.csv(here("all_data_expandedsampling.csv")) %>%
  mutate(severity = case_when(severity==1 ~ "unburned",
                              severity==2 ~ "low",
                              severity==3 ~ "moderate",
                              severity==4 ~ "high")) %>%
  group_by(fire) %>%
  mutate(dNBR_scaled = scale(dNBR)) %>%
  ungroup() %>%
  select(-residence_time_consumption) #not calculated correctly

# explore!

dat_site <- dat %>%
  group_by(site, severity_class, homogeneity_class, fire) %>%
  mutate(steep_severe = if_else((severity%in%c("high","moderate") & slope>23), 1, 0)) %>%
  summarize(dNBR = mean(dNBR),
            dNBR_scaled = mean(dNBR_scaled, na.rm=T),
            high_sev_pct = mean(severity%in%c("high","moderate")*100),
            mass_burnt_pct = mean(mass_burnt_pct, na.rm=T),
            surface_consumption_pct = mean(surface_consumption_pct,na.rm=T)*100,
            surface_consumption = sum(surface_consumption,na.rm=T),
            canopy_consumption = mean(canopy_consumption,na.rm=T)*100,
            max_power = mean(max_power,na.rm=T),
            residence_time_power = mean(residence_time_power, na.rm=T),
            high_sev_steep = mean(steep_severe)*100) %>%
  mutate(high_sev_bin = if_else(high_sev_pct>25,1,0)) %>%
  mutate(high_sev_bin = factor(high_sev_bin))

write.csv(dat_site, here("all_data_expandedsampling_site.csv"), row.names = F)

mburnt_site <- dat_site %>%
  ggplot() +
  geom_bar(stat = "identity", aes(site,mass_burnt_pct,fill=fire)) +
  facet_wrap(.~fire, scales = "free_y") +
  scale_fill_colorblind()+
  labs(x="",
       y="Mass Burnt (%)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
mburnt_site
ggsave(here("Plots","mburnt_by_site_ES.jpg"), mburnt_site, height = 12, width = 18, units = "cm")

canopy_cons_site <- dat_site %>%
  ggplot() +
  geom_bar(stat = "identity", aes(site,canopy_consumption,fill=fire)) +
  facet_wrap(.~fire, scales = "free_y") +
  scale_fill_colorblind()+
  labs(x="",
       y="Canopy Consumption (%)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
canopy_cons_site

dat_site %>%
  ggplot() +
  geom_point(aes(dNBR,mass_burnt_pct,color=fire)) +
  coord_flip() +
  theme_bw()

dat_site_long <- dat_site %>%
  pivot_longer(cols = 8:13,
               values_to = "val",
               names_to = "var") %>%
  mutate(var = factor(var, 
                      levels = c("mass_burnt_pct",
                                 "surface_consumption",
                                 "surface_consumption_pct",
                                 "canopy_consumption",
                                 "max_power",
                                 "residence_time_power",
                                 "residence_time_consumption"),
                      labels = c("Percent Mass Burnt",
                                 "Total Surface\nConsumption (kg/m3)",
                                 "Total Surface\nConsumption (%)",
                                 "Total Canopy\nConsumption (%)",
                                 "Average Max Power (W/m^3)",
                                 "Average Residence Time (s)\n- from power",
                                 "Average Residence Time (s)\n- from consumption")))

binary_severity <- dat_site_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_bin, color=fire)) +
  geom_smooth(aes(val, high_sev_bin), method = "glm", 
              method.args = list(family = "binomial"), 
              se = FALSE) +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="High Severity (0/1)") +
  theme_bw()
binary_severity

dNBR_scaled <- dat_site_long %>%
  ggplot() +
  geom_point(aes(val,dNBR_scaled,color=fire)) +
  geom_smooth(aes(val,dNBR_scaled), method="gam", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="Scaled dNBR",
       color = "Focal Fire") +
  theme_bw()
dNBR_scaled

dNBR <- dat_site_long %>%
  ggplot() +
  geom_point(aes(val,dNBR,color=fire)) +
  geom_smooth(aes(val,dNBR), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="dNBR",
       color = "Focal Fire") +
  theme_bw()
dNBR

ggsave(here("Plots","avg_dNBR_ES.jpg"), dNBR, height = 12, width = 18, units = "cm")

high_sev <- dat_site_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_pct,color=fire)) +
  geom_smooth(aes(val,high_sev_pct), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x="",
       y="Percent Burned at High Severity",
       color = "Focal Fire") +
  theme_bw()
high_sev
ggsave(here("Plots","percent_highseverity_ES.jpg"), steep_high_sev, height = 12, width = 18, units = "cm")

high_sev_steep <- dat_site_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_steep,color=fire)) +
  geom_smooth(aes(val,high_sev_steep), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x="",
       y="Percent Burned at High Severity\non Steep Slopes",
       color = "Focal Fire") +
  theme_bw()
high_sev_steep

## log-log
dNBR_loglog <- dat_site_long %>%
  mutate(val = log(val),
         dNBR = log(dNBR)) %>%
  ggplot() +
  geom_point(aes(val,dNBR,color=fire)) +
  geom_smooth(aes(val,dNBR), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="dNBR",
       color = "Focal Fire") +
  theme_bw()
dNBR_loglog

ggsave(here("Plots","avg_dNBR_loglog.jpg"), dNBR_loglog, height = 12, width = 18, units = "cm")

hsp_loglog <- dat_site_long %>%
  mutate(val = log(val),
         dNBR = log(high_sev_pct)) %>%
  ggplot() +
  geom_point(aes(val,dNBR,color=fire)) +
  geom_smooth(aes(val,dNBR), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="Percent Burned at High Severity",
       color = "Focal Fire") +
  theme_bw()
hsp_loglog

