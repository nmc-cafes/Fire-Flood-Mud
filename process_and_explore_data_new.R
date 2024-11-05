library(here)
library(tidyverse)
library(skimr)
library(ggthemes)
library(GGally)

dat_raw <- read_csv(here("QF_results","qf_results.csv"))
skim_without_charts(dat_raw)

dat_site <- dat_raw %>%
  group_by(site, fire) %>%
  summarize(severity_pct = sum(severity %in% c(3,4) & steep==1) / sum(severity > 0),
            canopy_consumption_pct_mean = mean(canopy_consumption_pct)*100,
            canopy_consumption_tot_sum = sum(canopy_consumption_tot),
            canopy_residence_time_mean = mean(canopy_residence_time),
            energy_flux_mean = mean(energy_flux),
            mass_burnt_pct_mean = mean(mass_burnt_pct),
            max_power_mean = mean(max_power),
            surface_consumption_pct_mean = mean(surface_consumption_pct)*100,
            surface_consumption_tot_sum = sum(surface_consumption_tot),
            surface_residence_time_mean = mean(surface_residence_time),
            total_power_sum = sum(total_power))

ggpairs(dat_site[,4:13])

dat_long <- dat_site %>%
  pivot_longer(cols = 4:13,
               values_to = "val",
               names_to = "var") %>%
  mutate(var = factor(var, 
                      levels = c("mass_burnt_pct_mean",
                                 "surface_consumption_tot_sum",
                                 "surface_consumption_pct_mean",
                                 "canopy_consumption_tot_sum",
                                 "canopy_consumption_pct_mean",
                                 "surface_residence_time_mean",
                                 "canopy_residence_time_mean",
                                 "max_power_mean",
                                 "total_power_sum",
                                 "energy_flux_mean"),
                      labels = c("Percent Mass Burnt",
                                 "Total Surface\nConsumption (kg/m3)",
                                 "Total Surface\nConsumption (%)",
                                 "Total Canopy\nConsumption (kg/m3)",
                                 "Total Canopy\nConsumption (%)",
                                 "Average Surface\nResidence Time (s)",
                                 "Average Canopy\nResidence Time (s)",
                                 "Average Max Power (W/m^3)",
                                 "Total Power (W)",
                                 "Mean Energy Flux (W/m^3/s)")))

## Bar charts
mburnt_site <- dat_site %>%
  ggplot() +
  geom_bar(stat = "identity", aes(site,mass_burnt_pct_mean,fill=fire)) +
  facet_wrap(.~fire, scales = "free_y") +
  scale_fill_colorblind()+
  labs(x="",
       y="Mass Burnt (%)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
mburnt_site

canopy_cons_site <- dat_site %>%
  ggplot() +
  geom_bar(stat = "identity", aes(site,canopy_consumption_pct_mean,fill=fire)) +
  facet_wrap(.~fire, scales = "free_y") +
  scale_fill_colorblind()+
  labs(x="",
       y="Canopy Consumption (%)") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
canopy_cons_site


## Scatterplots
scatter <- dat_long %>%
  ggplot() +
  geom_point(aes(val,severity_pct,color=fire)) +
  geom_smooth(aes(val,severity_pct), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="Percent Burned at\nModerate to High Severity on Steep Slopes",
       color = "Focal Fire") +
  theme_bw()
scatter

## severity class boxplots
mtbs_colors <-c("#006400","#7fffd4","#ffff00","#ff0000","#7fff00")

massburnt_boxplot <- dat_raw %>%
  filter(mass_burnt_pct > 0,
         severity %in% c(1,2,3,4)) %>%
  mutate(severity = factor(severity, labels = c("Unburned",
                                                "Low Severity",
                                                "Moderate Severity",
                                                "High Severity"))) %>%
  ggplot() +
  geom_boxplot(aes(fire,mass_burnt_pct,fill=severity), outlier.shape = 1, outlier.alpha = 0.25) +
  scale_fill_manual(values = mtbs_colors) +
  labs(x="Focal Fire",
       y="Mass Burnt (%)",
       fill = "Severity") +
  theme_bw()
massburnt_boxplot

srt_boxplot <- dat_raw %>%
  filter(surface_residence_time > 0,
         severity %in% c(1,2,3,4)) %>%
  mutate(severity = factor(severity, labels = c("Unburned",
                                                "Low Severity",
                                                "Moderate Severity",
                                                "High Severity"))) %>%
  ggplot() +
  geom_boxplot(aes(fire,surface_residence_time,fill=severity), outlier.shape = 1, outlier.alpha = 0.05) +
  scale_fill_manual(values = mtbs_colors) +
  labs(x="Focal Fire",
       y="Surface Residence Time (s)",
       fill = "Severity") +
  theme_bw()
srt_boxplot

flux_boxplot <- dat_raw %>%
  filter(energy_flux > 0,
         severity %in% c(1,2,3,4)) %>%
  mutate(severity = factor(severity, labels = c("Unburned",
                                                "Low Severity",
                                                "Moderate Severity",
                                                "High Severity"))) %>%
  ggplot() +
  geom_boxplot(aes(fire,energy_flux,fill=severity), outlier.shape = 1, outlier.alpha = 0.05) +
  scale_fill_manual(values = mtbs_colors) +
  labs(x="Focal Fire",
       y="Energy Flux (W)",
       fill = "Severity") +
  theme_bw()
flux_boxplot

canopy_pct_boxplot <- dat_raw %>%
  filter(canopy_consumption_pct > 0,
         severity %in% c(1,2,3,4)) %>%
  mutate(severity = factor(severity, labels = c("Unburned",
                                                "Low Severity",
                                                "Moderate Severity",
                                                "High Severity"))) %>%
  ggplot() +
  geom_boxplot(aes(fire,canopy_consumption_pct,fill=severity), outlier.shape = 1, outlier.alpha = 0.05) +
  scale_fill_manual(values = mtbs_colors) +
  labs(x="Focal Fire",
       y="Canopy Consumption (%)",
       fill = "Severity") +
  theme_bw()
canopy_pct_boxplot
  
