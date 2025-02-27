library(here)
library(tidyverse)
library(skimr)
library(ggthemes)
library(GGally)
library(scales)

corr_upper <- function(data, mapping, method="p", use="pairwise", ...){
  
  # grab data
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  # calculate correlation
  corr <- cor(x, y, method=method, use=use)
  
  # calculate colour based on correlation value
  # Here I have set a correlation of minus one to blue, 
  # zero to white, and one to red 
  # Change this to suit: possibly extend to add as an argument of `my_fn`
  colFn <- colorRampPalette(c("blue", "white", "red"), interpolate ='spline')
  fill <- colFn(100)[findInterval(corr, seq(-1, 1, length=100))]
  
  ggally_cor(data = data, mapping = mapping, color="black", ...) + 
    theme_void() +
    theme(panel.background = element_rect(fill=fill))
}

corr_lower <- function(data, mapping){
  ggally_points(data = data, mapping = mapping, shape = 1, alpha = 0.5) +
    scale_y_continuous(n.breaks = 3) +
    theme_bw()
}

dat_raw <- read_csv(here("QF_results","qf_results.csv"))
skim_without_charts(dat_raw)

dat_ifd <- dat_raw %>%
  mutate(canopy_rhof_init = canopy_consumption_tot/canopy_consumption_pct,
         surface_rhof_init = surface_consumption_tot/surface_consumption_pct) %>%
  mutate(total_rhof_init = canopy_rhof_init + surface_rhof_init)

dat_site <- dat_ifd %>%
  group_by(site, fire) %>%
  summarize(severity_pct = sum(severity %in% c(3,4) & steep==1) / sum(severity > 0),
            dNBR_mean = mean(dNBR),
            canopy_consumption_pct_mean = mean(canopy_consumption_pct)*100,
            canopy_consumption_tot_sum = sum(canopy_consumption_tot),
            canopy_residence_time_mean = mean(canopy_residence_time),
            energy_flux_mean = mean(energy_flux),
            mass_burnt_pct_mean = mean(mass_burnt_pct),
            max_power_mean = mean(max_power),
            surface_consumption_pct_mean = mean(surface_consumption_pct)*100,
            surface_consumption_tot_sum = sum(surface_consumption_tot),
            surface_residence_time_mean = mean(surface_residence_time),
            total_power_sum = sum(total_power),
            canopy_rhof_init_sum = sum(canopy_rhof_init, na.rm=T),
            surface_rhof_init_sum = sum(surface_rhof_init, na.rm=T),
            total_rhof_init_sum = sum(total_rhof_init, na.rm=T)) %>%
  mutate(canopy_residence_time_mean = canopy_residence_time_mean/60, #canopy res time to min
         canopy_consumption_tot_sum = canopy_consumption_tot_sum/1000, #kilograms to megagrams
         surface_consumption_tot_sum = surface_consumption_tot_sum/1000,
         total_power_sum = total_power_sum/1000000)

write.csv(dat_site, here("QF_results","qf_results_site.csv"), row.names = F)

dat_site_pairs <- dat_site
names(dat_site_pairs)[3:14] <- c("Percent Burned at\nModerate to High Severity\non Steep Slopes",
                                 "dNBR",
                                 "Total Canopy\nConsumption (%)",
                                 "Total Canopy\nConsumption (Mg/m3)",
                                 "Average Canopy\nResidence Time (min)",
                                 "Average Power (kW)",
                                 "Total Consumption (%)",
                                 "Average Max\nPower (kW/m3)",
                                 "Total Surface\nConsumption (%)",
                                 "Total Surface\nConsumption (Mg/m3)",
                                 "Average Surface\nResidence Time (s)",
                                 "Total Energy (GW)"
                                 )

corr_plot <- ggpairs(dat_site_pairs, 
        columns = c(3,5:14), 
        upper = list(continuous = corr_upper),
        lower = list(continuous = corr_lower)) +
  theme(strip.text.y = element_text(angle=0),
        strip.text.x = element_text(angle=90),
        axis.text.x = element_text(angle=90, vjust = 0.5, hjust = 1))

ggsave("correlation_matrix_responses.png", plot = corr_plot, path = here("Plots"), width = 9, height=7.5)

ggpairs(dat_site_pairs[3:4])

########
dat_long <- dat_site %>%
  pivot_longer(cols = 5:14,
               values_to = "val",
               names_to = "var") %>%
  mutate(var = factor(var, 
                      levels = c("surface_consumption_tot_sum",
                                 "surface_consumption_pct_mean",
                                 "canopy_consumption_tot_sum",
                                 "canopy_consumption_pct_mean",
                                 "mass_burnt_pct_mean",
                                 "surface_residence_time_mean",
                                 "canopy_residence_time_mean",
                                 "max_power_mean",
                                 "total_power_sum",
                                 "energy_flux_mean"),
                      labels = c("Total Surface\nConsumption (Mg/m3)",
                                 "Total Surface\nConsumption (%)",
                                 "Total Canopy\nConsumption (Mg/m3)",
                                 "Total Canopy\nConsumption (%)",
                                 "Total Consumption (%)",
                                 "Average Surface\nResidence Time (s)",
                                 "Average Canopy\nResidence Time (min)",
                                 "Average Max\nPower (kW/m3)",
                                 "Total Power (kW)",
                                 "Average Power (kW)")))

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
ss_scatter <- dat_long %>%
  filter(var != "Total Power (kW)") %>%
  ggplot() +
  geom_point(aes(val,severity_pct,color=fire)) +
  geom_smooth(aes(val,severity_pct), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x", nrow = 2) +
  scale_color_colorblind() +
  scale_y_continuous(labels = percent_format()) +
  labs(x="",
       y="Percent Burned at\nModerate to High Severity on Steep Slopes",
       color = "Focal Fire") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.2))
ss_scatter
ggsave("severe_steep_scatters.png",ss_scatter,path=here("Plots"),width=10, height=4)

dnbr_scatter <- dat_long %>%
  filter(var != "Total Power (kW)") %>%
  ggplot() +
  geom_point(aes(val,dNBR_mean,color=fire)) +
  geom_smooth(aes(val,dNBR_mean), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x", nrow = 2) +
  scale_color_colorblind() +
  labs(x="",
       y="Average dNBR",
       color = "Focal Fire") +
  theme_bw() +
  theme(legend.position = "inside",
        legend.position.inside = c(0.9, 0.2))
dnbr_scatter
ggsave("dNBR_scatters.png",dnbr_scatter,path=here("Plots"),width=10, height=4)

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


########

dat_site %>%
  ggplot() +
  geom_point(aes(surface_rhof_init_sum, severity_pct, color = fire)) +
  geom_smooth(aes(surface_rhof_init_sum, severity_pct), method = "lm") +
  theme_bw()
  

