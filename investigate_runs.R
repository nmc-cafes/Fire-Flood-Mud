library(here)
library(tidyverse)
library(patchwork)
library(scales)
library(ggthemes)

consump <- read_csv(here("consump_by_z.csv"))

consump_summ <- consump %>%
  group_by(Height) %>%
  summarize(median_init = median(Initial),
            median_final = median(Final),
            q3_init = quantile(Initial, probs = 0.75),
            q3_final = quantile(Final, probs = 0.75),
            q1_init = quantile(Initial, probs = 0.25),
            q1_final = quantile(Final, probs = 0.25)) %>%
  pivot_longer(cols = -Height, 
               names_to = c(".value", "Stage"), 
               names_sep = "_")

consump_summ %>%
  ggplot() +
  geom_line(aes(Height, median, color = Stage)) +
  geom_ribbon(aes(Height, ymin=q1, ymax=q3, fill=Stage), alpha=0.5) +
  theme_bw()

consump_summ2 <- consump %>%
  mutate(consumption = Initial - Final,
         consumption_pct = (Initial - Final)/Initial) %>%
  group_by(Fire, Height) %>%
  summarize(consump_mean = mean(consumption, na.rm=T),
            consump_pct_mean = mean(consumption_pct, na.rm=T),
            Initial = mean(Initial, na.rm=T))

init_fuel <- consump_summ2 %>%
  ggplot() +
  geom_line(aes(Height, Initial, color = Fire)) +
  scale_color_colorblind() +
  labs(x="Height (m)",
       y="Initial Fuel Loading (kg)") +
  coord_flip() +
  theme_bw()
fuel_consump <- consump_summ2 %>%
  ggplot() +
  geom_line(aes(Height, consump_mean, color=Fire)) +
  scale_color_colorblind() +
  labs(x="Height (m)",
       y="Fuel Consumption (kg)") +
  # scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip() +
  theme_bw()

init_fuel + fuel_consump + plot_layout(axes = "collect", axis_titles = "collect", guides = "collect")
ggsave("canopy_consumption_by_height.jpg", path = here("Plots"), width = 6, height = 3)


