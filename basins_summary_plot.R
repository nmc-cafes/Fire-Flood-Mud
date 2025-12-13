library(here)
library(tidyverse)
library(patchwork)
library(ggthemes)
library(scales)

dat_site <- read.csv(here("QF_results","SBS","qf_results_site_corrected.csv"))

dat_all <- dat_site %>%
  mutate(fire = "All")

dat <- bind_rows(dat_site, dat_all) %>%
  mutate(fire = factor(fire,
                       levels = c("All",
                                  "CubCreek2",
                                  "CedarCreek",
                                  "KNP",
                                  "Dixie",
                                  "Caldor"),
                       labels = c("All",
                                  "Cub Creek 2",
                                  "Cedar Creek",
                                  "KNP Complex",
                                  "Dixie",
                                  "Caldor")))

dnbr <- dat %>%
  ggplot() +
  geom_jitter(aes(fire, dNBR_mean, color=fire),
              alpha = 0.33,
              width = 0.2) +
  geom_boxplot(aes(fire, dNBR_mean, color=fire),
               outliers = F,
               fill = NA) +
  scale_color_manual(values = c("gray50",colorblind_pal()(8)[c(3,2)],"yellow3",colorblind_pal()(8)[c(4,1)])) +
  labs(x="Fire",
       y="Mean Basin dNBR") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
dnbr

ss <- dat %>%
  ggplot() +
  geom_jitter(aes(fire, severity_pct, color=fire),
              alpha = 0.33,
              width = 0.2) +
  geom_boxplot(aes(fire, severity_pct, color=fire),
               outliers = F,
               fill = NA) +
  geom_hline(aes(yintercept = 0.25),
             linetype = "dashed",
             color = "black") +
  scale_color_manual(values = c("gray50",colorblind_pal()(8)[c(3,2)],"yellow3",colorblind_pal()(8)[c(4,1)])) +
  scale_y_continuous(labels = percent_format()) +
  labs(x="Fire",
       y="Steep-Severe Percent") +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "none")
ss

responses <- dnbr + ss + plot_layout(axes = "collect")
ggsave("responses_summary.jpg", responses, path = here("Plots"), width = 10, height = 3)


summary(dat_site$dNBR_mean)
summary(dat_site$severity_pct)
