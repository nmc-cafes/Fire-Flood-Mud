library(tidyverse)
library(here)
library(GGally)
library(terra)

dat <- read.csv(here("all_data.csv")) %>%
  mutate(severity = case_when(severity==1 ~ "unburned",
                              severity==2 ~ "low",
                              severity==3 ~ "moderate",
                              severity==4 ~ "high"))

# explore!

dat %>%
  ggplot() + 
  geom_bar(aes(x=site,y=mass_burnt_pct),stat="identity") +
  facet_wrap(.~fire, scales="free") +
  coord_flip() +
  labs(y="Mass Burnt (%)")
  theme_bw() +
  theme(axis.title.x = element_blank())

