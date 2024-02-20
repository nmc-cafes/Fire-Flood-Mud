library(tidyverse)
library(here)
library(GGally)
library(terra)

dat <- read.csv(here("all_data.csv"))
length(dat$mass_burnt_pct)
rst <- rast(nrow = 20, 
            ncol = 20,
            xmin = -150,
            xmax = 150,
            ymin = -150,
            ymax = 150,
            vals = dat$dNBR)
plot(rst)

# explore!

#surface vs canopy consumption
dat %>%
  ggplot() +
  geom_point(aes(surface_consumption,canopy_consumption)) +
  theme_bw()

#max power vs max reaction rate
dat %>%
  ggplot() +
  geom_point(aes(max_power, max_reaction_rate)) +
  theme_bw()

#residence times
dat %>%
  ggplot() +
  geom_point(aes(residence_time_power, residence_time_consumption)) +
  theme_bw()

#mass burnt vs max_power
dat %>%
  ggplot() +
  geom_point(aes(max_power, mass_burnt_pct)) +
  theme_bw()

## these all seem really correlated
ggpairs(dat, columns = names(dat)[2:8])

# compare stuff to dnbr
dat %>%
  pivot_longer(cols = 2:8,
               names_to = "var",
               values_to = "val") %>%
  ggplot() +
  geom_point(aes(dNBR,val)) +
  geom_smooth(aes(dNBR,val), method = "lm") +
  facet_wrap(.~var, scales = "free_y") +
  theme_bw()

