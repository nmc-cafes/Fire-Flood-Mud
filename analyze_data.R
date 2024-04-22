library(tidyverse)
library(here)
library(GGally)
library(terra)
library(ggthemes)

dat <- read.csv(here("all_data_duet.csv")) %>%
  mutate(severity = case_when(severity==1 ~ "unburned",
                              severity==2 ~ "low",
                              severity==3 ~ "moderate",
                              severity==4 ~ "high"))

# explore!

dat_site <- dat %>%
  group_by(site, fire) %>%
  summarize(dNBR = mean(dNBR),
            high_sev_pct = mean(severity%in%c("moderate","high")*100),
            mass_burnt_pct = mean(mass_burnt_pct),
            surface_consumption = mean(surface_consumption,na.rm=T)*100,
            canopy_consumption = mean(canopy_consumption,na.rm=T)*100,
            max_power = mean(max_power),
            residence_time_power = mean(residence_time_power),
            residence_time_consumption = mean(residence_time_consumption))

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
ggsave(here("Plots","mburnt_by_site_duet.jpg"), mburnt_site, height = 12, width = 18, units = "cm")

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
               names_to = "var") %>%
  mutate(var = factor(var, 
                      levels = c("mass_burnt_pct",
                                 "surface_consumption",
                                 "canopy_consumption",
                                 "max_power",
                                 "residence_time_power",
                                 "residence_time_consumption"),
                      labels = c("Percent Mass Burnt",
                                 "Total Surface\nConsumption (%)",
                                 "Total Canopy\nConsumption (%)",
                                 "Average Max Power (W/m^3)",
                                 "Average Residence Time (s)\n- from power",
                                 "Average Residence Time (s)\n- from consumption")))

dNBR <- dat_site_fire_long %>%
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

ggsave(here("Plots","avg_dNBR_duet.jpg"), dNBR, height = 12, width = 18, units = "cm")

## only cells with slope > 23 deg

dat_steep <- dat %>% filter(slope >= 23)
dat_steep_site <- dat_steep %>%
  group_by(site, fire) %>%
  summarize(dNBR = mean(dNBR),
            high_sev_pct = mean(severity%in%c("moderate","high")*100),
            mass_burnt_pct = mean(mass_burnt_pct),
            surface_consumption = mean(surface_consumption)*100,
            canopy_consumption = mean(canopy_consumption)*100,
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
                                 "Total Surface\nConsumption (%)",
                                 "Total Canopy\nConsumption (%)",
                                 "Average Max Power (W/m^3)",
                                 "Average Residence Time (s)\n- from power",
                                 "Average Residence Time (s)\n- from consumption")))

steep_high_sev <- dat_steep_site_fire_long %>%
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
steep_high_sev
ggsave(here("Plots","severe_steep_duet.jpg"), steep_high_sev, height = 12, width = 18, units = "cm")

high_sev <- dat_site_fire_long %>%
  ggplot() +
  geom_point(aes(val,high_sev_pct,color=fire)) +
  geom_smooth(aes(val,high_sev_pct), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  scale_y_continuous(limits = c(0,100)) +
  labs(x="",
       y="Percent Burned at Moderate-to-High Severity",
       color = "Focal Fire") +
  theme_bw()
high_sev

#################
## Try some preliminary model selection

library(leaps)

# dNBR
best_subset <- regsubsets(dNBR ~ 
                            mass_burnt_pct + 
                            surface_consumption +
                            canopy_consumption +
                            max_power + 
                            residence_time_power +
                            residence_time_consumption, 
                          dat_site_fire) # 11 linear dependencies
summary(best_subset)

# create training - testing data
set.seed(47)
sample <- sample(c(TRUE, FALSE), nrow(dat_site_fire), replace = T, prob = c(0.6,0.4))
train <- dat_site_fire[sample, ]
test <- dat_site_fire[!sample, ]

# perform best subset selection
best_subset <- regsubsets(dNBR ~ 
                            mass_burnt_pct + 
                            surface_consumption +
                            canopy_consumption +
                            max_power + 
                            residence_time_power +
                            residence_time_consumption, 
                          train) # 11 linear dependencies
results <- summary(best_subset)

# extract and plot results
tibble(predictors = 1:6,
       adj_R2 = results$adjr2,
       Cp = results$cp,
       BIC = results$bic) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F) +
  facet_wrap(~ statistic, scales = "free") +
  scale_color_colorblind() +
  labs(x="Number of predictors",
       y="") +
  theme_bw()

which.max(results$adjr2)
which.min(results$bic)
which.min(results$cp)

# direct testing
test_m <- model.matrix(dNBR ~ 
                         mass_burnt_pct + 
                         surface_consumption +
                         canopy_consumption +
                         max_power + 
                         residence_time_power +
                         residence_time_consumption,
                       test)

# create empty vector to fill with error values
validation_errors <- vector("double", length = 6)

for(i in 1:6) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$dNBR - pred_x)^2)  # compute test error btwn actual & predicted salary
}

# plot validation errors
plot(validation_errors, type = "b")

# cross-validation
predict_regsubsets <- function(object, newdata, id ,...) {
  form <- as.formula(object$call[[2]]) 
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
}

# create matrix to store results
k <- 10
set.seed(4747)
folds <- sample(1:k, nrow(dat_site_fire), replace = TRUE)
cv_errors <- matrix(NA, k, 6, dimnames = list(NULL, paste(1:6)))

for(j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset_j <- regsubsets(dNBR ~ 
                              mass_burnt_pct + 
                              surface_consumption +
                              canopy_consumption +
                              max_power + 
                              residence_time_power +
                              residence_time_consumption, 
                            dat_site_fire[folds != j, ], nvmax = 6)
  
  # perform cross-validation
  for(i in 1:6) {
    pred_x <- predict_regsubsets(best_subset_j, dat_site_fire[folds == j, ], id = i)
    cv_errors[j, i] <- mean((dat_site_fire$dNBR[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors)
plot(mean_cv_errors, type = "b")

# find final model
final_best <- regsubsets(dNBR ~ 
                           mass_burnt_pct + 
                           surface_consumption +
                           canopy_consumption +
                           max_power + 
                           residence_time_power +
                           residence_time_consumption,
                         data = dat_site_fire,
                         nvmax = 6)
coef(final_best, which.min(mean_cv_errors))


# what is the r-squared, rmse?
final_mod <- lm(dNBR ~ residence_time_consumption, data=dat_site_fire)
summary(final_mod)

# tray a mixed model?
library(lme4)
mod_mixed <- lmer(dNBR ~ 
                    mass_burnt_pct + 
                    surface_consumption +
                    canopy_consumption +
                    max_power + 
                    residence_time_power +
                    residence_time_consumption +
                    (1 | fire),
                  data=dat_site_fire)
summary(mod_mixed)
