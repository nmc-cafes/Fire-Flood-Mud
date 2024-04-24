library(tidyverse)
library(here)
library(GGally)
library(terra)
library(ggthemes)

dat <- read.csv(here("all_data_duet.csv")) %>%
  mutate(severity = case_when(severity==1 ~ "unburned",
                              severity==2 ~ "low",
                              severity==3 ~ "moderate",
                              severity==4 ~ "high")) %>%
  group_by(fire) %>%
  mutate(dNBR_scaled = scale(dNBR)) %>%
  ungroup()

# explore!

dat_site <- dat %>%
  group_by(site, fire) %>%
  summarize(dNBR = mean(dNBR),
            dNBR_scaled = mean(dNBR_scaled),
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
  pivot_longer(cols = 6:11,
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

dNBR_scaled <- dat_site_fire_long %>%
  ggplot() +
  geom_point(aes(val,dNBR_scaled,color=fire)) +
  geom_smooth(aes(val,dNBR_scaled), method="lm", color = "black") +
  facet_wrap(.~var, scales="free_x") +
  scale_color_colorblind() +
  labs(x="",
       y="Scaled dNBR",
       color = "Focal Fire") +
  theme_bw()
dNBR_scaled

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
library(lmerTest)
library(MuMIn)
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
AICc(mod_mixed)

# mass_burnt should not be used with either of the consumption variables
# only one residence time should be used

f1 <- dNBR ~ mass_burnt_pct + max_power + residence_time_power + (1 | fire)
f2 <- dNBR ~ mass_burnt_pct + max_power + residence_time_consumption + (1 | fire)
f3 <- dNBR ~ surface_consumption + canopy_consumption + max_power + residence_time_power + (1 | fire)
f4 <- dNBR ~ surface_consumption + canopy_consumption + max_power + residence_time_consumption + (1 | fire)
f5 <- dNBR ~ surface_consumption + max_power + residence_time_power + (1 | fire)
f6 <- dNBR ~ surface_consumption + max_power + residence_time_consumption + (1 | fire)
f7 <- dNBR ~ canopy_consumption + max_power + residence_time_power + (1 | fire)
f8 <- dNBR ~ canopy_consumption + max_power + residence_time_consumption + (1 | fire)
f9 <- dNBR ~ mass_burnt_pct + residence_time_power + (1 | fire)
f10 <- dNBR ~ mass_burnt_pct + residence_time_consumption + (1 | fire)
f11 <- dNBR ~ surface_consumption + canopy_consumption + residence_time_power + (1 | fire)
f12 <- dNBR ~ surface_consumption + canopy_consumption + residence_time_consumption + (1 | fire)
f13 <- dNBR ~ surface_consumption + residence_time_power + (1 | fire)
f14 <- dNBR ~ surface_consumption + residence_time_consumption + (1 | fire)
f15 <- dNBR ~ canopy_consumption + residence_time_power + (1 | fire)
f16 <- dNBR ~ canopy_consumption + residence_time_consumption + (1 | fire)
f17 <- dNBR ~ max_power + residence_time_power + (1 | fire)
f18 <- dNBR ~ max_power + residence_time_consumption + (1 | fire)
f19 <- dNBR ~ mass_burnt_pct + max_power + (1 | fire)
f20 <- dNBR ~ surface_consumption + canopy_consumption + max_power + (1 | fire)
f21 <- dNBR ~ surface_consumption + max_power + (1 | fire)
f22 <- dNBR ~ canopy_consumption + max_power + (1 | fire)
f23 <- dNBR ~ mass_burnt_pct + (1 | fire)
f24 <- dNBR ~ surface_consumption + canopy_consumption + (1 | fire)
f25 <- dNBR ~ surface_consumption + (1 | fire)
f26 <- dNBR ~ canopy_consumption + (1 | fire)
f26 <- dNBR ~ max_power + (1 | fire)
f27 <- dNBR ~ residence_time_power + (1 | fire)
f28 <- dNBR ~ residence_time_consumption + (1 | fire)

formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,
              f24,f25,f26,f27,f28)
aic_df <- tibble("model" = seq(1,length(formulae)), "AICc" = rep(NA, length(formulae)))
for(i in 1:length(formulae)){
  mod <- lmer(formulae[i][[1]], data = dat_site_fire)
  aicc <- AICc(mod)
  aic_df$AICc[i] <- aicc
}

ggplot(aic_df, aes(model,AICc)) + geom_point()
selected <- paste0("f",aic_df[which.min(aic_df$AICc),"model"])

print(f24)
library(DHARMa)
final_model <- lmer(f24, data = dat_site_fire)
mod_sim <- simulateResiduals(final_model)
plot(mod_sim)

summary(final_model)
