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
  summarize(dNBR = mean(dNBR),
            dNBR_scaled = mean(dNBR_scaled, na.rm=T),
            high_sev_pct = mean(severity%in%c("high")*100),
            mass_burnt_pct = mean(mass_burnt_pct, na.rm=T),
            surface_consumption_pct = mean(surface_consumption_pct,na.rm=T)*100,
            surface_consumption = mean(surface_consumption,na.rm=T),
            canopy_consumption = mean(canopy_consumption,na.rm=T)*100,
            max_power = mean(max_power,na.rm=T),
            residence_time_power = mean(residence_time_power, na.rm=T))

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

dNBR_scaled <- dat_site_long %>%
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


#################
## Try some preliminary model selection

library(leaps)

dat_scaled <- dat_site
dat_scaled[8:13] <- as.data.frame(scale(dat_scaled[8:13]))

# dNBR
best_subset <- regsubsets(dNBR_scaled ~ 
                            mass_burnt_pct + 
                            surface_consumption +
                            surface_consumption_pct +
                            canopy_consumption +
                            max_power + 
                            residence_time_power +
                            residence_time_consumption, 
                          dat_scaled)
summary(best_subset)

# create training - testing data
set.seed(47)
sample <- sample(c(TRUE, FALSE), nrow(dat_scaled), replace = T, prob = c(0.6,0.4))
train <- dat_scaled[sample, ]
test <- dat_scaled[!sample, ]

# perform best subset selection
best_subset <- regsubsets(dNBR_scaled ~ 
                            mass_burnt_pct + 
                            surface_consumption +
                            surface_consumption_pct +
                            canopy_consumption +
                            max_power + 
                            residence_time_power +
                            residence_time_consumption,
                          train)
results <- summary(best_subset)

# extract and plot results
tibble(predictors = 1:7,
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
test_m <- model.matrix(dNBR_scaled ~ 
                         mass_burnt_pct + 
                         surface_consumption +
                         surface_consumption_pct +
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
folds <- sample(1:k, nrow(dat_scaled), replace = TRUE)
cv_errors <- matrix(NA, k, 6, dimnames = list(NULL, paste(1:6)))

for(j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset_j <- regsubsets(dNBR_scaled ~ 
                                mass_burnt_pct + 
                                surface_consumption +
                                surface_consumption_pct +
                                canopy_consumption +
                                max_power + 
                                residence_time_power +
                                residence_time_consumption,
                            dat_scaled[folds != j, ], nvmax = 6)
  
  # perform cross-validation
  for(i in 1:6) {
    pred_x <- predict_regsubsets(best_subset_j, dat_scaled[folds == j, ], id = i)
    cv_errors[j, i] <- mean((dat_scaled$dNBR[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors)
plot(mean_cv_errors, type = "b")

# find final model
final_best <- regsubsets(dNBR_scaled ~ 
                           mass_burnt_pct + 
                           surface_consumption +
                           surface_consumption_pct +
                           canopy_consumption +
                           max_power + 
                           residence_time_power +
                           residence_time_consumption,
                         data = dat_site,
                         nvmax = 6)
coef(final_best, which.min(mean_cv_errors))


# what is the r-squared, rmse?
final_mod <- lm(dNBR_scaled ~ mass_burnt_pct + max_power, data=dat_scaled)
summary(final_mod)

#############

# try a mixed model?
library(lmerTest)
library(MuMIn)
library(DHARMa)

dat_homo <- dat_scaled %>% filter(homogeneity_class == "homogeneous")
dat_hetero <- dat_scaled %>% filter(homogeneity_class == "heterogeneous")
dat_high <- dat_scaled %>% filter(severity_class == "high")
dat_mod <- dat_scaled %>% filter(severity_class == "moderate")
dat_low <- dat_scaled %>% filter(severity_class == "low")

mod_mixed <- lmer(dNBR_scaled ~ 
                    mass_burnt_pct + 
                    surface_consumption +
                    surface_consumption_pct +
                    canopy_consumption +
                    max_power + 
                    residence_time_power +
                    (1 | fire),
                  data=dat_scaled)
summary(mod_mixed)
AICc(mod_mixed)

# mass_burnt should not be used with either of the percent consumption variables
# only one of surface consumption and surface consumption pct should be used
# only one residence time should be used

f1 <- dNBR_scaled ~ mass_burnt_pct + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f2 <- dNBR_scaled ~ surface_consumption_pct + canopy_consumption + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f3 <- dNBR_scaled ~ surface_consumption_pct + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f4 <- dNBR_scaled ~ canopy_consumption + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f5 <- dNBR_scaled ~ mass_burnt_pct + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f6 <- dNBR_scaled ~ surface_consumption_pct + canopy_consumption + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f7 <- dNBR_scaled ~ surface_consumption_pct + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f8 <- dNBR_scaled ~ canopy_consumption + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f9 <- dNBR_scaled ~ max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f10 <- dNBR_scaled ~ mass_burnt_pct + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f11 <- dNBR_scaled ~ surface_consumption_pct + canopy_consumption + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f12 <- dNBR_scaled ~ surface_consumption_pct + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f13 <- dNBR_scaled ~ canopy_consumption + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f14 <- dNBR_scaled ~ mass_burnt_pct + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f15 <- dNBR_scaled ~ surface_consumption_pct + canopy_consumption + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f16 <- dNBR_scaled ~ surface_consumption_pct + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f17 <- dNBR_scaled ~ canopy_consumption + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f18 <- dNBR_scaled ~ max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f19 <- dNBR_scaled ~ residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)

f20 <- dNBR_scaled ~ surface_consumption + canopy_consumption + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f21 <- dNBR_scaled ~ surface_consumption + max_power + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f22 <- dNBR_scaled ~ surface_consumption + canopy_consumption + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f23 <- dNBR_scaled ~ surface_consumption + residence_time_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f24 <- dNBR_scaled ~ surface_consumption + canopy_consumption + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f25 <- dNBR_scaled ~ surface_consumption + max_power + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f26 <- dNBR_scaled ~ surface_consumption + canopy_consumption + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)
f27 <- dNBR_scaled ~ surface_consumption + (1 | fire) + (1 | severity_class) + (1 | homogeneity_class)

formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,
              f24,f25,f26,f27)
aic_df <- tibble("model" = seq(1,length(formulae)), "AICc" = rep(NA, length(formulae)))
for(i in 1:length(formulae)){
  mod <- lmer(formulae[i][[1]], data = dat_scaled)
  aicc <- AICc(mod)
  aic_df$AICc[i] <- aicc
}

ggplot(aic_df, aes(model,AICc)) + geom_point()
selected <- paste0("f",aic_df[which.min(aic_df$AICc),"model"])
print(selected)
print(f17)
final_model <- lmer(f17, data = dat_scaled)
mod_sim <- simulateResiduals(final_model)
plot(mod_sim)

summary(final_model)
