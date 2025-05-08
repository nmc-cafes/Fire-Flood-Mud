library(here)
library(tidyverse)
library(ggthemes)
library(scales)
library(patchwork)
library(DHARMa)
library(glmmTMB)
library(performance)
library(effects)
library(rcompanion)
library(MuMIn)

## GLMM Model Selection
dat_site <- read.csv(here("QF_results","SBS","qf_results_site_corrected.csv"))

# beta family needs to be 0 < x < 1
dat_beta <- dat_site %>%
  mutate(severity_pct = case_when(severity_pct==0 ~ 0.001,
                                  severity_pct==1 ~ 0.999,
                                  TRUE ~ severity_pct))

# no need to use mass burnt since it is just surface + canopy consumption
# only one of consumption_tot and consumption_pct should be used

f1 <- severity_pct ~ canopy_consumption_pct_mean + (1|fire)
f2 <- severity_pct ~ surface_consumption_pct_mean + (1|fire)
f3 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + (1|fire)

f4 <- severity_pct ~ canopy_consumption_pct_mean + max_power_mean + (1|fire)
f5 <- severity_pct ~ surface_consumption_pct_mean + max_power_mean + (1|fire)
f6 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + max_power_mean + (1|fire)

f7 <- severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f8 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f9 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f10 <- severity_pct ~ canopy_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f11 <- severity_pct ~ surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f12 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f13 <- severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f14 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f15 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f16 <- severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f17 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f18 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f19 <- severity_pct ~ canopy_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f20 <- severity_pct ~ surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f21 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f22 <- severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f23 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f24 <- severity_pct ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

f26 <- severity_pct ~ max_power_mean + (1|fire)

f27 <- severity_pct ~ canopy_residence_time_mean + (1|fire)
f28 <- severity_pct ~ surface_residence_time_mean + (1|fire)
f29 <- severity_pct ~ canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f30 <- severity_pct ~ canopy_residence_time_mean + max_power_mean + (1|fire)
f31 <- severity_pct ~ surface_residence_time_mean + max_power_mean + (1|fire)
f32 <- severity_pct ~ canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

f33 <- severity_pct ~ canopy_consumption_tot_sum + (1|fire)
f34 <- severity_pct ~ surface_consumption_pct_mean + (1|fire)
f35 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + (1|fire)

f36 <- severity_pct ~ canopy_consumption_tot_sum + max_power_mean + (1|fire)
f37 <- severity_pct ~ surface_consumption_pct_mean + max_power_mean + (1|fire)
f38 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + max_power_mean + (1|fire)

f39 <- severity_pct ~ canopy_consumption_tot_sum + canopy_residence_time_mean + (1|fire)
f40 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f41 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f42 <- severity_pct ~ canopy_consumption_tot_sum + surface_residence_time_mean + (1|fire)
f43 <- severity_pct ~ surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f44 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f45 <- severity_pct ~ canopy_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f46 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f47 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f48 <- severity_pct ~ canopy_consumption_tot_sum + canopy_residence_time_mean + max_power_mean + (1|fire)
f49 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f50 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f51 <- severity_pct ~ canopy_consumption_tot_sum + surface_residence_time_mean + max_power_mean + (1|fire)
f52 <- severity_pct ~ surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f53 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f54 <- severity_pct ~ canopy_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f55 <- severity_pct ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f56 <- severity_pct ~ canopy_consumption_tot_sum + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,
              f21,f22,f23,f24,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,
              f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56)
aic_df <- tibble("model" = seq(1,length(formulae)), "AICc" = rep(NA, length(formulae)))
for(j in 1:length(formulae)){
  print(j)
  mod <- glmmTMB(formulae[j][[1]], 
                 family=beta_family(link="logit"), 
                 data = dat_beta,
                 control = glmmTMBControl(rank_check = "adjust"))
  aicc <- AICc(mod)
  aic_df$AICc[j] <- aicc
}

ggplot(aic_df, aes(model,AICc)) + geom_point()
selected <- paste0("f",aic_df[which.min(aic_df$AICc),"model"])
get(selected)
final_model <- glmmTMB(get(selected), 
                       family=beta_family(link="logit"), 
                       data = dat_beta,
                       control = glmmTMBControl(rank_check = "adjust"))
mod_sim <- simulateResiduals(final_model)
plot(mod_sim)
print(summary(final_model))
print(r.squaredGLMM(final_model))
rmse_final <- sqrt(mean(residuals(final_model)^2))
rmse_final
check_singularity(final_model)
check_collinearity(final_model)


beta_actual    = dat_beta$severity_pct
beta_predicted = predict(final_model, type="response")
beta_residuals = residuals(final_model)
# efronRSquared(residual = Residuals, 
#               predicted = Predicted, 
#               statistic = "EfronRSquared")

beta_resid <- ggplot(mapping=aes(beta_actual,beta_predicted)) +
  geom_abline(slope=1, intercept=c(0,0), color="red",linetype="dashed") +
  geom_point(shape=1) +
  coord_equal() +
  scale_x_continuous(limits = c(0,1), labels = percent_format()) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x="Observed Severe-Steep Percent",
       y="Predicted Severe-Steep Percent") +
  theme_bw()
beta_resid

ggsave("beta_glmm_resid.jpg", plot = beta_resid, path = here("Plots"), height = 3, width = 3)

# effects plots
ccp_effects <- effect(term = "canopy_consumption_pct_mean", mod = final_model, xlevels = 100)
ccp_effects <- as.data.frame(ccp_effects)
crt_effects <- effect(term = "canopy_residence_time_mean", mod = final_model, xlevels = 100)
crt_effects <- as.data.frame(crt_effects)
srt_effects <- effect(term = "surface_residence_time_mean", mod = final_model, xlevels = 100)
srt_effects <- as.data.frame(srt_effects)

ccp <- ggplot() +
  geom_line(data = ccp_effects, 
            aes(x=canopy_consumption_pct_mean/100, y=fit), 
            color = "slateblue") +
  geom_ribbon(data = ccp_effects, 
              aes(x=canopy_consumption_pct_mean/100, ymin=lower, ymax=upper), 
              alpha=0.5,
              fill = "slateblue") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x = "Mean Canopy\nConsumption Percent",
       y = "Steep-Severe Percent",
       color = "Fire") +
  geom_rug(data = dat_beta,
           aes(x=canopy_consumption_pct_mean/100, color=fire),
           sides = "b") +
  scale_color_colorblind() +
  theme_bw()
ccp

crt <- ggplot() +
  geom_line(data = crt_effects, 
            aes(x=canopy_residence_time_mean, y=fit), 
            color = "slateblue") +
  geom_ribbon(data = crt_effects, 
              aes(x=canopy_residence_time_mean, ymin=lower, ymax=upper), 
              alpha=0.5,
              fill = "slateblue") +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x = "Mean Canopy\nResidence Time (s)",
       y = "Steep-Severe Percent",
       color = "Fire") +
  geom_rug(data = dat_beta,
           aes(x=canopy_residence_time_mean, color=fire),
           sides = "b") +
  scale_color_colorblind() +
  theme_bw()
crt

srt <- ggplot() +
  geom_line(data = srt_effects, 
            aes(x=surface_residence_time_mean, y=fit), 
            color = "slateblue") +
  geom_ribbon(data = srt_effects, 
              aes(x=surface_residence_time_mean, ymin=lower, ymax=upper), 
              alpha=0.5,
              fill = "slateblue") +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x = "Mean Surface\nResidence Time (s)",
       y = "Steep-Severe Percent",
       color = "Fire") +
  geom_rug(data = dat_beta,
           aes(x=surface_residence_time_mean, color=fire),
           sides = "b") +
  scale_color_colorblind() +
  theme_bw()
srt


beta_effects <- ccp + crt + srt + plot_layout(guides = "collect", axes = "collect")
ggsave("glmm_effects.jpg", plot=beta_effects, path=here("Plots"), height = 3, width = 6)

###########
# GLMM with binomial family
threshold <- 0.25
dat_binom <- dat_site %>%
  mutate(high_risk = if_else(severity_pct > threshold, 1, 0))

f1 <- high_risk ~ canopy_consumption_pct_mean + (1|fire)
f2 <- high_risk ~ surface_consumption_pct_mean + (1|fire)
f3 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + (1|fire)

f4 <- high_risk ~ canopy_consumption_pct_mean + max_power_mean + (1|fire)
f5 <- high_risk ~ surface_consumption_pct_mean + max_power_mean + (1|fire)
f6 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + max_power_mean + (1|fire)

f7 <- high_risk ~ canopy_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f8 <- high_risk ~ surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f9 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + (1|fire)
f10 <- high_risk ~ canopy_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f11 <- high_risk ~ surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f12 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + surface_residence_time_mean + (1|fire)
f13 <- high_risk ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f14 <- high_risk ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f15 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f16 <- high_risk ~ canopy_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f17 <- high_risk ~ surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f18 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + max_power_mean + (1|fire)
f19 <- high_risk ~ canopy_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f20 <- high_risk ~ surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f21 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f22 <- high_risk ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f23 <- high_risk ~ surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f24 <- high_risk ~ canopy_consumption_pct_mean + surface_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

f26 <- high_risk ~ max_power_mean + (1|fire)

f27 <- high_risk ~ canopy_residence_time_mean + (1|fire)
f28 <- high_risk ~ surface_residence_time_mean + (1|fire)
f29 <- high_risk ~ canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f30 <- high_risk ~ canopy_residence_time_mean + max_power_mean + (1|fire)
f31 <- high_risk ~ surface_residence_time_mean + max_power_mean + (1|fire)
f32 <- high_risk ~ canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

f33 <- high_risk ~ canopy_consumption_tot_sum + (1|fire)
f34 <- high_risk ~ surface_consumption_tot_sum + (1|fire)
f35 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + (1|fire)

f36 <- high_risk ~ canopy_consumption_tot_sum + max_power_mean + (1|fire)
f37 <- high_risk ~ surface_consumption_tot_sum + max_power_mean + (1|fire)
f38 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + max_power_mean + (1|fire)

f39 <- high_risk ~ canopy_consumption_tot_sum + canopy_residence_time_mean + (1|fire)
f40 <- high_risk ~ surface_consumption_tot_sum + canopy_residence_time_mean + (1|fire)
f41 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + canopy_residence_time_mean + (1|fire)
f42 <- high_risk ~ canopy_consumption_tot_sum + surface_residence_time_mean + (1|fire)
f43 <- high_risk ~ surface_consumption_tot_sum + surface_residence_time_mean + (1|fire)
f44 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + surface_residence_time_mean + (1|fire)
f45 <- high_risk ~ canopy_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f46 <- high_risk ~ surface_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)
f47 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + (1|fire)

f48 <- high_risk ~ canopy_consumption_tot_sum + canopy_residence_time_mean + max_power_mean + (1|fire)
f49 <- high_risk ~ surface_consumption_tot_sum + canopy_residence_time_mean + max_power_mean + (1|fire)
f50 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + canopy_residence_time_mean + max_power_mean + (1|fire)
f51 <- high_risk ~ canopy_consumption_tot_sum + surface_residence_time_mean + max_power_mean + (1|fire)
f52 <- high_risk ~ surface_consumption_tot_sum + surface_residence_time_mean + max_power_mean + (1|fire)
f53 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + surface_residence_time_mean + max_power_mean + (1|fire)
f54 <- high_risk ~ canopy_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f55 <- high_risk ~ surface_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)
f56 <- high_risk ~ canopy_consumption_tot_sum + surface_consumption_tot_sum + canopy_residence_time_mean + surface_residence_time_mean + max_power_mean + (1|fire)

formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,
              f21,f22,f23,f24,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,
              f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56)
aic_df <- tibble("model" = seq(1,length(formulae)), "AICc" = rep(NA, length(formulae)))
for(j in 1:length(formulae)){
  print(j)
  mod <- glmmTMB(formulae[j][[1]], 
                 family=binomial(link = "logit"), 
                 data = dat_binom,
                 control = glmmTMBControl(rank_check = "adjust"))
  aicc <- AICc(mod)
  aic_df$AICc[j] <- aicc
}

ggplot(aic_df, aes(model,AICc)) + geom_point()
selected <- paste0("f",aic_df[which.min(aic_df$AICc),"model"])
get(selected)
final_model <- glmmTMB(get(selected), 
                       family=binomial(link = "logit"), 
                       data = dat_binom,
                       control = glmmTMBControl(rank_check = "adjust"))

mod_sim <- simulateResiduals(final_model)
plot(mod_sim)
print(summary(final_model))
rmse(final_model)
check_singularity(final_model)

binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

binomial_effects <- dat_binom %>%
  ggplot(aes(canopy_consumption_pct_mean/100,high_risk, color = fire)) +
  geom_point(shape = 1) +
  binomial_smooth(color = "slateblue", fill="slateblue") +
  scale_x_continuous(labels = percent_format()) +
  labs(x="Mean Canopy Consumption Percent",
       y = "Predicted Probability of\nHigh Debris Flow Risk",
       color = "Fire") +
  scale_color_colorblind() +
  theme_bw() +
  theme(legend.position = "none")

ggsave( "binomial_effects.jpg", plot=binomial_effects, path = here("Plots"), width = 4, height = 3)

library(yardstick)
Actual <- factor(dat_binom$high_risk)
Predicted <- factor(round(predict(final_model, type="response")))
Predicted_prob <- predict(final_model, type="response")
confusion <- tibble(actual = Actual, predicted = Predicted)
confusion_matrix <- conf_mat(confusion, truth="actual", estimate="predicted")
confusion_matrix
auc_df <- tibble(truth = Actual, .pred_yes = Predicted_prob)
roc_auc(auc_df, truth, .pred_yes)

TClass <- factor(c("Low Risk", "Low Risk", "High Risk", "High Risk"), 
                 levels = c("Low Risk","High Risk"))
PClass <- factor(c("Low Risk", "High Risk", "Low Risk", "High Risk"),
                 levels = c("High Risk","Low Risk"))
Y      <- c(23, 27, 15, 35)
conf_df <- data.frame(TClass, PClass, Y)

binom_conf <- ggplot(data = conf_df, 
       mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), 
            vjust = 1) +
  scale_fill_gradient(low = "white", high = "gray50") +
  scale_x_discrete(expand = c(0,0), position = "top") +
  scale_y_discrete(expand = c(0,0)) +
  labs(x="Truth",
       y="Prediction") +
  coord_equal() +
  theme_bw() + 
  theme(legend.position = "none",
        axis.ticks=element_blank(),
        axis.text.y = element_text(angle=90, hjust = 0.5))


## Assemble plot

free(beta_effects) / (beta_resid + binomial_effects + binom_conf)
ggsave("linear_modeling.jpg",path=here("Plots"),height=6, width=9)



