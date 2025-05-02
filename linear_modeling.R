library(here)
library(tidyverse)
library(ggthemes)
library(scales)

reg.output.search.with.test<- function (search_object) {  ## input an object from a regsubsets search
  ## First build a df listing model components and metrics of interest
  search_comp<-data.frame(R2=summary(search_object)$rsq,  
                          adjR2=summary(search_object)$adjr2,
                          BIC=summary(search_object)$bic,
                          CP=summary(search_object)$cp,
                          n_predictors=row.names(summary(search_object)$which),
                          summary(search_object)$which)
  ## Categorize different types of predictors based on whether '.' is present
  predictors<-colnames(search_comp)[(match("X.Intercept.",names(search_comp))+1):dim(search_comp)[2]]
  main_pred<-predictors[grep(pattern = ".", x = predictors, invert=T, fixed=T)]
  higher_pred<-predictors[grep(pattern = ".", x = predictors, fixed=T)]
  ##  Define a variable that indicates whether model should be reject, set to FALSE for all models initially.
  search_comp$reject_model<-FALSE  
  
  for(main_eff_n in 1:length(main_pred)){  ## iterate through main effects
    ## Find column numbers of higher level ters containing the main effect
    search_cols<-grep(pattern=main_pred[main_eff_n],x=higher_pred) 
    ## Subset models that are not yet flagged for rejection, only test these
    valid_model_subs<-search_comp[search_comp$reject_model==FALSE,]  
    ## Subset dfs with only main or higher level predictor columns
    main_pred_df<-valid_model_subs[,colnames(valid_model_subs)%in%main_pred]
    higher_pred_df<-valid_model_subs[,colnames(valid_model_subs)%in%higher_pred]
    
    if(length(search_cols)>0){  ## If there are higher level pred, test each one
      for(high_eff_n in search_cols){  ## iterate through higher level pred. 
        ##  Test if the intxn effect is present without main effect (working with whole column of models)
        test_responses<-((main_pred_df[,main_eff_n]==FALSE)&(higher_pred_df[,high_eff_n]==TRUE)) 
        valid_model_subs[test_responses,"reject_model"]<-TRUE  ## Set reject to TRUE where appropriate
      } ## End high_eff for
      ## Transfer changes in reject to primary df:
      search_comp[row.names(valid_model_subs),"reject_model"]<-valid_model_subs[,"reject_model"]
    } ## End if
  }  ## End main_eff for
  
  ## Output resulting table of all models named for original search object and current time/date in folder "model_search_reg"
  current_time_date<-format(Sys.time(), "%m_%d_%y at %H_%M_%S")
  write.table(search_comp,file=paste("./model_search_reg/",paste(current_time_date,deparse(substitute(search_object)),
                                                                 "regSS_model_search.csv",sep="_"),sep=""),row.names=FALSE, col.names=TRUE, sep=",")
}  ## End reg.output.search.with.test fn
## Try some preliminary model selection

library(leaps)

dat_site <- read.csv(here("QF_results","SBS","qf_results_site_corrected.csv"))

# dat_loglog <- dat_site %>%
#   mutate(across(c(dNBR,surface_consumption,canopy_consumption, max_power, residence_time_power), log))

# dNBR
# best_subset <- regsubsets(dNBR_mean ~ 
#                             mass_burnt_pct_mean *
#                             surface_consumption_tot_sum *
#                             surface_consumption_pct_mean *
#                             canopy_consumption_tot_sum *
#                             canopy_consumption_pct_mean *
#                             max_power_mean *
#                             energy_flux_mean *
#                             surface_residence_time_mean *
#                             canopy_residence_time_mean,
#                           method = "exhaustive",
#                           nbest = 30,
#                           nvmax = 5,
#                           dat_site)
# summary(best_subset)
# reg.output.search.with.test(best_subset)

# create training - testing data
set.seed(47)
sample <- sample(c(TRUE, FALSE), nrow(dat_site), replace = T, prob = c(0.7,0.3))
train <- dat_site[sample, ]
test <- dat_site[!sample, ]

# perform best subset selection
best_subset <- regsubsets(dNBR_mean ~ 
                            surface_consumption_tot_sum *
                            canopy_consumption_tot_sum *
                            max_power_mean *
                            energy_flux_mean *
                            surface_residence_time_mean *
                            canopy_residence_time_mean,
                          method = "exhaustive",
                          nbest = 30,
                          nvmax = 5,
                          dat_site,
                          really.big = T)
results <- summary(best_subset)

# extract and plot results
tibble(predictors = 1:150,
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
test_m <- model.matrix(dNBR_mean ~ 
                         surface_consumption_tot_sum *
                         canopy_consumption_tot_sum *
                         max_power_mean *
                         energy_flux_mean *
                         surface_residence_time_mean *
                         canopy_residence_time_mean,
                       method = "exhaustive",
                       nbest = 30,
                       nvmax = 5,
                       test)

# create empty vector to fill with error values
validation_errors <- vector("double", length = 150)

for(i in 1:150) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$dNBR_mean - pred_x)^2)  # compute test error btwn actual & predicted salary
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
folds <- sample(1:k, nrow(dat_site), replace = TRUE)
cv_errors <- matrix(NA, k, 150, dimnames = list(NULL, paste(1:150)))

for(j in 1:k) {
  # perform best subset on rows not equal to j
  best_subset_j <- regsubsets(dNBR_mean ~ 
                                surface_consumption_tot_sum *
                                canopy_consumption_tot_sum *
                                max_power_mean *
                                energy_flux_mean *
                                surface_residence_time_mean *
                                canopy_residence_time_mean,
                              dat_site[folds != j, ], 
                              nbest = 30,
                              nvmax = 5,
                              really.big=T)
  
  # perform cross-validation
  for(i in 1:150) {
    pred_x <- predict_regsubsets(best_subset_j, dat_site[folds == j, ], id = i)
    cv_errors[j, i] <- mean((dat_site$dNBR_mean[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors, na.rm=T)
plot(mean_cv_errors, type = "b")

# find final model
final_best <- regsubsets(dNBR_mean ~ 
                           surface_consumption_tot_sum *
                           canopy_consumption_tot_sum *
                           max_power_mean *
                           energy_flux_mean *
                           surface_residence_time_mean *
                           canopy_residence_time_mean,
                         data = dat_site,
                         nbest = 30,
                         nvmax = 5,
                         really.big = T)
coef(final_best, which.min(mean_cv_errors))


# what is the r-squared, rmse?
# final_mod <- lm(severity_pct ~ 
#                   canopy_residence_time_mean +
#                   energy_flux_mean:surface_residence_time_mean +
#                   surface_consumption_tot_sum:canopy_residence_time_mean +
#                   surface_consumption_tot_sum:max_power_mean:energy_flux_mean:surface_residence_time_mean,
#                 data=dat_site)
final_mod <- lm(severity_pct ~ 
                  surface_consumption_tot_sum:canopy_consumption_tot_sum,
                data=dat_site)
summary(final_mod)
library(performance)
model_check <- check_model(final_mod)

#############

# try a mixed model?
library(lmerTest)
library(MuMIn)
library(DHARMa)
library(glmmTMB)

dat_beta <- dat_site %>%
  mutate(severity_pct = case_when(severity_pct==0 ~ 0.001,
                                  severity_pct==1 ~ 0.999,
                                  TRUE ~ severity_pct))
mod_mixed <- glmmTMB(severity_pct ~ 
                    mass_burnt_pct_mean + 
                    surface_consumption_tot_sum +
                    surface_consumption_pct_mean +
                    canopy_consumption_tot_sum +
                    canopy_consumption_pct_mean +
                    max_power_mean + 
                    energy_flux_mean +
                    surface_residence_time_mean +
                    canopy_residence_time_mean +
                    (1 | fire),
                    family = beta_family(link="logit"),
                  data=dat_beta)
summary(mod_mixed)
AICc(mod_mixed)

# no need to use mass burnt since it is just surface + canopy consumption
# only one of consumption_tot and consumption_pct should be used

## Predictors that are NOT correlated
# mass_burnt_pct_mean X canopy_consumption_tot_sum
# surface_consumption_tot_sum X canopy_consumption_tot_sum
# mass_burnt_pct_mean X canopy_residence_time_mean
# mass_burn_pct_mean X surface_consumption_tot_sum
# ^basically this is saying that surface and canopy consumption aren't correlated

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

f33 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean + (1|fire)
f34 <- severity_pct ~ canopy_consumption_pct_mean*max_power_mean + (1|fire)
f35 <- severity_pct ~ surface_consumption_pct_mean*max_power_mean + (1|fire)
f36 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*max_power_mean + (1|fire)

f37 <- severity_pct ~ canopy_consumption_pct_mean*canopy_residence_time_mean + (1|fire)
f38 <- severity_pct ~ surface_consumption_pct_mean*canopy_residence_time_mean + (1|fire)
f39 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*canopy_residence_time_mean + (1|fire)
f40 <- severity_pct ~ canopy_consumption_pct_mean*surface_residence_time_mean + (1|fire)
f41 <- severity_pct ~ surface_consumption_pct_mean*surface_residence_time_mean + (1|fire)
f42 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*surface_residence_time_mean + (1|fire)
f43 <- severity_pct ~ canopy_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean + (1|fire)
f44 <- severity_pct ~ surface_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean + (1|fire)
f45 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean + (1|fire)

f46 <- severity_pct ~ canopy_consumption_pct_mean*canopy_residence_time_mean*max_power_mean + (1|fire)
f47 <- severity_pct ~ surface_consumption_pct_mean*canopy_residence_time_mean*max_power_mean + (1|fire)
f48 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*canopy_residence_time_mean*max_power_mean + (1|fire)
f49 <- severity_pct ~ canopy_consumption_pct_mean*surface_residence_time_mean*max_power_mean + (1|fire)
f50 <- severity_pct ~ surface_consumption_pct_mean*surface_residence_time_mean*max_power_mean + (1|fire)
f51 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*surface_residence_time_mean*max_power_mean + (1|fire)
f52 <- severity_pct ~ canopy_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean*max_power_mean + (1|fire)
f53 <- severity_pct ~ surface_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean*max_power_mean + (1|fire)
f54 <- severity_pct ~ canopy_consumption_pct_mean*surface_consumption_pct_mean*canopy_residence_time_mean*surface_residence_time_mean*max_power_mean + (1|fire)

formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,
              f21,f22,f23,f24,f26,f27,f28,f29,f30,f31,f32)
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
final_model <- glmmTMB(get(selected), 
                       family=beta_family(link="logit"), 
                       data = dat_beta,
                       control = glmmTMBControl(rank_check = "adjust"))
mod_sim <- simulateResiduals(final_model)
plot(mod_sim)
print(summary(final_model))
print(r.squaredGLMM(final_model))

library(car)
vif(lm(severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean, 
    data=dat_beta), type = "predictor")

library(performance)
check_singularity(final_model)
check_collinearity(final_model)

# see if it's different/not singular without a random effect
final_model_fixed <- glmmTMB(severity_pct ~ canopy_consumption_pct_mean + canopy_residence_time_mean + surface_residence_time_mean,
                             family=beta_family(link="logit"), 
                             data = dat_beta)
mod_sim_fixed <- simulateResiduals(final_model_fixed)
plot(mod_sim_fixed)
print(summary(final_model_fixed))

library(rcompanion)
Actual    = dat_beta$severity_pct
Predicted = predict(final_model_fixed, type="response")
Residuals = residuals(final_model_fixed)
efronRSquared(residual = Residuals, 
              predicted = Predicted, 
              statistic = "EfronRSquared")

rmse(final_model_fixed)
check_singularity(final_model_fixed)
check_collinearity(final_model_fixed)

ggplot(mapping=aes(Actual,Predicted)) +
  geom_abline(slope=1, intercept=c(0,0), color="red",linetype="dashed") +
  geom_point(shape=1) +
  coord_equal() +
  scale_x_continuous(limits = c(0,1), labels = percent_format()) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x="Observed Severe-Steep Percent",
       y="Predicted Severe-Steep Percent") +
  theme_bw()

ggsave("beta_glmm_resid.jpg",path = here("Plots"), height = 3, width = 3)

###########
# GAM

library(mgcv)
mod_lm <- gam(high_sev_pct ~ surface_consumption + canopy_consumption + residence_time_power, data=dat_site)
mod_gam <- gam(high_sev_pct ~ s(surface_consumption, bs="cr") + s(canopy_consumption, bs="cr") + s(residence_time_power, bs="cr"), data=dat_site)
summary(mod_lm)
summary(mod_gam)
mod_gam <- update(mod_gam, . ~ . -s(residence_time_power, bs="cr") + residence_time_power)
summary(mod_gam)
AIC(mod_lm)
AIC(mod_gam)
anova(mod_lm, mod_gam, test = "Chisq")

