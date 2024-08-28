library(here)
library(tidyverse)

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

dat_scaled <- dat_site
dat_scaled[8:13] <- as.data.frame(scale(dat_scaled[8:13]))

dat_loglog <- dat_site %>%
  mutate(across(c(dNBR,surface_consumption,canopy_consumption, max_power, residence_time_power), log))

# dNBR
best_subset <- regsubsets(high_sev_pct ~ 
                            surface_consumption *
                            canopy_consumption *
                            max_power * 
                            residence_time_power,
                          method = "exhaustive",
                          nbest = 30,
                          nvmax = 5,
                          dat_scaled)
summary(best_subset)
reg.output.search.with.test(best_subset)

# create training - testing data
set.seed(47)
sample <- sample(c(TRUE, FALSE), nrow(dat_scaled), replace = T, prob = c(0.7,0.3))
train <- dat_scaled[sample, ]
test <- dat_scaled[!sample, ]

# perform best subset selection
best_subset <- regsubsets(high_sev_pct ~ 
                            surface_consumption *
                            canopy_consumption *
                            max_power *
                            residence_time_power,
                          train)
results <- summary(best_subset)

# extract and plot results
tibble(predictors = 1:8,
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
test_m <- model.matrix(high_sev_pct ~ 
                         surface_consumption *
                         canopy_consumption *
                         max_power * 
                         residence_time_power,
                       test)

# create empty vector to fill with error values
validation_errors <- vector("double", length = 8)

for(i in 1:8) {
  coef_x <- coef(best_subset, id = i)                     # extract coefficients for model size i
  pred_x <- test_m[ , names(coef_x)] %*% coef_x           # predict salary using matrix algebra
  validation_errors[i] <- mean((test$high_sev_pct - pred_x)^2)  # compute test error btwn actual & predicted salary
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
cv_errors <- matrix(NA, k, 8, dimnames = list(NULL, paste(1:8)))

for(j in 1:k) {
  
  # perform best subset on rows not equal to j
  best_subset_j <- regsubsets(high_sev_pct ~ 
                                surface_consumption *
                                canopy_consumption *
                                max_power *
                                residence_time_power,
                              dat_scaled[folds != j, ], nvmax = 10)
  
  # perform cross-validation
  for(i in 1:8) {
    pred_x <- predict_regsubsets(best_subset_j, dat_scaled[folds == j, ], id = i)
    cv_errors[j, i] <- mean((dat_scaled$high_sev_pct[folds == j] - pred_x)^2)
  }
}

mean_cv_errors <- colMeans(cv_errors)
plot(mean_cv_errors, type = "b")

# find final model
final_best <- regsubsets(high_sev_pct ~ 
                           surface_consumption *
                           canopy_consumption *
                           max_power * 
                           residence_time_power,
                         data = dat_scaled,
                         nvmax = 10)
coef(final_best, which.min(mean_cv_errors))


# what is the r-squared, rmse?
final_mod <- lm(high_sev_pct ~ surface_consumption, data=dat_scaled)
summary(final_mod)
check_model(final_mod)

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

dat_subsets <- list(dat_homo, dat_hetero, dat_high, dat_mod, dat_low)

mod_mixed <- lmer(high_sev_pct ~ 
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
for(i in 1:length(dat_subsets)){
  
  f1 <- high_sev_pct ~ mass_burnt_pct + max_power + residence_time_power + (1 | fire) 
  f2 <- high_sev_pct ~ surface_consumption_pct + canopy_consumption + max_power + residence_time_power + (1 | fire) 
  f3 <- high_sev_pct ~ surface_consumption_pct + max_power + residence_time_power + (1 | fire) 
  f4 <- high_sev_pct ~ canopy_consumption + max_power + residence_time_power + (1 | fire) 
  f5 <- high_sev_pct ~ mass_burnt_pct + residence_time_power + (1 | fire) 
  f6 <- high_sev_pct ~ surface_consumption_pct + canopy_consumption + residence_time_power + (1 | fire) 
  f7 <- high_sev_pct ~ surface_consumption_pct + residence_time_power + (1 | fire) 
  f8 <- high_sev_pct ~ canopy_consumption + residence_time_power + (1 | fire) 
  f9 <- high_sev_pct ~ max_power + residence_time_power + (1 | fire) 
  f10 <- high_sev_pct ~ mass_burnt_pct + max_power + (1 | fire) 
  f11 <- high_sev_pct ~ surface_consumption_pct + canopy_consumption + max_power + (1 | fire) 
  f12 <- high_sev_pct ~ surface_consumption_pct + max_power + (1 | fire) 
  f13 <- high_sev_pct ~ canopy_consumption + max_power + (1 | fire) 
  f14 <- high_sev_pct ~ mass_burnt_pct + (1 | fire) 
  f15 <- high_sev_pct ~ surface_consumption_pct + canopy_consumption + (1 | fire) 
  f16 <- high_sev_pct ~ surface_consumption_pct + (1 | fire) 
  f17 <- high_sev_pct ~ canopy_consumption + (1 | fire) 
  f18 <- high_sev_pct ~ max_power + (1 | fire) 
  f19 <- high_sev_pct ~ residence_time_power + (1 | fire) 
  
  f20 <- high_sev_pct ~ surface_consumption + canopy_consumption + max_power + residence_time_power + (1 | fire) 
  f21 <- high_sev_pct ~ surface_consumption + max_power + residence_time_power + (1 | fire) 
  f22 <- high_sev_pct ~ surface_consumption + canopy_consumption + residence_time_power + (1 | fire) 
  f23 <- high_sev_pct ~ surface_consumption + residence_time_power + (1 | fire) 
  f24 <- high_sev_pct ~ surface_consumption + canopy_consumption + max_power + (1 | fire) 
  f25 <- high_sev_pct ~ surface_consumption + max_power + (1 | fire) 
  f26 <- high_sev_pct ~ surface_consumption + canopy_consumption + (1 | fire) 
  f27 <- high_sev_pct ~ surface_consumption + (1 | fire) 
  
  formulae <- c(f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,
                f24,f25,f26,f27)
  aic_df <- tibble("model" = seq(1,length(formulae)), "AICc" = rep(NA, length(formulae)))
  for(j in 1:length(formulae)){
    mod <- lmer(formulae[j][[1]], data = dat_scaled)
    aicc <- AICc(mod)
    aic_df$AICc[j] <- aicc
  }
  
  ggplot(aic_df, aes(model,AICc)) + geom_point()
  selected <- paste0("f",aic_df[which.min(aic_df$AICc),"model"])
  final_model <- lmer(get(selected), data = dat_scaled)
  mod_sim <- simulateResiduals(final_model)
  plot(mod_sim)
  print(summary(final_model))
  print(r.squaredGLMM(final_model))
}


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

