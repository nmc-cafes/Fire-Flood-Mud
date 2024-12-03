"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)
library(themis)

### First with multiple classes
dat_raw <- read_csv(here("QF_results","qf_results.csv"))

dat <- dat_raw %>%
  group_by(site, fire) %>%
  summarize(severity_pct = sum(severity %in% c(3,4)) / sum(severity > 0),
            canopy_consumption_pct_mean = mean(canopy_consumption_pct)*100,
            canopy_consumption_tot_sum = sum(canopy_consumption_tot),
            canopy_residence_time_mean = mean(canopy_residence_time),
            energy_flux_mean = mean(energy_flux),
            mass_burnt_pct_mean = mean(mass_burnt_pct),
            max_power_mean = mean(max_power),
            surface_consumption_pct_mean = mean(surface_consumption_pct)*100,
            surface_consumption_tot_sum = sum(surface_consumption_tot),
            surface_residence_time_mean = mean(surface_residence_time),
            total_power_sum = sum(total_power)) %>%
  ungroup() %>%
  mutate(high_sev_cls = cut_interval(severity_pct, 4))

set.seed(47)
dat_split <- initial_split(dat, strata = high_sev_cls)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

cls_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

cls_formula <- formula(high_sev_cls ~ 
                         mass_burnt_pct_mean +
                         surface_consumption_pct_mean + 
                         surface_consumption_tot_sum + 
                         canopy_consumption_pct_mean + 
                         canopy_consumption_tot_sum +
                         max_power_mean + 
                         energy_flux_mean +
                         surface_residence_time_mean +
                         canopy_residence_time_mean)

dat_rec <- recipe(cls_formula, data = dat_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_smote(high_sev_cls)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(tune_spec)

set.seed(4747)
dat_folds <- vfold_cv(dat_train)

doParallel::registerDoParallel()

set.seed(474747)
tune_res <- tune_grid(
  tune_wf,
  resamples = dat_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "ROC_AUC")

rf_grid <- grid_regular(
  mtry(range = c(1,8)),
  min_n(range = c(20,40)),
  levels = 8
)

set.seed(47474747)
regular_res <- tune_grid(
  tune_wf,
  resamples = dat_folds,
  grid = rf_grid,
  metrics = cls_metrics
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "roc_auc")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

library(vip)

dat_prep <- prep(dat_rec)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(high_sev_cls ~ .,
      data = juice(dat_prep)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(dat_split,
           metrics = cls_metrics)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  conf_mat(high_sev_cls, .pred_class)

train_fit <- final_wf %>% fit(dat_train)
predictions <- predict(train_fit, new_data = dat_test, type = "class")

###############
## Then with only two

dat <- dat %>%
  mutate(high_sev_bin = if_else(severity_pct>0.50,1,0)) %>%
  mutate(high_sev_bin = factor(high_sev_bin))

set.seed(47)
dat_split <- initial_split(dat, strata = high_sev_bin)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

cls_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

cls_formula <- formula(high_sev_bin ~ 
                         mass_burnt_pct_mean +
                         surface_consumption_pct_mean + 
                         surface_consumption_tot_sum + 
                         canopy_consumption_pct_mean + 
                         canopy_consumption_tot_sum +
                         max_power_mean + 
                         energy_flux_mean +
                         surface_residence_time_mean +
                         canopy_residence_time_mean)

dat_rec <- recipe(cls_formula, data = dat_train) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_smote(high_sev_bin)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(tune_spec)

set.seed(4747)
dat_folds <- vfold_cv(dat_train)

doParallel::registerDoParallel()

set.seed(474747)
tune_res <- tune_grid(
  tune_wf,
  resamples = dat_folds,
  grid = 20
)

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "ROC_AUC")

rf_grid <- grid_regular(
  mtry(range = c(1, 9)),
  min_n(range = c(20, 40)),
  levels = 5
)

set.seed(47474747)
regular_res <- tune_grid(
  tune_wf,
  resamples = dat_folds,
  grid = rf_grid,
  metrics = cls_metrics
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "roc_auc")

best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

library(vip)

dat_prep <- prep(dat_rec)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(high_sev_bin ~ .,
      data = juice(dat_prep)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(dat_split,
           metrics = cls_metrics)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  conf_mat(high_sev_bin, .pred_class)

train_fit <- final_wf %>% fit(dat_train)
predictions <- predict(train_fit, new_data = dat_test, type = "class")
