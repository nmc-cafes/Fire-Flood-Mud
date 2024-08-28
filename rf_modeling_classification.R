"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)
library(themis)

### First with multiple classes

dat <- read.csv(here("all_data_expandedsampling_site.csv")) %>%
  mutate(high_sev_cls = cut_interval(high_sev_pct, 4))

set.seed(47)
dat_split <- initial_split(dat, strata = high_sev_cls)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

cls_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

cls_formula <- formula(high_sev_cls ~ surface_consumption_pct + surface_consumption + canopy_consumption + max_power + residence_time_power)

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
  mtry(range = c(3,9)),
  min_n(range = c(30,70)),
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

dat <- read.csv(here("all_data_expandedsampling_site.csv")) %>%
  mutate(high_sev_bin = factor(high_sev_bin))

set.seed(47)
dat_split <- initial_split(dat, strata = high_sev_bin)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

cls_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

cls_formula <- formula(high_sev_bin ~ surface_consumption_pct + surface_consumption + canopy_consumption + max_power + residence_time_power)

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
  mtry(range = c(1, 5)),
  min_n(range = c(15, 35)),
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