library(here)
library(tidyverse)
library(tidymodels)

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

lm_spec <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

dat_prep <- prep(dat_rec)
lm_spec %>%
  set_engine("glm") %>%
  fit(high_sev_bin ~ .,
      data = juice(dat_prep)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(lm_spec)

final_res <- final_wf %>%
  last_fit(dat_split,
           metrics = cls_metrics)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  conf_mat(high_sev_bin, .pred_class)
