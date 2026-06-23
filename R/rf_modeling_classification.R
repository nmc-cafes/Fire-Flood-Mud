"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)
library(themis)
library(vip)

### First with multiple classes
dat <- read.csv(here("QF_results","SBS","qf_results_site_corrected.csv"))
dat <- dat %>%
  mutate(severity_class = if_else(severity_pct > 0.25, 1, 0)) %>%
  mutate(severity_class = factor(severity_class))

set.seed(47)
dat_split <- initial_split(dat, strata = severity_class)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

cls_metrics <- metric_set(roc_auc, accuracy, sensitivity, specificity)

cls_formula <- formula(severity_class ~ 
                        surface_consumption_pct_mean + 
                        surface_consumption_tot_sum + 
                        canopy_consumption_pct_mean +
                        canopy_consumption_tot_sum + 
                        surface_residence_time_mean +
                        canopy_residence_time_mean +
                        max_power_mean +
                        energy_flux_mean)

dat_rec <- recipe(cls_formula, data = dat_train) %>%
  step_dummy(all_nominal(), -all_outcomes())

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
  mtry(range = c(1,6)),
  min_n(range = c(10,40)),
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

best_auc <- select_best(regular_res, metric="roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

dat_prep <- prep(dat_rec)

var_names <- c(
  "surface_consumption_tot_sum" = "Total Surface Consumption",
  "surface_residence_time_mean" = "Mean Surface Residence Time",
  "canopy_residence_time_mean" = "Mean Canopy Residence Time",
  "canopy_consumption_tot_sum" = "Total Canopy Consumption",
  "energy_flux_mean" = "Mean 30-s Power",
  "surface_consumption_pct_mean" = "Mean Surface Consumption Percent",
  "max_power_mean" = "Mean Maximum Power",
  "canopy_consumption_pct_mean" = "Mean Canopy Consumption Percent"
)

final_fit <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(cls_formula, data = juice(dat_prep))

vip_data <- vi(final_fit)

ggplot(vip_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Importance)) +
  geom_point(size = 2) +
  scale_x_discrete(labels = var_names) +
  coord_flip() +
  theme_bw() +
  xlab("Variable") + 
  theme(axis.title.y = element_blank())

ggsave("rf_vip_classification.jpg", path = here("Plots"), width = 4.7, height = 3)

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
  conf_mat(severity_class, .pred_class)

train_fit <- final_wf %>% fit(dat_train)
predictions <- predict(train_fit, new_data = dat_test, type = "class")

