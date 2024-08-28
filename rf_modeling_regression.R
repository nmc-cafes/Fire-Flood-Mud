"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)

dat <- read.csv(here("all_data_expandedsampling_site.csv"))
# dat <- dat %>% filter(severity_class != "low")

set.seed(47)
dat_split <- initial_split(dat, strata = severity_class)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

scaled_formula <- formula(high_sev_pct ~ surface_consumption_pct + surface_consumption + canopy_consumption + max_power + residence_time_power)

dat_rec <- recipe(scaled_formula, data = dat_train) %>%
  step_dummy(all_nominal(), -all_outcomes())

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
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
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "RMSE")

rf_grid <- grid_regular(
  mtry(range = c(1, 5)),
  min_n(range = c(1, 20)),
  levels = 5
)

set.seed(47474747)
regular_res <- tune_grid(
  tune_wf,
  resamples = dat_folds,
  grid = rf_grid
)

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, linewidth = 1.5) +
  geom_point() +
  labs(y = "RMSE")

best_rmse <- select_best(regular_res, "rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
)

library(vip)

dat_prep <- prep(dat_rec)
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(high_sev_pct ~ .,
      data = juice(dat_prep)
  ) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(dat_split)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  ggplot() +
  geom_point(aes(high_sev_pct,.pred), shape=1, alpha=0.5) +
  geom_abline(intercept = 0, slope=1, linetype="dashed", color="red") +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x="Observed Percent Burneed at High Severity",
       y="Predicted Percent\nBurned at High Severity") +
  theme_bw() +
  theme(aspect.ratio = 1)
  
  
