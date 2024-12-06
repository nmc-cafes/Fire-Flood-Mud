"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)

dat <- read.csv(here("QF_results","qf_results_site.csv"))
# dat <- dat %>% filter(severity_class != "low")

dat <- dat %>%
  mutate(severity_class = if_else(severity_pct > 30, 1, 0))

set.seed(47)
dat_split <- initial_split(dat, strata = severity_class)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

dnbr_formula <- formula(dNBR_mean ~ 
                surface_consumption_pct_mean + 
                surface_consumption_tot_sum + 
                canopy_consumption_pct_mean +
                canopy_consumption_tot_sum + 
                surface_residence_time_mean +
                canopy_residence_time_mean +
                max_power_mean +
                energy_flux_mean)

dat_rec <- recipe(dnbr_formula, data = dat_train) %>%
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
  mtry(range = c(1, 4)),
  min_n(range = c(20, 40)),
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
  fit(dnbr_formula,
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
  geom_point(aes(dNBR_mean,.pred), shape=1, alpha=0.5) +
  geom_abline(intercept = 0, slope=1, linetype="dashed", color="red") +
  scale_x_continuous(limits = c(200,800)) +
  scale_y_continuous(limits = c(200,800)) +
  labs(x="Observed dNBR",
       y="Predicted dNBR") +
  theme_bw() +
  theme(aspect.ratio = 1)
  
  
