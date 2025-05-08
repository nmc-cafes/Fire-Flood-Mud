"
Model severity from QF outputs using Random Forests
"

library(here)
library(tidyverse)
library(tidymodels)
library(doParallel)
library(scales)
library(vip)

dat <- read.csv(here("QF_results","SBS","qf_results_site_corrected.csv"))
# dat <- dat %>% filter(severity_class != "low")

dat <- dat %>%
  mutate(severity_class = if_else(severity_pct > 0.25, 1, 0))

set.seed(47)
dat_split <- initial_split(dat, strata = severity_class)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)

ss_formula <- formula(severity_pct ~ 
                surface_consumption_pct_mean + 
                surface_consumption_tot_sum + 
                canopy_consumption_pct_mean +
                canopy_consumption_tot_sum + 
                surface_residence_time_mean +
                canopy_residence_time_mean +
                max_power_mean +
                energy_flux_mean)

dat_rec <- recipe(ss_formula, data = dat_train) %>%
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
  mtry(range = c(1, 8)),
  min_n(range = c(1, 40)),
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

best_rmse <- select_best(regular_res, metric="rmse")

final_rf <- finalize_model(
  tune_spec,
  best_rmse
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
  fit(ss_formula, data = juice(dat_prep))

vip_data <- vi(final_fit)

ggplot(vip_data, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_segment(aes(xend = Variable, y = 0, yend = Importance)) +
  geom_point(size = 2) +
  scale_x_discrete(labels = var_names) +
  coord_flip() +
  theme_bw() +
  xlab("Variable") + 
  theme(axis.title.y = element_blank())

ggsave("rf_vip_regression.jpg", path = here("Plots"), width = 4.7, height = 3)

final_wf <- workflow() %>%
  add_recipe(dat_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(dat_split)

final_res %>%
  collect_metrics()

reg_res <- final_res %>%
  collect_predictions() %>%
  ggplot() +
  geom_point(aes(severity_pct,.pred), shape=1, alpha=0.5) +
  geom_abline(intercept = 0, slope=1, linetype="dashed", color="red") +
  scale_x_continuous(limits = c(0,1), labels = percent_format()) +
  scale_y_continuous(limits = c(0,1), labels = percent_format()) +
  labs(x="Observed Severe-Steep Percent",
       y="Predicted Severe-Steep Percent") +
  theme_bw() +
  theme(aspect.ratio = 1)

ggsave("rf_fes_ss.jpg",path = here("Plots"), height = 3, width = 3)


# Results from rf_modeling_classification
TClass <- factor(c("Low Risk", "Low Risk", "High Risk", "High Risk"), 
                 levels = c("Low Risk","High Risk"))
PClass <- factor(c("Low Risk", "High Risk", "Low Risk", "High Risk"),
                 levels = c("High Risk","Low Risk"))
Y      <- c(8,5,7,6)
conf_df <- data.frame(TClass, PClass, Y)

binom_conf <- ggplot(data = conf_df, 
                     mapping = aes(x = TClass, y = PClass)) +
  geom_tile(aes(fill = Y), colour = "black") +
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

free(reg_res) + free(binom_conf)
ggsave("rf_results.jpg", path=here("Plots"), height = 3, width=6)

