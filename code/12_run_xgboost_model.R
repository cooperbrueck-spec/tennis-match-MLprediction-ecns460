library(tidyverse)
library(rsample)
library(readr)
library(recipes)
library(here)

set.seed(460)

# -------------------------------------------------
# 1. Load main modeling dataset
# -------------------------------------------------

atp_final_modeling_dataset <- read_csv(
  here("data", "cleaned", "atp_final_modeling_dataset.csv")
)

# -------------------------------------------------
# 2. Keep all variables that could be known before
#    the match, while removing obvious leakage columns
#    and pure identifiers.
# -------------------------------------------------

modeling_data <- atp_final_modeling_dataset |>
  select(
    -match_id,
    -tourney_id,
    -tourney_name,
    -match_num,
    -player_A_id,
    -player_A_name,
    -player_B_id,
    -player_B_name,
    -winner_id,
    -winner_name,
    -loser_id,
    -loser_name
  ) |>
  mutate(
    outcome_A_win = factor(outcome_A_win, levels = c(0, 1)),
    surface = as.factor(surface),
    round = as.factor(round),
    tourney_level = as.factor(tourney_level)
  )

# -------------------------------------------------
# 3. Split into training and testing samples.
#    We create the test set object, but do not process
#    or use it yet.
# -------------------------------------------------

tennis_split <- initial_split(
  modeling_data,
  prop = 0.80,
  strata = outcome_A_win
)

train_data <- training(tennis_split)
test_data  <- testing(tennis_split)

# -------------------------------------------------
# 4. Identify missingness using training data only
# -------------------------------------------------

h2h_neutral_vars <- c("prior_h2h_win_rate_A")

missing_indicator_vars <- train_data |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  filter(
    n_missing > 100,
    variable != "prior_h2h_win_rate_A"
  ) |>
  pull(variable)

tiny_missing_numeric_vars <- train_data |>
  summarise(across(where(is.numeric), ~ sum(is.na(.x)))) |>
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) |>
  filter(
    n_missing > 0,
    n_missing <= 100
  ) |>
  pull(variable)

# -------------------------------------------------
# 5. Build preprocessing recipe for boosted trees
# -------------------------------------------------

tennis_recipe <- recipe(outcome_A_win ~ ., data = train_data) |>
  
  # Dates are kept in the processed data for reference,
  # but they will be removed before model fitting.
  update_role(match_date, tourney_date, new_role = "date") |>
  
  # If prior H2H win rate is missing, that usually means no prior matchup.
  # A neutral value of 0.5 says neither player has a prior H2H advantage.
  step_mutate(
    prior_h2h_win_rate_A = if_else(
      is.na(prior_h2h_win_rate_A),
      0.5,
      prior_h2h_win_rate_A
    )
  ) |>
  
  # Add missingness indicators for variables where missingness is meaningful.
  step_indicate_na(all_of(missing_indicator_vars)) |>
  
  # Median-impute the same high-missingness variables.
  step_impute_median(all_of(missing_indicator_vars)) |>
  
  # Median-impute small numeric gaps without adding indicators.
  step_impute_median(all_of(tiny_missing_numeric_vars)) |>
  
  # Handle missing categorical predictors.
  step_unknown(all_nominal_predictors()) |>
  
  # Convert categorical predictors into dummy variables for xgboost.
  step_dummy(all_nominal_predictors()) |>
  
  # Remove predictors with no variation.
  step_zv(all_predictors())

# -------------------------------------------------
# 6. Prep and apply recipe to training data only
# -------------------------------------------------

tennis_recipe_prepped <- prep(
  tennis_recipe,
  training = train_data,
  retain = TRUE
)

train_processed <- bake(
  tennis_recipe_prepped,
  new_data = train_data
)

# -------------------------------------------------
# 7. Remove date columns before model fitting
# -------------------------------------------------

train_xgb <- train_processed |>
  select(
    -match_date,
    -tourney_date
  )

# -------------------------------------------------
# 8. Training data checks
# -------------------------------------------------

sum(is.na(train_xgb))
dim(train_xgb)

train_xgb |>
  count(outcome_A_win) |>
  mutate(prop = n / sum(n))

# -------------------------------------------------
# Run Gradient Boosted Tree Model
# -------------------------------------------------

library(tidymodels)
library(xgboost)

set.seed(460)

# -------------------------------------------------
# 1. Define boosted tree model
# -------------------------------------------------

xgb_model <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  min_n = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

# -------------------------------------------------
# 2. Cross-validation on training data only
# -------------------------------------------------

xgb_folds <- vfold_cv(
  train_xgb,
  v = 5,
  strata = outcome_A_win
)

# -------------------------------------------------
# 3. Workflow
# -------------------------------------------------

xgb_workflow <- workflow() |>
  add_model(xgb_model) |>
  add_formula(outcome_A_win ~ .)

# -------------------------------------------------
# 4. Create tuning grid
# -------------------------------------------------
# This grid searches across different model complexities.
# It is intentionally moderate so it does not take forever to run.

xgb_grid <- grid_latin_hypercube(
  trees(range = c(300, 1000)),
  tree_depth(range = c(2, 8)),
  learn_rate(range = c(-4, -1)),      # 10^-4 to 10^-1
  loss_reduction(range = c(-4, 1)),   # gamma
  sample_prop(range = c(0.60, 1.00)),
  finalize(mtry(), train_xgb |> select(-outcome_A_win)),
  min_n(range = c(5, 40)),
  size = 30
)

# -------------------------------------------------
# 5. Tune model using cross-validation
# -------------------------------------------------

xgb_tuned <- tune_grid(
  xgb_workflow,
  resamples = xgb_folds,
  grid = xgb_grid,
  metrics = metric_set(roc_auc, accuracy, mn_log_loss),
  control = control_grid(save_pred = TRUE)
)

# -------------------------------------------------
# 6. View cross-validation results
# -------------------------------------------------

xgb_cv_results <- collect_metrics(xgb_tuned)

xgb_cv_results

# Best model based on ROC AUC
best_xgb <- select_best(
  xgb_tuned,
  metric = "roc_auc"
)

best_xgb

# Performance at selected tuning values
xgb_best_metrics <- xgb_cv_results |>
  inner_join(best_xgb, by = names(best_xgb))

xgb_best_metrics

# -------------------------------------------------
# 7. Fit final boosted tree model on full training data
# -------------------------------------------------

final_xgb_workflow <- finalize_workflow(
  xgb_workflow,
  best_xgb
)

final_xgb_fit <- fit(
  final_xgb_workflow,
  data = train_xgb
)

# -------------------------------------------------
# 8. Training-set predictions
# -------------------------------------------------
# These are diagnostics only. Final evaluation should still
# happen once on the untouched test set later.

xgb_train_predictions <- predict(
  final_xgb_fit,
  new_data = train_xgb,
  type = "prob"
) |>
  bind_cols(
    predict(final_xgb_fit, new_data = train_xgb, type = "class")
  ) |>
  bind_cols(
    train_xgb |> select(outcome_A_win)
  )

# -------------------------------------------------
# 9. Training-set performance diagnostics
# -------------------------------------------------

xgb_train_metrics <- metric_set(
  roc_auc,
  accuracy,
  mn_log_loss,
  sensitivity,
  specificity
)

xgb_train_performance <- xgb_train_predictions |>
  xgb_train_metrics(
    truth = outcome_A_win,
    estimate = .pred_class,
    .pred_1,
    event_level = "second"
  )

xgb_train_performance

# Confusion matrix
xgb_train_predictions |>
  conf_mat(
    truth = outcome_A_win,
    estimate = .pred_class
  )

# -------------------------------------------------
# 10. Plot tuning results
# -------------------------------------------------

xgb_cv_results |>
  filter(.metric %in% c("roc_auc", "accuracy", "mn_log_loss")) |>
  ggplot(aes(x = trees, y = mean)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ .metric, scales = "free_y") +
  labs(
    title = "Cross-Validated XGBoost Performance",
    x = "Number of Trees",
    y = "Cross-validated metric"
  ) +
  theme_minimal()

# -------------------------------------------------
# Save Final Boosted Tree Model
# -------------------------------------------------

# Create models folder if it does not already exist
dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)

# Save final tuned XGBoost model
saveRDS(
  final_xgb_fit,
  file = here("models", "final_xgb_fit.rds")
)

# Confirmation
list.files(here("models"))
