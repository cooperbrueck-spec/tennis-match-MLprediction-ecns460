# ---------------------------------------------------------------
# ECNS 460 Final Project
# Script: 13_final_test_set_evaluation.R
#
# Purpose:
# Evaluate the naive benchmark, LASSO model, and XGBoost model
# on the untouched test set.
# ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(rsample)
library(recipes)
library(tidymodels)
library(glmnet)
library(xgboost)
library(here)

set.seed(460)

# -------------------------------------------------
# 1. Load main modeling dataset
# -------------------------------------------------

atp_final_modeling_dataset <- read_csv(
  here("data", "cleaned", "atp_final_modeling_dataset.csv")
)

# -------------------------------------------------
# 2. Recreate same modeling data
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
# 3. Recreate same train/test split
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
# 5. Recreate preprocessing recipe
# -------------------------------------------------

tennis_recipe <- recipe(outcome_A_win ~ ., data = train_data) |>
  update_role(match_date, tourney_date, new_role = "date") |>
  step_mutate(
    prior_h2h_win_rate_A = if_else(
      is.na(prior_h2h_win_rate_A),
      0.5,
      prior_h2h_win_rate_A
    )
  ) |>
  step_indicate_na(all_of(missing_indicator_vars)) |>
  step_impute_median(all_of(missing_indicator_vars)) |>
  step_impute_median(all_of(tiny_missing_numeric_vars)) |>
  step_unknown(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

tennis_recipe_prepped <- prep(
  tennis_recipe,
  training = train_data,
  retain = TRUE
)

# -------------------------------------------------
# 6. Bake test set
# -------------------------------------------------

test_processed <- bake(
  tennis_recipe_prepped,
  new_data = test_data
)

test_model_data <- test_processed |>
  select(
    -match_date,
    -tourney_date
  )

sum(is.na(test_model_data))
dim(test_model_data)

# -------------------------------------------------
# 7. Load saved final models
# -------------------------------------------------

final_lasso_fit <- readRDS(here("models", "final_lasso_fit.rds"))
final_xgb_fit   <- readRDS(here("models", "final_xgb_fit.rds"))

# -------------------------------------------------
# 8. Naive benchmark: always predict Player A wins
# -------------------------------------------------

naive_predictions <- test_model_data |>
  transmute(
    outcome_A_win,
    .pred_class = factor("1", levels = levels(outcome_A_win)),
    .pred_0 = 0,
    .pred_1 = 1
  )

# -------------------------------------------------
# 9. LASSO test predictions
# -------------------------------------------------

lasso_test_predictions <- predict(
  final_lasso_fit,
  new_data = test_model_data,
  type = "prob"
) |>
  bind_cols(
    predict(final_lasso_fit, new_data = test_model_data, type = "class")
  ) |>
  bind_cols(
    test_model_data |> select(outcome_A_win)
  )

# -------------------------------------------------
# 10. XGBoost test predictions
# -------------------------------------------------

xgb_test_predictions <- predict(
  final_xgb_fit,
  new_data = test_model_data,
  type = "prob"
) |>
  bind_cols(
    predict(final_xgb_fit, new_data = test_model_data, type = "class")
  ) |>
  bind_cols(
    test_model_data |> select(outcome_A_win)
  )

# -------------------------------------------------
# 11. Final test-set metrics
# -------------------------------------------------

final_metrics <- metric_set(
  accuracy,
  roc_auc,
  mn_log_loss,
  sensitivity,
  specificity
)

naive_test_metrics <- naive_predictions |>
  final_metrics(
    truth = outcome_A_win,
    estimate = .pred_class,
    .pred_1,
    event_level = "second"
  ) |>
  mutate(model = "Naive: Always Pick Player A")

lasso_test_metrics <- lasso_test_predictions |>
  final_metrics(
    truth = outcome_A_win,
    estimate = .pred_class,
    .pred_1,
    event_level = "second"
  ) |>
  mutate(model = "LASSO")

xgb_test_metrics <- xgb_test_predictions |>
  final_metrics(
    truth = outcome_A_win,
    estimate = .pred_class,
    .pred_1,
    event_level = "second"
  ) |>
  mutate(model = "XGBoost")

final_test_results <- bind_rows(
  naive_test_metrics,
  lasso_test_metrics,
  xgb_test_metrics
) |>
  select(model, .metric, .estimate) |>
  pivot_wider(
    names_from = .metric,
    values_from = .estimate
  )

final_test_results

# -------------------------------------------------
# 12. Confusion matrices
# -------------------------------------------------

lasso_test_predictions |>
  conf_mat(
    truth = outcome_A_win,
    estimate = .pred_class
  )

xgb_test_predictions |>
  conf_mat(
    truth = outcome_A_win,
    estimate = .pred_class
  )

# -------------------------------------------------
# 13. Save final test results
# -------------------------------------------------

dir.create(here("results", "tables"), recursive = TRUE, showWarnings = FALSE)

write_csv(
  final_test_results,
  here("results", "tables", "final_test_model_comparison.csv")
)
