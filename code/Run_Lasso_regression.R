
library(tidyverse)
library(rsample)
library(readr)
library(recipes)

atp_final_modeling_dataset <- read_csv(
  "C:/Users/coope/OneDrive - Montana State University/Desktop/ECNS 460/Tennis_match_ML/tennis-match-MLprediction-ecns460/data/cleaned/atp_final_modeling_dataset.csv"
)

set.seed(460)

# -------------------------------------------------
# 1. Keep all variables that could be known before
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
# 2. Split into training and testing samples.
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
# 3. Identify missingness 
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
# 4. Build preprocessing recipe.
# -------------------------------------------------

tennis_recipe <- recipe(outcome_A_win ~ ., data = train_data) |>
  
  # Dates are kept in the data but not used as model predictors.
  # They are not unique IDs, so we assign them a separate role.
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
  
  # Convert categorical predictors into dummy variables.
  step_dummy(all_nominal_predictors()) |>
  
  # Remove predictors with no variation.
  step_zv(all_predictors()) |>
  
  # Normalize numeric predictors for LASSO.
  step_normalize(all_numeric_predictors())

# -------------------------------------------------
# 5. Prep and apply recipe to TRAINING DATA ONLY.
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
# 6. Training data checks
# -------------------------------------------------

sum(is.na(train_processed))
dim(train_processed)

train_processed |>
  count(outcome_A_win) |>
  mutate(prop = n / sum(n))

# ------------------------------------------------
# Run LASSO Regression
# ------------------------------------------------

library(tidymodels)
library(glmnet)


set.seed(460)

# -------------------------------------------------
# 0. Remove date variables before modeling
# -------------------------------------------------
# These dates were kept through preprocessing but should not be used
# as raw predictors in the LASSO model.

train_lasso <- train_processed |>
  select(
    -match_date,
    -tourney_date
  )

# -------------------------------------------------
# 1. Define LASSO model
# -------------------------------------------------
# penalty = tune() lets cross-validation choose lambda.
# mixture = 1 means pure LASSO.
# logistic_reg() is used because outcome_A_win is binary.

lasso_model <- logistic_reg(
  penalty = tune(),
  mixture = 1
) |>
  set_engine("glmnet")

# -------------------------------------------------
# 2. 5-fold cross-validation on training data only
# -------------------------------------------------

cv_folds <- vfold_cv(
  train_lasso,
  v = 5,
  strata = outcome_A_win
)

# -------------------------------------------------
# 3. Workflow
# -------------------------------------------------

lasso_workflow <- workflow() |>
  add_model(lasso_model) |>
  add_formula(outcome_A_win ~ .)

# -------------------------------------------------
# 4. Grid of lambda values to test
# -------------------------------------------------

lambda_grid <- grid_regular(
  penalty(range = c(-4, 1)),   # 10^-4 to 10^1
  levels = 50
)

# -------------------------------------------------
# 5. Tune model using cross-validation
# -------------------------------------------------
# ROC AUC: main performance measure
# accuracy: easy-to-explain classification performance
# mn_log_loss: rewards well-calibrated predicted probabilities

lasso_tuned <- tune_grid(
  lasso_workflow,
  resamples = cv_folds,
  grid = lambda_grid,
  metrics = metric_set(roc_auc, accuracy, mn_log_loss),
  control = control_grid(save_pred = TRUE)
)

# -------------------------------------------------
# 6. View cross-validation results
# -------------------------------------------------

lasso_cv_results <- collect_metrics(lasso_tuned)

lasso_cv_results

# Best lambda based on ROC AUC
best_lambda <- select_best(
  lasso_tuned,
  metric = "roc_auc"
)

best_lambda

# View performance at the selected lambda
lasso_best_metrics <- lasso_cv_results |>
  inner_join(best_lambda, by = "penalty")

lasso_best_metrics

# -------------------------------------------------
# 7. Optional: plot performance across lambda values
# -------------------------------------------------

lasso_cv_results |>
  filter(.metric %in% c("roc_auc", "accuracy", "mn_log_loss")) |>
  ggplot(aes(x = penalty, y = mean)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ .metric, scales = "free_y") +
  scale_x_log10() +
  labs(
    title = "Cross-Validated LASSO Performance Across Lambda Values",
    x = "Penalty parameter, lambda",
    y = "Cross-validated metric"
  ) +
  theme_minimal()

# -------------------------------------------------
# 8. Fit final LASSO model on full training data
# -------------------------------------------------

final_lasso_workflow <- finalize_workflow(
  lasso_workflow,
  best_lambda
)

final_lasso_fit <- fit(
  final_lasso_workflow,
  data = train_lasso
)

# -------------------------------------------------
# 9. Training-set predictions
# -------------------------------------------------
# These are not final test-set results.
# They help confirm that the model is fitting correctly.

lasso_train_predictions <- predict(
  final_lasso_fit,
  new_data = train_lasso,
  type = "prob"
) |>
  bind_cols(
    predict(final_lasso_fit, new_data = train_lasso, type = "class")
  ) |>
  bind_cols(
    train_lasso |> select(outcome_A_win)
  )

# -------------------------------------------------
# 10. Training-set performance
# -------------------------------------------------
# Use cross-validation metrics for model selection.
# Use these training metrics only as a diagnostic.

lasso_train_metrics <- metric_set(
  roc_auc,
  accuracy,
  mn_log_loss,
  sensitivity,
  specificity
)

lasso_train_performance <- lasso_train_predictions |>
  lasso_train_metrics(
    truth = outcome_A_win,
    estimate = .pred_class,
    .pred_1,
    event_level = "second"
  )

lasso_train_performance

# Confusion matrix on training data
lasso_train_predictions |>
  conf_mat(
    truth = outcome_A_win,
    estimate = .pred_class
  )

# -------------------------------------------------
# 11. Extract non-zero coefficients
# -------------------------------------------------

lasso_coefs <- final_lasso_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(estimate != 0) |>
  arrange(desc(abs(estimate)))

lasso_coefs

# -------------------------------------------------
# 12. Number of predictors selected
# -------------------------------------------------

n_selected_predictors <- lasso_coefs |>
  filter(term != "(Intercept)") |>
  nrow()

n_selected_predictors

# -------------------------------------------------
# 13. Top predictors by absolute coefficient size
# -------------------------------------------------

top_lasso_predictors <- lasso_coefs |>
  filter(term != "(Intercept)") |>
  mutate(abs_estimate = abs(estimate)) |>
  arrange(desc(abs_estimate)) |>
  slice_head(n = 25)

top_lasso_predictors

train_lasso |>
  count(outcome_A_win) |>
  mutate(prop = n / sum(n))
baseline_accuracy <- train_lasso |>
  summarise(acc = mean(outcome_A_win == "1"))

baseline_accuracy

# -------------------------------------------------
# Save Final LASSO Model
# -------------------------------------------------

# Create models folder if it does not already exist
dir.create("models", showWarnings = FALSE)

# Save final tuned LASSO model
saveRDS(
  final_lasso_fit,
  file = "models/final_lasso_fit.rds"
)

# Confirmation
list.files("models")
