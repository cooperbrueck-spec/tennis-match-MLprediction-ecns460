# ---------------------------------------------------------------
#  Create Probability Calibration Plot
# Compare LASSO vs XGBoost on Test Set
# ---------------------------------------------------------------

library(tidyverse)
library(scales)
library(here)

dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)

# -------------------------------------------------
# 1. Combine prediction sets
# -------------------------------------------------

calibration_data <- bind_rows(
  lasso_test_predictions |>
    transmute(
      model = "LASSO",
      outcome_A_win,
      pred_prob = .pred_1
    ),
  
  xgb_test_predictions |>
    transmute(
      model = "XGBoost",
      outcome_A_win,
      pred_prob = .pred_1
    )
) |>
  mutate(
    actual_win = as.numeric(as.character(outcome_A_win))
  )

# -------------------------------------------------
# 2. Create probability bins
# -------------------------------------------------
# Deciles: 0-10%, 10-20%, etc.

calibration_summary <- calibration_data |>
  mutate(
    prob_bin = cut(
      pred_prob,
      breaks = seq(0, 1, by = 0.10),
      include.lowest = TRUE
    )
  ) |>
  group_by(model, prob_bin) |>
  summarise(
    mean_pred = mean(pred_prob, na.rm = TRUE),
    actual_rate = mean(actual_win, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(!is.na(mean_pred))

# -------------------------------------------------
# 3. Calibration plot
# -------------------------------------------------

calibration_plot <- ggplot(
  calibration_summary,
  aes(x = mean_pred, y = actual_rate, color = model)
) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 1,
    alpha = 0.7
  ) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  facet_wrap(~ model) +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Probability Calibration on Test Set",
    subtitle = "Closer to the 45-degree line indicates better calibrated win probabilities",
    x = "Average Predicted Probability Player A Wins",
    y = "Actual Frequency Player A Wins"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

calibration_plot

# -------------------------------------------------
# 4. Save figure
# -------------------------------------------------

ggsave(
  filename = here("results", "figures", "probability_calibration_plot.pdf"),
  plot = calibration_plot,
  width = 10,
  height = 6
)


# -------------------------------------------------
# 1. Combine predictions with rank difference
# -------------------------------------------------
# Because Player A is defined as the higher-ranked player,
# rank_diff measures the ranking gap between Player A and Player B.

rank_accuracy_data <- bind_rows(
  naive_predictions |>
    transmute(
      model = "Naive: Always Pick Player A",
      outcome_A_win,
      .pred_class
    ),
  
  lasso_test_predictions |>
    transmute(
      model = "LASSO",
      outcome_A_win,
      .pred_class
    ),
  
  xgb_test_predictions |>
    transmute(
      model = "XGBoost",
      outcome_A_win,
      .pred_class
    )
) |>
  bind_cols(
    bind_rows(
      test_model_data |> select(rank_diff),
      test_model_data |> select(rank_diff),
      test_model_data |> select(rank_diff)
    )
  ) |>
  mutate(
    rank_gap = abs(rank_diff),
    correct = .pred_class == outcome_A_win
  )

# -------------------------------------------------
# 2. Create ranking-difference bins
# -------------------------------------------------

rank_accuracy_summary <- rank_accuracy_data |>
  mutate(
    rank_gap_bin = case_when(
      rank_gap <= 10 ~ "0 to 10",
      rank_gap <= 25 ~ "11 to 25",
      rank_gap <= 50 ~ "26 to 50",
      rank_gap <= 100 ~ "51 to 100",
      rank_gap > 100 ~ "100+"
    ),
    rank_gap_bin = factor(
      rank_gap_bin,
      levels = c("0 to 10", "11 to 25", "26 to 50", "51 to 100", "100+")
    ),
    model = factor(
      model,
      levels = c("Naive: Always Pick Player A", "LASSO", "XGBoost")
    )
  ) |>
  group_by(model, rank_gap_bin) |>
  summarise(
    accuracy = mean(correct),
    n = n(),
    .groups = "drop"
  )

rank_accuracy_summary

# -------------------------------------------------
# 3. Plot accuracy by ranking gap
# -------------------------------------------------

rank_accuracy_plot <- ggplot(
  rank_accuracy_summary,
  aes(x = rank_gap_bin, y = accuracy, group = model, color = model)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(
    aes(label = percent(accuracy, accuracy = 0.1)),
    vjust = -0.75,
    size = 3,
    show.legend = FALSE
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0.45, 0.85)
  ) +
  labs(
    title = "Model Accuracy by Ranking Difference",
    subtitle = "Accuracy generally rises as the ranking gap between players increases",
    x = "Ranking Gap Between Player A and Player B",
    y = "Accuracy",
    color = "Model"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

rank_accuracy_plot

# -------------------------------------------------
# 4. Save figure
# -------------------------------------------------

ggsave(
  filename = here("results", "figures", "accuracy_by_rank_gap.pdf"),
  plot = rank_accuracy_plot,
  width = 9,
  height = 6
)

# ---------------------------------------------------------------
# Confusion Matrix Heatmaps figure
# ---------------------------------------------------------------

# -------------------------------------------------
# 1. Combine LASSO and XGBoost predictions
# -------------------------------------------------

confusion_data <- bind_rows(
  lasso_test_predictions |>
    transmute(
      model = "LASSO",
      actual = outcome_A_win,
      predicted = .pred_class
    ),
  
  xgb_test_predictions |>
    transmute(
      model = "XGBoost",
      actual = outcome_A_win,
      predicted = .pred_class
    )
) |>
  mutate(
    actual = recode(
      as.character(actual),
      "0" = "Player A loses\nUpset",
      "1" = "Player A wins\nFavorite wins"
    ),
    predicted = recode(
      as.character(predicted),
      "0" = "Predicted Player A loses",
      "1" = "Predicted Player A wins"
    ),
    actual = factor(
      actual,
      levels = c("Player A loses\nUpset", "Player A wins\nFavorite wins")
    ),
    predicted = factor(
      predicted,
      levels = c("Predicted Player A loses", "Predicted Player A wins")
    )
  )

# -------------------------------------------------
# 2. Calculate row percentages
# -------------------------------------------------
# Row percentages answer:
# "Among matches with this actual outcome, how often did the model
# predict each class?"

confusion_summary <- confusion_data |>
  count(model, actual, predicted, name = "n") |>
  group_by(model, actual) |>
  mutate(
    row_total = sum(n),
    percent = n / row_total,
    label = paste0(
      percent(percent, accuracy = 0.1),
      "\n(n = ", n, ")"
    )
  ) |>
  ungroup()

confusion_summary

# -------------------------------------------------
# 3. Plot confusion matrix heatmaps
# -------------------------------------------------

confusion_heatmap <- ggplot(
  confusion_summary,
  aes(x = predicted, y = actual, fill = percent)
) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(
    aes(label = label),
    size = 4,
    fontface = "bold"
  ) +
  facet_wrap(~ model) +
  scale_fill_gradient(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Confusion Matrix Heatmaps on Test Set",
    subtitle = "Cells show row percentages within each actual match outcome",
    x = "Predicted Outcome",
    y = "Actual Outcome",
    fill = "Row %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

confusion_heatmap

# -------------------------------------------------
# 4. Save figure
# -------------------------------------------------

ggsave(
  filename = here("results", "figures", "confusion_matrix_heatmaps.pdf"),
  plot = confusion_heatmap,
  width = 10,
  height = 6
)
