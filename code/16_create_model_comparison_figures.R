# ---------------------------------------------------------------
# Results Figures for Model Comparison
# ---------------------------------------------------------------

library(tidyverse)
library(readr)
library(scales)
library(here)

# -------------------------------------------------
# 1. Load final model comparison table
# -------------------------------------------------

final_test_results <- read_csv(
  here("results", "tables", "final_test_model_comparison.csv")
)

final_test_results

# -------------------------------------------------
# 2. Reshape results for plotting
# -------------------------------------------------

model_results_long <- final_test_results |>
  pivot_longer(
    cols = c(accuracy, sensitivity, specificity, roc_auc, mn_log_loss),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    model = factor(
      model,
      levels = c(
        "Naive: Always Pick Player A",
        "LASSO",
        "XGBoost"
      )
    ),
    metric = recode(
      metric,
      accuracy = "Accuracy",
      sensitivity = "Sensitivity",
      specificity = "Specificity",
      roc_auc = "ROC AUC",
      mn_log_loss = "Mean Log Loss"
    )
  )

# -------------------------------------------------
# Horizontal Classification Metrics Plot
# -------------------------------------------------

classification_metrics_plot <- model_results_long |>
  filter(metric != "Mean Log Loss") |>
  ggplot(aes(
    y = fct_rev(model),
    x = value,
    fill = model
  )) +
  geom_col(width = 0.70, show.legend = FALSE) +
  geom_text(
    aes(label = round(value, 3)),
    hjust = -0.10,
    size = 3.5
  ) +
  facet_wrap(~ metric, ncol = 2) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.05)
  ) +
  labs(
    title = "Final Test Set Performance by Model",
    subtitle = "Player A defined as higher-ranked player before the match",
    x = "Metric Value",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

classification_metrics_plot

# -------------------------------------------------
# Horizontal Log Loss Plot
# -------------------------------------------------

log_loss_plot <- model_results_long |>
  filter(metric == "Mean Log Loss") |>
  ggplot(aes(
    y = fct_rev(model),
    x = value,
    fill = model
  )) +
  geom_col(width = 0.70, show.legend = FALSE) +
  geom_text(
    aes(label = round(value, 3)),
    hjust = -0.10,
    size = 3.5
  ) +
  labs(
    title = "Final Test Set Mean Log Loss",
    subtitle = "Lower values indicate better calibrated probabilities",
    x = "Mean Log Loss",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

log_loss_plot

# -------------------------------------------------
# 5. Save figures
# -------------------------------------------------

dir.create(here("results", "figures"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = here("results", "figures", "final_model_classification_metrics.pdf"),
  plot = classification_metrics_plot,
  width = 11,
  height = 5
)

ggsave(
  filename = here("results", "figures", "final_model_log_loss.pdf"),
  plot = log_loss_plot,
  width = 8,
  height = 5
)
