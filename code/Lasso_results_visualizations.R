# ---------------------------------------------------------------
# LASSO Coefficient Table
# ---------------------------------------------------------------

library(tidyverse)
library(broom)
library(readr)

# -------------------------------------------------
# 1. Extract coefficients from saved model
# -------------------------------------------------

lasso_coefs <- final_lasso_fit |>
  extract_fit_parsnip() |>
  tidy() |>
  filter(term != "(Intercept)") |>
  filter(estimate != 0)

# -------------------------------------------------
# 2. Clean and organize table
# -------------------------------------------------

lasso_coef_table <- lasso_coefs |>
  mutate(
    abs_coef = abs(estimate),
    effect = case_when(
      estimate > 0 ~ "Increases Player A win odds",
      estimate < 0 ~ "Decreases Player A win odds"
    )
  ) |>
  arrange(desc(abs_coef)) |>
  select(
    Predictor = term,
    Coefficient = estimate,
    Effect = effect
  )

# View full table
lasso_coef_table

# -------------------------------------------------
# 3. Top 25 predictors
# -------------------------------------------------

lasso_top25 <- lasso_coef_table |>
  slice_head(n = 25)

lasso_top25

# -------------------------------------------------
# 4. Save tables
# -------------------------------------------------

dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(
  lasso_coef_table,
  "results/tables/lasso_all_coefficients.csv"
)

write_csv(
  lasso_top25,
  "results/tables/lasso_top25_coefficients.csv"
)

# ---------------------------------------------------------------
# LASSO Top 15 Coefficient Plot
# ---------------------------------------------------------------

library(tidyverse)
library(forcats)

# -------------------------------------------------
# 1. Select top 15 by absolute coefficient size
# -------------------------------------------------

lasso_top15_plot_data <- lasso_coef_table |>
  mutate(abs_coef = abs(Coefficient)) |>
  slice_max(
    order_by = abs_coef,
    n = 15
  ) |>
  arrange(Coefficient)

# -------------------------------------------------
# 2. Create horizontal coefficient chart
# -------------------------------------------------

# ---------------------------------------------------------------
# Cleaner LASSO Coefficient Plot
# ---------------------------------------------------------------

lasso_top15_plot_data <- lasso_coef_table |>
  mutate(
    abs_coef = abs(Coefficient),
    
    Predictor = recode(
      Predictor,
      surface_elo_diff = "Surface Elo Advantage",
      elo_diff = "Overall Elo Advantage",
      service_vs_return_matchup_diff = "Serve vs Return Matchup Edge",
      rank_diff = "Ranking Advantage",
      lag_points_1wk_diff = "ATP Points Advantage",
      na_ind_surface_win_rate_last_10_B = "Opponent Missing Surface Form",
      service_points_won_rate_last_5_diff = "Recent Service Edge",
      tourney_level_G = "Grand Slam Tournament",
      matches_played_last_14d_diff = "Recent Match Volume Edge",
      prior_surface_matches_diff = "Surface Experience Edge",
      na_ind_wins_last_5_B = "Opponent Missing Wins Data",
      return_points_won_rate_last_5_diff = "Recent Return Edge",
      lag_rank_1wk_A = "Player A Prior Rank",
      wins_last_10_diff = "Recent Wins Edge",
      prior_match_count_diff = "Head-to-Head Match Count"
    )
  ) |>
  slice_max(order_by = abs_coef, n = 15) |>
  arrange(Coefficient)

lasso_coef_plot <- ggplot(
  lasso_top15_plot_data,
  aes(
    x = Coefficient,
    y = forcats::fct_reorder(Predictor, Coefficient),
    fill = Effect
  )
) +
  geom_col(width = 0.75) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed"
  ) +
  scale_fill_manual(
    values = c(
      "Increases Player A win odds" = "#2C7FB8",
      "Decreases Player A win odds" = "#D95F0E"
    )
  ) +
  labs(
    title = "Top 15 LASSO Predictors",
    subtitle = "Positive coefficients favor Player A winning",
    x = "Standardized Logistic Coefficient",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

lasso_coef_plot

# -------------------------------------------------
# 3. Save figure
# -------------------------------------------------

ggsave(
  filename = "results/figures/lasso_top15_coefficients.pdf",
  plot = lasso_coef_plot,
  width = 10,
  height = 7
)