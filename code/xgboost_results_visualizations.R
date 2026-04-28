# ---------------------------------------------------------------
# Improved XGBoost Feature Importance Table and Plot
# ---------------------------------------------------------------

library(tidyverse)
library(xgboost)
library(tidymodels)
library(forcats)
library(readr)

# -------------------------------------------------
# 1. Extract XGBoost engine from fitted workflow
# -------------------------------------------------

xgb_engine <- final_xgb_fit |>
  extract_fit_engine()

# -------------------------------------------------
# 2. Extract feature importance
# -------------------------------------------------

xgb_importance <- xgb.importance(
  model = xgb_engine
) |>
  as_tibble()

# -------------------------------------------------
# 3. Clean predictor names
# -------------------------------------------------

xgb_importance_clean <- xgb_importance |>
  mutate(
    readable_feature = recode(
      Feature,
      elo_diff = "Overall Elo Advantage",
      rank_ratio = "Ranking Ratio",
      surface_elo_diff = "Surface Elo Advantage",
      service_vs_return_matchup_diff = "Serve vs Return Matchup Edge",
      service_points_won_rate_last_5_diff = "Recent Service Points Won Edge",
      prior_surface_matches_B = "Opponent Prior Surface Matches",
      return_vs_service_matchup_diff = "Return vs Service Matchup Edge",
      prior_match_count_diff = "Prior Match Count Advantage",
      tourney_level_G = "Grand Slam Tournament",
      lag_points_1wk_diff = "ATP Points Advantage",
      sd_points_8wk_B = "Opponent Ranking Points Volatility",
      matches_played_last_14d_diff = "Recent Match Volume Advantage",
      surface_elo_A = "Player A Surface Elo",
      prior_surface_matches_A = "Player A Prior Surface Matches",
      service_points_won_rate_last_5_A = "Recent Service Points Won Player A",
      rank_change_4wk_B = "Opponent 4-Week Rank Change",
      wins_last_10_diff = "Recent Wins Advantage",
      prior_match_count_A = "Player A Prior Match Count",
      best_of = "Best-of Sets",
      first_serve_points_won_rate_last_5_diff = "Recent First-Serve Points Won Edge",
      .default = Feature
    )
  )

# Save full importance table
dir.create("results/tables", recursive = TRUE, showWarnings = FALSE)

write_csv(
  xgb_importance_clean,
  "results/tables/xgboost_feature_importance.csv"
)

# -------------------------------------------------
# 4. Plot top 20 features by gain
# -------------------------------------------------

xgb_importance_plot_data <- xgb_importance_clean |>
  slice_max(order_by = Gain, n = 20) |>
  arrange(Gain)

xgb_importance_plot <- ggplot(
  xgb_importance_plot_data,
  aes(
    x = Gain,
    y = fct_reorder(readable_feature, Gain)
  )
) +
  geom_col(width = 0.75) +
  labs(
    title = "Top XGBoost Predictors by Feature Importance",
    subtitle = "Importance measured by gain from tree splits",
    x = "Gain",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

xgb_importance_plot

dir.create("results/figures", recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = "results/figures/xgboost_feature_importance.pdf",
  plot = xgb_importance_plot,
  width = 10,
  height = 7
)