# -------------------------------------------------------------------------
# Ranking Feature Construction
#
# This script constructs player-level ranking features from ATP rankings
# data to capture recent performance, momentum, and consistency over time.
#
# The raw rankings dataset is a weekly panel with one observation per
# player_id and ranking_date, containing rank and ranking points.
#
# The script performs the following steps:
# 1. Sorts the data by player_id and ranking_date to ensure correct
#    time ordering within each player.
# 2. Groups the data by player_id so that all calculations are done
#    within each player’s ranking history.
# 3. Creates lagged features (1 week and 4 weeks) for rank and points,
#    representing prior performance.
# 4. Computes change (momentum) variables by comparing past rankings
#    to current rankings. Positive values indicate improvement
#    (i.e., a lower rank number).
# 5. Constructs rolling averages and standard deviations over 4-week
#    and 8-week windows using only prior observations. These capture
#    recent performance level and volatility while avoiding data leakage.
#
# The resulting dataset remains at the player-week level and will later
# be merged into the match-level dataset by matching each player to the
# most recent ranking observation prior to each match date. The Most important
# thing is I avoid data leakage. We use lags becuase nearest ranking to match 
# data must be nearest prior ranking to match date.
# -------------------------------------------------------------------------
library(tidyverse)
library(here)
library(slider)

# Load data ---------------------------------------------------------------

atp_matches <- read_csv(here("data", "cleaned", "atp_matches_with_nasa_weather.csv"))
rankings <- read_csv(here("data", "cleaned", "atp_rankings_all.csv"))
players <- read_csv(here("data", "cleaned", "atp_players_clean.csv"))

# Make sure dates are real dates -----------------------------------------

rankings <- rankings |>
  mutate(ranking_date = as.Date(ranking_date))

# Sort rankings -----------------------------------------------------------

rankings <- rankings |>
  arrange(player_id, ranking_date)

# Create ranking features -------------------------------------------------
# Important:
# lower rank number = better rank
# so a positive "improvement" means rank got better over time (lag_rank_4wk - rank)

ranking_features <- rankings |>
  group_by(player_id) |>
  arrange(ranking_date, .by_group = TRUE) |>
  mutate(
    lag_rank_1wk = lag(rank, 1),
    lag_rank_4wk = lag(rank, 4),
    lag_points_1wk = lag(points, 1),
    lag_points_4wk = lag(points, 4),
    
    rank_change_1wk = lag_rank_1wk - rank,
    rank_change_4wk = lag_rank_4wk - rank,
    points_change_1wk = points - lag_points_1wk,
    points_change_4wk = points - lag_points_4wk,
    
    avg_rank_4wk = slide_dbl(
      lag(rank, 1),
      ~ mean(.x, na.rm = TRUE),
      .before = 3,
      .complete = TRUE
    ),
    avg_rank_8wk = slide_dbl(
      lag(rank, 1),
      ~ mean(.x, na.rm = TRUE),
      .before = 7,
      .complete = TRUE
    ),
    sd_rank_8wk = slide_dbl(
      lag(rank, 1),
      ~ sd(.x, na.rm = TRUE),
      .before = 7,
      .complete = TRUE
    ),
    
    avg_points_4wk = slide_dbl(
      lag(points, 1),
      ~ mean(.x, na.rm = TRUE),
      .before = 3,
      .complete = TRUE
    ),
    avg_points_8wk = slide_dbl(
      lag(points, 1),
      ~ mean(.x, na.rm = TRUE),
      .before = 7,
      .complete = TRUE
    ),
    sd_points_8wk = slide_dbl(
      lag(points, 1),
      ~ sd(.x, na.rm = TRUE),
      .before = 7,
      .complete = TRUE
    )
  ) |>
  ungroup()

# Quick checks ------------------------------------------------------------

glimpse(ranking_features)

# Notice relatively low missing data. Likely from new players entering.
ranking_features |>
  summarise(
    n_rows = n(),
    pct_missing_lag_rank_1wk = mean(is.na(lag_rank_1wk)) * 100,
    pct_missing_lag_rank_4wk = mean(is.na(lag_rank_4wk)) * 100,
    pct_missing_avg_rank_4wk = mean(is.na(avg_rank_4wk)) * 100,
    pct_missing_avg_rank_8wk = mean(is.na(avg_rank_8wk)) * 100
  )

ranking_features |>
  select(
    player_id, ranking_date, rank, points,
    lag_rank_1wk, lag_rank_4wk,
    rank_change_1wk, rank_change_4wk,
    avg_rank_4wk, avg_rank_8wk, sd_rank_8wk,
    avg_points_4wk, avg_points_8wk, sd_points_8wk
  ) |>
  arrange(player_id, ranking_date) |>
  print(n = 20)

# Save --------------------------------------------------------------------

write_csv(
  ranking_features,
  here("data", "cleaned", "atp_rankings_features.csv")
)