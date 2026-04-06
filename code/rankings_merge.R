library(tidyverse)
library(here)
library(data.table)

# -------------------------------------------------------------------------
# Load data
# -------------------------------------------------------------------------

atp_matches <- read_csv(here("data", "cleaned", "atp_matches_prefeature.csv"))
ranking_features <- read_csv(here("data", "cleaned", "atp_rankings_features.csv"))

# -------------------------------------------------------------------------
# Convert to data.table
# -------------------------------------------------------------------------

setDT(atp_matches)
setDT(ranking_features)

# -------------------------------------------------------------------------
# Ensure dates are Date class
# -------------------------------------------------------------------------

atp_matches[, tourney_date := as.Date(tourney_date)]
ranking_features[, ranking_date := as.Date(ranking_date)]

# -------------------------------------------------------------------------
# Add unique match id so we can merge features back cleanly
# -------------------------------------------------------------------------

atp_matches[, match_id := .I]

# -------------------------------------------------------------------------
# Keep rankings sorted for rolling join
# -------------------------------------------------------------------------

setkey(ranking_features, player_id, ranking_date)

# -------------------------------------------------------------------------
# Winner feature join
# For each match, find the most recent ranking row on or before tourney_date
# -------------------------------------------------------------------------

winner_join <- ranking_features[
  atp_matches,
  on = .(player_id = winner_id, ranking_date <= tourney_date),
  mult = "last",
  nomatch = NA,
  .(
    match_id = i.match_id,
    w_ranking_date = ranking_date,
    w_rank_feature = rank,
    w_points_feature = points,
    w_lag_rank_1wk = lag_rank_1wk,
    w_lag_rank_4wk = lag_rank_4wk,
    w_lag_points_1wk = lag_points_1wk,
    w_lag_points_4wk = lag_points_4wk,
    w_rank_change_1wk = rank_change_1wk,
    w_rank_change_4wk = rank_change_4wk,
    w_points_change_1wk = points_change_1wk,
    w_points_change_4wk = points_change_4wk,
    w_avg_rank_4wk = avg_rank_4wk,
    w_avg_rank_8wk = avg_rank_8wk,
    w_sd_rank_8wk = sd_rank_8wk,
    w_avg_points_4wk = avg_points_4wk,
    w_avg_points_8wk = avg_points_8wk,
    w_sd_points_8wk = sd_points_8wk
  )
]

# -------------------------------------------------------------------------
# Loser feature join
# -------------------------------------------------------------------------

loser_join <- ranking_features[
  atp_matches,
  on = .(player_id = loser_id, ranking_date <= tourney_date),
  mult = "last",
  nomatch = NA,
  .(
    match_id = i.match_id,
    l_ranking_date = ranking_date,
    l_rank_feature = rank,
    l_points_feature = points,
    l_lag_rank_1wk = lag_rank_1wk,
    l_lag_rank_4wk = lag_rank_4wk,
    l_lag_points_1wk = lag_points_1wk,
    l_lag_points_4wk = lag_points_4wk,
    l_rank_change_1wk = rank_change_1wk,
    l_rank_change_4wk = rank_change_4wk,
    l_points_change_1wk = points_change_1wk,
    l_points_change_4wk = points_change_4wk,
    l_avg_rank_4wk = avg_rank_4wk,
    l_avg_rank_8wk = avg_rank_8wk,
    l_sd_rank_8wk = sd_rank_8wk,
    l_avg_points_4wk = avg_points_4wk,
    l_avg_points_8wk = avg_points_8wk,
    l_sd_points_8wk = sd_points_8wk
  )
]

# -------------------------------------------------------------------------
# Merge winner and loser features back to original match data
# -------------------------------------------------------------------------

match_weather_ranking_data <- merge(atp_matches, winner_join, by = "match_id", all.x = TRUE)
match_weather_ranking_data <- merge(match_weather_ranking_data, loser_join, by = "match_id", all.x = TRUE)

# -------------------------------------------------------------------------
# matchup features
# -------------------------------------------------------------------------

match_weather_ranking_data[, rank_change_4wk_diff := w_rank_change_4wk - l_rank_change_4wk]
match_weather_ranking_data[, avg_rank_8wk_diff := w_avg_rank_8wk - l_avg_rank_8wk]
match_weather_ranking_data[, avg_points_8wk_diff := w_avg_points_8wk - l_avg_points_8wk]

# -------------------------------------------------------------------------
# Sanity checks
# -------------------------------------------------------------------------

stopifnot(nrow(match_weather_ranking_data) == nrow(atp_matches))

# -------------------------------------------------------------------------
# Convert back to tibble
# -------------------------------------------------------------------------

match_weather_ranking_data <- as_tibble(match_weather_ranking_data)

# We now have a complete data set with weather, match, and ranking data 
# Merging rankings was done this way to ensure no data leakage

# ------------
# save data
# ------------

write_csv(
  match_weather_ranking_data,
  here("data", "cleaned", "atp_matches_weather_ranking_data.csv")
)


