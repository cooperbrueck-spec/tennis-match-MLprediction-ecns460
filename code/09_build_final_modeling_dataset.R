# build_final_modeling_dataset_commented.R
# -----------------------------------------------------------------------------
# Purpose:
# This script constructs the final match-level modeling dataset for the tennis
# winner-prediction project.
#
# Design:
# - One row per match
# - Player A is always the higher-ranked player before the match
# - The target is whether Player A won
# - All engineered predictors are intended to be pre-match only
#
# Main outputs:
# - data/cleaned/atp_final_modeling_dataset.csv
# - data/cleaned/atp_final_modeling_data_dictionary.csv
# - results/tables/final_modeling_dataset_summary.md
# -----------------------------------------------------------------------------

# Clear the workspace at the start so this script does not depend on leftover objects from a prior R session.
# This improves reproducibility because every run starts from the same clean environment.
rm(list = ls())

# Load the packages used in the build process.
suppressPackageStartupMessages({
  library(data.table)
  library(readr)
  library(here)
})

# Define the primary input and output paths.
# Using file.path keeps these paths platform-safe and easier to reproduce across machines.
input_path <- here("data", "cleaned", "atp_matches_weather_ranking_data.csv")
output_dataset_path <- here("data", "cleaned", "atp_final_modeling_dataset.csv")
output_dictionary_path <- here("data", "cleaned", "atp_final_modeling_data_dictionary.csv")

# Divide safely so undefined ratios become missing values rather than Inf or NaN.
# This is important for rate features such as win rates and serve rates when denominators are zero or unavailable.
safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  result[is.nan(result) | is.infinite(result)] <- NA_real_
  result[denominator <= 0 | is.na(denominator)] <- NA_real_
  result
}

# Count how many matches a player completed within a fixed lookback window before each current match.
# The function uses only earlier dates in the sorted player history, which preserves the pre-match design.
count_prior_matches_in_days <- function(match_dates, days_back) {
  n <- length(match_dates)
  output <- integer(n)
  left <- 1L

  if (n == 0L) {
    return(output)
  }

  for (i in seq_len(n)) {
    while (left < i && as.integer(match_dates[i] - match_dates[left]) > days_back) {
      left <- left + 1L
    }

    output[i] <- i - left
  }

  output
}

# Define the serve and return rate features that will be built from rolling numerator and denominator sums.
# This table centralizes the formulas so the script can apply the same logic consistently across all rolling windows.
rate_window_specs <- data.table(
  feature = c(
    'ace_rate',
    'double_fault_rate',
    'first_serve_in_rate',
    'first_serve_points_won_rate',
    'second_serve_points_won_rate',
    'service_points_won_rate',
    'break_points_saved_rate',
    'ace_rate_allowed',
    'opp_first_serve_in_rate_allowed',
    'return_points_won_rate',
    'break_points_converted_rate'
  ),
  numerator = c(
    'ace',
    'df',
    'first_in',
    'first_won',
    'second_won',
    'service_points_won',
    'bp_saved',
    'opp_ace',
    'opp_first_in',
    'return_points_won',
    'bp_converted'
  ),
  denominator = c(
    'svpt',
    'svpt',
    'svpt',
    'first_in',
    'second_serve_points',
    'svpt',
    'bp_faced',
    'opp_svpt',
    'opp_svpt',
    'opp_svpt',
    'opp_bp_faced'
  )
)

# Create rolling recent-form and recent-performance features for each player.
# All rolling features are based on shift(..., 1L), which excludes the current match from its own predictors.
# That is the main safeguard against leakage in these rolling summaries.
add_recent_rate_features <- function(player_history, windows = c(5L, 10L)) {
  player_history[, prior_match_count := seq_len(.N) - 1L, by = player_id]

  for (window_size in windows) {
    player_history[, (paste0('wins_last_', window_size)) := fifelse(
      prior_match_count >= window_size,
      frollsum(shift(won, 1L), n = window_size, align = 'right', na.rm = TRUE),
      NA_real_
    ), by = player_id]

    player_history[, (paste0('win_rate_last_', window_size)) := fifelse(
      prior_match_count >= window_size,
      frollmean(shift(won, 1L), n = window_size, align = 'right', na.rm = TRUE),
      NA_real_
    ), by = player_id]

    for (row_index in seq_len(nrow(rate_window_specs))) {
      feature_name <- rate_window_specs$feature[row_index]
      numerator_name <- rate_window_specs$numerator[row_index]
      denominator_name <- rate_window_specs$denominator[row_index]
      output_name <- paste0(feature_name, '_last_', window_size)

      player_history[, (output_name) := {
        numerator_sum <- frollsum(shift(get(numerator_name), 1L), n = window_size, align = 'right', na.rm = TRUE)
        denominator_sum <- frollsum(shift(get(denominator_name), 1L), n = window_size, align = 'right', na.rm = TRUE)
        value <- safe_divide(numerator_sum, denominator_sum)
        value[prior_match_count < window_size] <- NA_real_
        value
      }, by = player_id]
    }
  }

  player_history
}

# Read the cleaned match-level input file and convert it to a data.table for fast grouped operations.
matches <- read_csv(input_path, show_col_types = FALSE)
setDT(matches)

# Standardize the date fields before any chronological sorting or feature engineering.
# This ensures date comparisons and rolling time windows behave correctly.
matches[, `:=`(
  match_date = as.Date(match_date),
  tourney_date = as.Date(tourney_date)
)]

# Store the original row number as a final deterministic tie-breaker in case two observations are otherwise identical in ordering fields.
matches[, row_in_input := .I]

# Translate round labels into a numeric order so same-day matches can be processed in a consistent order.
round_levels <- c('RR', 'R128', 'R64', 'R32', 'R16', 'QF', 'SF', 'BR', 'F')
matches[, round_order := match(round, round_levels)]
matches[is.na(round_order), round_order := length(round_levels) + 1L]

# Establish a match processing order.
# This order is used later for rolling features, Elo updates, and prior head-to-head counts.
setorder(matches, match_date, tourney_date, tourney_id, round_order, match_num, match_id, row_in_input)
matches[, match_order := .I]

# Identify matches where both sides have usable pre-match ATP rankings.
# Positive ranks are treated as valid. Missing or nonpositive ranks make A/B assignment impossible.
matches[, `:=`(
  valid_winner_rank = !is.na(winner_rank) & winner_rank > 0,
  valid_loser_rank = !is.na(loser_rank) & loser_rank > 0
)]

# Determine whether the winner or loser entered the match as the higher-ranked player.
# This is the key step that later defines Player A as the favorite based on rank.No OBS are dropped
matches[, higher_rank_side := fifelse(
  valid_winner_rank & valid_loser_rank & winner_rank < loser_rank,
  'winner',
  fifelse(
    valid_winner_rank & valid_loser_rank & loser_rank < winner_rank,
    'loser',
    NA_character_
  )
)]

# Keep only matches where the higher-ranked player can be uniquely identified.
# Matches with missing ranks or tied ranks are excluded because the target setup depends on a clear Player A. No OBS is dropped
matches[, usable_for_model := !is.na(higher_rank_side)]
matches[, drop_reason := fifelse(
  usable_for_model,
  NA_character_,
  fifelse(
    !valid_winner_rank | !valid_loser_rank,
    'missing_or_invalid_rank',
    'rank_tie'
  )
)]

# Restrict the history-building data to matches with both player identifiers present.
# Player IDs are required for chronological player histories, Elo, and head-to-head construction.
history_source <- matches[!is.na(winner_id) & !is.na(loser_id)]

# Build a player-history view from the winner side.
# Each row represents what the winner contributed in that match, along with opponent information needed for return features.
winner_history <- history_source[, .(
  match_id,
  match_order,
  match_date,
  surface,
  player_id = winner_id,
  player_name = winner_name,
  opponent_id = loser_id,
  opponent_name = loser_name,
  won = 1,
  ace = as.numeric(w_ace),
  df = as.numeric(w_df),
  svpt = as.numeric(w_svpt),
  first_in = as.numeric(w_1stIn),
  first_won = as.numeric(w_1stWon),
  second_won = as.numeric(w_2ndWon),
  bp_saved = as.numeric(w_bpSaved),
  bp_faced = as.numeric(w_bpFaced),
  opp_ace = as.numeric(l_ace),
  opp_svpt = as.numeric(l_svpt),
  opp_first_in = as.numeric(l_1stIn),
  opp_first_won = as.numeric(l_1stWon),
  opp_second_won = as.numeric(l_2ndWon),
  opp_bp_saved = as.numeric(l_bpSaved),
  opp_bp_faced = as.numeric(l_bpFaced)
)]

# Build the symmetric player-history view from the loser side.
# Stacking winner_history and loser_history later creates one player-level row per player-match appearance.
loser_history <- history_source[, .(
  match_id,
  match_order,
  match_date,
  surface,
  player_id = loser_id,
  player_name = loser_name,
  opponent_id = winner_id,
  opponent_name = winner_name,
  won = 0,
  ace = as.numeric(l_ace),
  df = as.numeric(l_df),
  svpt = as.numeric(l_svpt),
  first_in = as.numeric(l_1stIn),
  first_won = as.numeric(l_1stWon),
  second_won = as.numeric(l_2ndWon),
  bp_saved = as.numeric(l_bpSaved),
  bp_faced = as.numeric(l_bpFaced),
  opp_ace = as.numeric(w_ace),
  opp_svpt = as.numeric(w_svpt),
  opp_first_in = as.numeric(w_1stIn),
  opp_first_won = as.numeric(w_1stWon),
  opp_second_won = as.numeric(w_2ndWon),
  opp_bp_saved = as.numeric(w_bpSaved),
  opp_bp_faced = as.numeric(w_bpFaced)
)]

# Combine winner-side and loser-side records into a unified player-level history table.
# This table is the foundation for rolling recent-form, serve, return, and surface features.
player_history <- rbindlist(list(winner_history, loser_history), use.names = TRUE)
setorder(player_history, player_id, match_date, match_order)

# Create derived tennis counting stats that are needed before rate construction.
# These are still match-level components at the player-history stage and will later be aggregated over prior matches only.
player_history[, `:=`(
  service_points_won = first_won + second_won,
  second_serve_points = svpt - first_in,
  return_points_won = opp_svpt - opp_first_won - opp_second_won,
  bp_converted = opp_bp_faced - opp_bp_saved
)]

# Count recent workload over fixed day windows.
# These features act as fatigue or scheduling-pressure proxies and use only prior matches for each player.
player_history[, matches_played_last_7d := count_prior_matches_in_days(match_date, 7L), by = player_id]
player_history[, matches_played_last_14d := count_prior_matches_in_days(match_date, 14L), by = player_id]

# Add recent-form and recent serve/return rolling features over the last 5 and last 10 prior matches.
# No expanding career averages are used here; only fixed backward-looking windows are created.
player_history <- add_recent_rate_features(player_history, windows = c(5L, 10L))

# Re-sort within player and surface so the next step can compute prior same-surface histories only.
setorder(player_history, player_id, surface, match_date, match_order)
player_history[, prior_surface_matches := seq_len(.N) - 1L, by = .(player_id, surface)]

# Build recent surface-specific win rates.
# These features answer whether a player has been performing well recently on the same surface as the current match.
for (window_size in c(5L, 10L)) {
  player_history[, (paste0('surface_win_rate_last_', window_size)) := fifelse(
    prior_surface_matches >= window_size,
    frollmean(shift(won, 1L), n = window_size, align = 'right', na.rm = TRUE),
    NA_real_
  ), by = .(player_id, surface)]
}

setorder(player_history, match_id, player_id)

# Extract the minimal match information needed to build Elo ratings in chronological order.
# Elo is constructed separately from the rolling feature table to keep the update logic transparent.
elo_source <- history_source[, .(match_id, match_order, match_date, surface, winner_id, loser_id)]
setorder(elo_source, match_order)

# Use environments as fast key-value stores for current Elo ratings.
# One environment tracks general Elo and one tracks surface-specific Elo.
general_elo <- new.env(hash = TRUE, parent = emptyenv())
surface_elo <- new.env(hash = TRUE, parent = emptyenv())
k_factor <- 32
initial_elo <- 1500

elo_source[, `:=`(
  elo_winner_pre = NA_real_,
  elo_loser_pre = NA_real_,
  surface_elo_winner_pre = NA_real_,
  surface_elo_loser_pre = NA_real_
)]

# Helper for Elo construction: return an existing rating if it has already been established,
# otherwise use the starting rating for players or player-surface combinations not yet seen.
get_rating <- function(env_object, key, default_value) {
  if (exists(key, envir = env_object, inherits = FALSE)) {
    get(key, envir = env_object, inherits = FALSE)
  } else {
    default_value
  }
}

# Walk through matches in chronological order to compute leakage-safe pre-match Elo values.
# Ratings are recorded first, then updated after the observed result, so the current match never feeds into its own Elo predictor.
for (i in seq_len(nrow(elo_source))) {
  winner_key <- as.character(elo_source$winner_id[i])
  loser_key <- as.character(elo_source$loser_id[i])
  surface_key_winner <- paste(elo_source$winner_id[i], elo_source$surface[i], sep = '||')
  surface_key_loser <- paste(elo_source$loser_id[i], elo_source$surface[i], sep = '||')

  winner_pre <- get_rating(general_elo, winner_key, initial_elo)
  loser_pre <- get_rating(general_elo, loser_key, initial_elo)
  winner_surface_pre <- get_rating(surface_elo, surface_key_winner, initial_elo)
  loser_surface_pre <- get_rating(surface_elo, surface_key_loser, initial_elo)

  elo_source$elo_winner_pre[i] <- winner_pre
  elo_source$elo_loser_pre[i] <- loser_pre
  elo_source$surface_elo_winner_pre[i] <- winner_surface_pre
  elo_source$surface_elo_loser_pre[i] <- loser_surface_pre

  winner_expected <- 1 / (1 + 10 ^ ((loser_pre - winner_pre) / 400))
  loser_expected <- 1 - winner_expected
  winner_surface_expected <- 1 / (1 + 10 ^ ((loser_surface_pre - winner_surface_pre) / 400))
  loser_surface_expected <- 1 - winner_surface_expected

  assign(winner_key, winner_pre + k_factor * (1 - winner_expected), envir = general_elo)
  assign(loser_key, loser_pre + k_factor * (0 - loser_expected), envir = general_elo)
  assign(surface_key_winner, winner_surface_pre + k_factor * (1 - winner_surface_expected), envir = surface_elo)
  assign(surface_key_loser, loser_surface_pre + k_factor * (0 - loser_surface_expected), envir = surface_elo)
}

# Keep only the pre-match Elo columns that need to be merged back into the final match-level dataset.
elo_features <- elo_source[, .(
  match_id,
  elo_winner_pre,
  elo_loser_pre,
  surface_elo_winner_pre,
  surface_elo_loser_pre
)]

# Build a head-to-head history table keyed by unordered player pairs.
# This lets the script count prior meetings regardless of which player won or how the pair was oriented in the raw data.
pair_history <- history_source[, .(match_id, match_order, winner_id, loser_id)]
pair_history[, pair_player_1 := pmin(winner_id, loser_id)]
pair_history[, pair_player_2 := pmax(winner_id, loser_id)]
setorder(pair_history, pair_player_1, pair_player_2, match_order)
pair_history[, prior_h2h_matches := seq_len(.N) - 1L, by = .(pair_player_1, pair_player_2)]
pair_history[, prior_wins_player_1 := shift(cumsum(winner_id == pair_player_1), 1L, fill = 0), by = .(pair_player_1, pair_player_2)]
pair_history[, prior_wins_player_2 := shift(cumsum(winner_id == pair_player_2), 1L, fill = 0), by = .(pair_player_1, pair_player_2)]

# Enumerate the player-level engineered features that will be pulled from player_history and merged onto the final match table.
# Listing them explicitly keeps the final dataset construction transparent and reproducible.
player_feature_columns <- c(
  'prior_match_count',
  'matches_played_last_7d',
  'matches_played_last_14d',
  'wins_last_5',
  'wins_last_10',
  'win_rate_last_5',
  'win_rate_last_10',
  'prior_surface_matches',
  'surface_win_rate_last_5',
  'surface_win_rate_last_10',
  'ace_rate_last_5',
  'ace_rate_last_10',
  'double_fault_rate_last_5',
  'double_fault_rate_last_10',
  'first_serve_in_rate_last_5',
  'first_serve_in_rate_last_10',
  'first_serve_points_won_rate_last_5',
  'first_serve_points_won_rate_last_10',
  'second_serve_points_won_rate_last_5',
  'second_serve_points_won_rate_last_10',
  'service_points_won_rate_last_5',
  'service_points_won_rate_last_10',
  'break_points_saved_rate_last_5',
  'break_points_saved_rate_last_10',
  'ace_rate_allowed_last_5',
  'ace_rate_allowed_last_10',
  'opp_first_serve_in_rate_allowed_last_5',
  'opp_first_serve_in_rate_allowed_last_10',
  'return_points_won_rate_last_5',
  'return_points_won_rate_last_10',
  'break_points_converted_rate_last_5',
  'break_points_converted_rate_last_10'
)

# Keep just the merge keys plus the engineered player features.
# The same table will be merged once for Player A and once for Player B.
player_features <- player_history[, c('match_id', 'player_id', 'surface', player_feature_columns), with = FALSE]
setnames(player_features, old = player_feature_columns, new = paste0(player_feature_columns, '_player'))

# Start the final modeling sample from matches where a unique higher-ranked player exists.
final_matches <- matches[usable_for_model == TRUE]

# Create a convenience flag indicating whether Player A is the raw-data winner.
# This allows all later A/B assignments to be written once and then reused consistently.
final_matches[, A_is_winner := higher_rank_side == 'winner']
# Re-orient each retained match into the modeling frame.
# Player A is always the higher-ranked player and Player B is always the lower-ranked player.
# The target is then whether Player A won.
final_matches[, `:=`(
  player_A_id = fifelse(A_is_winner, winner_id, loser_id),
  player_A_name = fifelse(A_is_winner, winner_name, loser_name),
  player_B_id = fifelse(A_is_winner, loser_id, winner_id),
  player_B_name = fifelse(A_is_winner, loser_name, winner_name),
  rank_A = fifelse(A_is_winner, winner_rank, loser_rank),
  rank_B = fifelse(A_is_winner, loser_rank, winner_rank),
  rank_points_A = fifelse(A_is_winner, winner_rank_points, loser_rank_points),
  rank_points_B = fifelse(A_is_winner, loser_rank_points, winner_rank_points),
  outcome_A_win = fifelse(A_is_winner, 1L, 0L)
)]

# Create simple matchup-level ranking comparisons.
# rank_diff is positive by construction because Player B is lower ranked in quality and higher in numeric rank value.
final_matches[, `:=`(
  rank_diff = rank_B - rank_A,
  rank_ratio = safe_divide(rank_B, rank_A)
)]

# Map pre-existing ranking features from the cleaned input into A-side and B-side versions.
# These columns are inherited rather than rebuilt in this script, then re-expressed consistently under the A/B orientation.
ranking_map <- list(
  lag_rank_1wk = c('w_lag_rank_1wk', 'l_lag_rank_1wk'),
  lag_rank_4wk = c('w_lag_rank_4wk', 'l_lag_rank_4wk'),
  lag_points_1wk = c('w_lag_points_1wk', 'l_lag_points_1wk'),
  lag_points_4wk = c('w_lag_points_4wk', 'l_lag_points_4wk'),
  rank_change_1wk = c('w_rank_change_1wk', 'l_rank_change_1wk'),
  rank_change_4wk = c('w_rank_change_4wk', 'l_rank_change_4wk'),
  points_change_1wk = c('w_points_change_1wk', 'l_points_change_1wk'),
  points_change_4wk = c('w_points_change_4wk', 'l_points_change_4wk'),
  avg_rank_4wk = c('w_avg_rank_4wk', 'l_avg_rank_4wk'),
  avg_rank_8wk = c('w_avg_rank_8wk', 'l_avg_rank_8wk'),
  sd_rank_8wk = c('w_sd_rank_8wk', 'l_sd_rank_8wk'),
  avg_points_4wk = c('w_avg_points_4wk', 'l_avg_points_4wk'),
  avg_points_8wk = c('w_avg_points_8wk', 'l_avg_points_8wk'),
  sd_points_8wk = c('w_sd_points_8wk', 'l_sd_points_8wk')
)

# For each inherited ranking feature, create Player A, Player B, and A minus B versions.
for (feature_name in names(ranking_map)) {
  winner_column <- ranking_map[[feature_name]][1]
  loser_column <- ranking_map[[feature_name]][2]
  final_matches[, (paste0(feature_name, '_A')) := fifelse(A_is_winner, get(winner_column), get(loser_column))]
  final_matches[, (paste0(feature_name, '_B')) := fifelse(A_is_winner, get(loser_column), get(winner_column))]
  final_matches[, (paste0(feature_name, '_diff')) := get(paste0(feature_name, '_A')) - get(paste0(feature_name, '_B'))]
}

# Merge the engineered player-level histories onto the match table for Player A.
# The keys are match_id, player_id, and surface so the correct pre-match record is attached to each side.
final_matches <- merge(
  final_matches,
  player_features,
  by.x = c('match_id', 'player_A_id', 'surface'),
  by.y = c('match_id', 'player_id', 'surface'),
  all.x = TRUE,
  sort = FALSE
)
# Rename the merged player-history columns to indicate they belong to Player A.
setnames(final_matches, old = paste0(player_feature_columns, '_player'), new = paste0(player_feature_columns, '_A'))

# Repeat the same merge for Player B so both competitors' pre-match histories are available on the same row.
final_matches <- merge(
  final_matches,
  player_features,
  by.x = c('match_id', 'player_B_id', 'surface'),
  by.y = c('match_id', 'player_id', 'surface'),
  all.x = TRUE,
  sort = FALSE
)
setnames(final_matches, old = paste0(player_feature_columns, '_player'), new = paste0(player_feature_columns, '_B'))

# Convert the raw Player A and Player B history features into explicit matchup differences.
# These difference variables are often the most natural inputs for a pairwise classification model.
for (feature_name in player_feature_columns) {
  final_matches[, (paste0(feature_name, '_diff')) := get(paste0(feature_name, '_A')) - get(paste0(feature_name, '_B'))]
}

# Merge the pre-match Elo values back to the final match-level table.
final_matches <- merge(final_matches, elo_features, by = 'match_id', all.x = TRUE, sort = FALSE)

# Re-orient general and surface Elo so they align with the Player A / Player B convention.
final_matches[, `:=`(
  elo_A = fifelse(A_is_winner, elo_winner_pre, elo_loser_pre),
  elo_B = fifelse(A_is_winner, elo_loser_pre, elo_winner_pre),
  surface_elo_A = fifelse(A_is_winner, surface_elo_winner_pre, surface_elo_loser_pre),
  surface_elo_B = fifelse(A_is_winner, surface_elo_loser_pre, surface_elo_winner_pre)
)]

# Create Elo matchup comparisons that summarize overall and surface-specific strength gaps.
final_matches[, `:=`(
  elo_diff = elo_A - elo_B,
  surface_elo_diff = surface_elo_A - surface_elo_B
)]

# Merge prior head-to-head counts and prior wins by pair back to the match table.
final_matches <- merge(
  final_matches,
  pair_history[, .(match_id, pair_player_1, pair_player_2, prior_h2h_matches, prior_wins_player_1, prior_wins_player_2)],
  by = 'match_id',
  all.x = TRUE,
  sort = FALSE
)

# Translate unordered pair-level head-to-head counts into the Player A perspective.
final_matches[, prior_h2h_wins_A := fifelse(player_A_id == pair_player_1, prior_wins_player_1, prior_wins_player_2)]
final_matches[, prior_h2h_win_rate_A := safe_divide(prior_h2h_wins_A, prior_h2h_matches)]

# Create explicit style-matchup interaction features.
# These compare one player's strengths to the opponent's weaknesses rather than treating both players independently.
final_matches[, `:=`(
  ace_matchup_A = ace_rate_last_10_A - ace_rate_allowed_last_10_B,
  ace_matchup_B = ace_rate_last_10_B - ace_rate_allowed_last_10_A,
  service_vs_return_matchup_A = service_points_won_rate_last_10_A - return_points_won_rate_last_10_B,
  service_vs_return_matchup_B = service_points_won_rate_last_10_B - return_points_won_rate_last_10_A,
  return_vs_service_matchup_A = return_points_won_rate_last_10_A - service_points_won_rate_last_10_B,
  return_vs_service_matchup_B = return_points_won_rate_last_10_B - service_points_won_rate_last_10_A
)]

# Collapse the side-specific matchup measures into A minus B differences for direct model use.
final_matches[, `:=`(
  ace_matchup_diff = ace_matchup_A - ace_matchup_B,
  service_vs_return_matchup_diff = service_vs_return_matchup_A - service_vs_return_matchup_B,
  return_vs_service_matchup_diff = return_vs_service_matchup_A - return_vs_service_matchup_B
)]

# Preserve the already merged weather information under explicit, readable final column names.
final_matches[, `:=`(
  weather_temp_max_c = T2M_MAX,
  weather_temp_min_c = T2M_MIN,
  weather_temp_avg_c = (T2M_MAX + T2M_MIN) / 2,
  weather_wind_speed_mps = WS2M,
  weather_humidity_pct = RH2M,
  weather_precipitation_mm = PRECTOTCORR
)]

# Define the exact order of columns in the final dataset.
final_dataset_columns <- c(
  'match_id', 'match_date', 'tourney_date', 'tourney_id', 'tourney_name', 'tourney_level', 'round', 'surface', 'best_of', 'draw_size', 'match_num',
  'player_A_id', 'player_A_name', 'player_B_id', 'player_B_name',
  'winner_id', 'winner_name', 'loser_id', 'loser_name',
  'rank_A', 'rank_B', 'rank_diff', 'rank_ratio', 'rank_points_A', 'rank_points_B',
  'outcome_A_win',
  paste0(names(ranking_map), '_A'),
  paste0(names(ranking_map), '_B'),
  paste0(names(ranking_map), '_diff'),
  'elo_A', 'elo_B', 'elo_diff', 'surface_elo_A', 'surface_elo_B', 'surface_elo_diff',
  paste0(player_feature_columns, '_A'),
  paste0(player_feature_columns, '_B'),
  paste0(player_feature_columns, '_diff'),
  'ace_matchup_A', 'ace_matchup_B', 'ace_matchup_diff',
  'service_vs_return_matchup_A', 'service_vs_return_matchup_B', 'service_vs_return_matchup_diff',
  'return_vs_service_matchup_A', 'return_vs_service_matchup_B', 'return_vs_service_matchup_diff',
  'prior_h2h_matches', 'prior_h2h_wins_A', 'prior_h2h_win_rate_A',
  'weather_temp_max_c', 'weather_temp_min_c', 'weather_temp_avg_c', 'weather_wind_speed_mps', 'weather_humidity_pct', 'weather_precipitation_mm'
)

# Materialize the final dataset using only the selected columns and then sort it for a clean final export.
final_dataset <- final_matches[, ..final_dataset_columns]
setorder(final_dataset, match_date, tourney_date, match_id)

# Quality-control checks begin here.
# These checks fail if the final table violates the intended one-row-per-match design or other core assumptions.
if (nrow(final_dataset) != uniqueN(final_dataset$match_id)) {
  stop('Final dataset does not contain exactly one row per match_id.')
}

if (any(final_dataset$rank_A >= final_dataset$rank_B, na.rm = TRUE)) {
  stop('Player A assignment failed: rank_A must be strictly smaller than rank_B.')
}

# Independently recompute the target to verify that outcome_A_win was coded correctly.
target_check <- fifelse(final_dataset$player_A_id == final_dataset$winner_id, 1L, 0L)
if (!identical(final_dataset$outcome_A_win, target_check)) {
  stop('Target coding check failed for outcome_A_win.')
}

# Check that all retained rate variables stay within logical probability bounds.
rate_columns <- grep('rate|win_rate', names(final_dataset), value = TRUE)
rate_columns <- rate_columns[!grepl('_diff$', rate_columns)]
rate_columns <- rate_columns[!grepl('matchup', rate_columns)]
for (column_name in rate_columns) {
  values <- final_dataset[[column_name]]
  if (any(values < 0 | values > 1, na.rm = TRUE)) {
    stop(paste('Rate column outside [0, 1]:', column_name))
  }
}

if (any(final_dataset$rank_diff <= 0, na.rm = TRUE)) {
  stop('rank_diff must be strictly positive for all retained matches.')
}

if (any(is.na(final_dataset$elo_A) | is.na(final_dataset$elo_B))) {
  stop('Pre-match Elo features contain unexpected missing values.')
}

# Build a feature-group lookup so the script can summarize missingness and document the final dataset more clearly.
feature_groups <- data.table(column = names(final_dataset))
feature_groups[, feature_group := fifelse(
  column %in% c('match_id', 'match_date', 'tourney_date', 'tourney_id', 'tourney_name', 'tourney_level', 'round', 'surface', 'best_of', 'draw_size', 'match_num'),
  'match_context',
  fifelse(
    column %in% c('player_A_id', 'player_A_name', 'player_B_id', 'player_B_name', 'winner_id', 'winner_name', 'loser_id', 'loser_name'),
    'player_identity',
    fifelse(
      grepl('^rank|^lag_rank|^avg_rank|^sd_rank|^lag_points|^avg_points|^sd_points|^points_change|^rank_change', column),
      'ranking',
      fifelse(
        grepl('^elo|^surface_elo', column),
        'elo',
        fifelse(
          grepl('prior_surface_matches|surface_win_rate', column),
          'surface_form',
          fifelse(
            grepl('matches_played_last|wins_last|win_rate_last|prior_match_count', column),
            'recent_form',
            fifelse(
              grepl('ace_rate|double_fault_rate|first_serve|second_serve|service_points_won_rate|break_points_saved_rate|opp_first_serve_in_rate_allowed|return_points_won_rate|break_points_converted_rate', column),
              'serve_return',
              fifelse(
                grepl('matchup', column),
                'matchup',
                fifelse(
                  grepl('^prior_h2h', column),
                  'head_to_head',
                  fifelse(grepl('^weather_', column), 'weather', 'target')
                )
              )
            )
          )
        )
      )
    )
  )
)]

# Provide human-readable descriptions for the most important fields in the final data dictionary.
description_lookup <- list(
  match_id = 'Unique match identifier inherited from the cleaned input dataset.',
  match_date = 'Standardized match date used for chronological feature construction.',
  tourney_date = 'Tournament start date from the cleaned input file.',
  tourney_id = 'Tournament identifier from the cleaned input file.',
  tourney_name = 'Tournament name from the cleaned input file.',
  tourney_level = 'ATP tournament level code.',
  round = 'Round code from the cleaned input file.',
  surface = 'Match surface category.',
  best_of = 'Scheduled best-of format when available.',
  draw_size = 'Tournament draw size when available.',
  match_num = 'Within-tournament match number used in deterministic tie-breaking.',
  player_A_id = 'Player A identifier. Player A is the higher-ranked player before the match.',
  player_A_name = 'Player A name from the cleaned input file.',
  player_B_id = 'Player B identifier. Player B is the lower-ranked player before the match.',
  player_B_name = 'Player B name from the cleaned input file.',
  winner_id = 'Original winner identifier from the cleaned input file.',
  winner_name = 'Original winner name from the cleaned input file.',
  loser_id = 'Original loser identifier from the cleaned input file.',
  loser_name = 'Original loser name from the cleaned input file.',
  rank_A = 'Pre-match ATP rank for Player A; lower numeric value means stronger rank.',
  rank_B = 'Pre-match ATP rank for Player B.',
  rank_diff = 'Ranking gap defined as rank_B minus rank_A.',
  rank_ratio = 'Ranking ratio defined as rank_B divided by rank_A when both ranks are positive.',
  rank_points_A = 'Pre-match ATP ranking points for Player A.',
  rank_points_B = 'Pre-match ATP ranking points for Player B.',
  outcome_A_win = 'Binary target equal to 1 when Player A won and 0 when Player A lost.'
)

# For columns not listed explicitly above, infer a generic description from the naming convention.
# This is why the A, B, diff, weather, and head-to-head columns are named systematically.
infer_description <- function(column_name) {
  if (!is.null(description_lookup[[column_name]])) {
    return(description_lookup[[column_name]])
  }

  if (grepl('_A$', column_name)) {
    return(paste('Pre-match value for Player A of', sub('_A$', '', column_name), 'computed from prior data only.'))
  }

  if (grepl('_B$', column_name)) {
    return(paste('Pre-match value for Player B of', sub('_B$', '', column_name), 'computed from prior data only.'))
  }

  if (grepl('_diff$', column_name)) {
    return(paste('Difference feature defined as Player A minus Player B for', sub('_diff$', '', column_name), '.'))
  }

  if (grepl('^weather_', column_name)) {
    return('Weather variable preserved from the cleaned match-weather input file.')
  }

  if (grepl('^prior_h2h', column_name)) {
    return('Head-to-head feature computed only from meetings before the current match.')
  }

  'Engineered pre-match feature.'
}

# Create the variable-level data dictionary with feature group, data type, missingness, and description.
feature_dictionary <- data.table(
  column = names(final_dataset),
  feature_group = feature_groups$feature_group,
  data_type = sapply(final_dataset, function(x) class(x)[1]),
  pct_missing = round(sapply(final_dataset, function(x) mean(is.na(x)) * 100), 3),
  description = vapply(names(final_dataset), infer_description, character(1))
)

# Export the final modeling dataset and the companion variable dictionary.
write_csv(as.data.frame(final_dataset), output_dataset_path)
write_csv(as.data.frame(feature_dictionary), output_dictionary_path)
