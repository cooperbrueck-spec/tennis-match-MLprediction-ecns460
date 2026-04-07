# 04_visualization_stage.R
library(tidyverse)
library(here)
library(ggplot2)
library(maps)

# -------------------------------------------------------------------------
# Paths and setup
# -------------------------------------------------------------------------

figures_dir <- here("results", "figures")
tables_dir <- here("results", "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

atp_data <- read_csv(here("data", "cleaned", "atp_matches_weather_ranking_data.csv"))

save_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 300) {
  ggsave(
    filename = file.path(figures_dir, filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi
  )
}

save_table <- function(table_obj, filename) {
  write_csv(table_obj, file.path(tables_dir, filename))
}

# -------------------------------------------------------------------------
# Figure: Global tournament map
# -------------------------------------------------------------------------

tourney_locations <- atp_data |>
  distinct(tourney_name, latitude, longitude)

world <- map_data("world")

fig_global_tournament_locations <- ggplot() +
  geom_polygon(
    data = world,
    aes(x = long, y = lat, group = group),
    fill = "gray90",
    color = "white"
  ) +
  geom_point(
    data = tourney_locations,
    aes(x = longitude, y = latitude),
    color = "darkred",
    size = 2,
    alpha = 0.7
  ) +
  coord_fixed(1.3) +
  labs(
    title = "Global Distribution of ATP Tournament Locations",
    subtitle = "Locations of professional men's tennis tournaments in the dataset",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

save_plot(fig_global_tournament_locations, "global_atp_tournament_locations.png")

# -------------------------------------------------------------------------
# Figure: Win probability by ranking difference
# -------------------------------------------------------------------------

ranking_plot_data <- atp_data |>
  transmute(
    match_id,
    rank_diff = winner_rank - loser_rank,
    outcome = 1
  ) |>
  bind_rows(
    atp_data |>
      transmute(
        match_id,
        rank_diff = loser_rank - winner_rank,
        outcome = 0
      )
  ) |>
  mutate(rank_bin = cut(rank_diff, breaks = seq(-200, 200, by = 10)))

ranking_summary <- ranking_plot_data |>
  group_by(rank_bin) |>
  summarise(
    win_rate = mean(outcome),
    mid_point = mean(rank_diff),
    n_obs = n(),
    .groups = "drop"
  )

fig_win_probability_by_ranking_difference <- ggplot(
  ranking_summary,
  aes(x = mid_point, y = win_rate)
) +
  geom_line(color = "darkblue") +
  geom_point(size = 1.5) +
  labs(
    title = "Win Probability by Ranking Difference",
    subtitle = "Players with better rankings are significantly more likely to win",
    x = "Ranking Difference (player rank - opponent rank)",
    y = "Win Probability"
  ) +
  theme_minimal()

save_plot(fig_win_probability_by_ranking_difference, "win_probability_by_ranking_difference.png")
save_table(ranking_summary, "win_probability_by_ranking_difference_summary.csv")

# -------------------------------------------------------------------------
# Weather-upset analysis
# -------------------------------------------------------------------------

atp_weather <- atp_data |>
  mutate(upset = winner_rank > loser_rank)

heat_cutoff <- quantile(atp_weather$T2M_MAX, 0.90, na.rm = TRUE)
wind_cutoff <- quantile(atp_weather$WS2M, 0.90, na.rm = TRUE)

atp_weather <- atp_weather |>
  mutate(
    extreme_heat = T2M_MAX >= heat_cutoff,
    extreme_wind = WS2M >= wind_cutoff,
    precipitation_event = PRECTOTCORR > 0
  )

heat_summary <- atp_weather |>
  group_by(extreme_heat) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(condition = if_else(extreme_heat, "Extreme heat", "Non-extreme heat"))

wind_summary <- atp_weather |>
  group_by(extreme_wind) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(condition = if_else(extreme_wind, "Extreme wind", "Non-extreme wind"))

precip_summary <- atp_weather |>
  group_by(precipitation_event) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(condition = if_else(precipitation_event, "Precipitation", "No precipitation"))

fig_upset_extreme_heat <- ggplot(heat_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)), vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability Under Extreme Heat",
    subtitle = "Extreme heat defined as matches in the top 10% of maximum temperature",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

fig_upset_extreme_wind <- ggplot(wind_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)), vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability Under Extreme Wind",
    subtitle = "Extreme wind defined as matches in the top 10% of wind speed",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

fig_upset_precipitation <- ggplot(precip_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)), vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability and Precipitation",
    subtitle = "Precipitation event defined as any positive daily precipitation",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

save_plot(fig_upset_extreme_heat, "upset_probability_extreme_heat.png")
save_plot(fig_upset_extreme_wind, "upset_probability_extreme_wind.png")
save_plot(fig_upset_precipitation, "upset_probability_precipitation.png")

save_table(heat_summary, "upset_probability_extreme_heat_summary.csv")
save_table(wind_summary, "upset_probability_extreme_wind_summary.csv")
save_table(precip_summary, "upset_probability_precipitation_summary.csv")

# -------------------------------------------------------------------------
# Figure: Upsets in close matches under extreme heat
# -------------------------------------------------------------------------

close_data <- atp_weather |>
  mutate(rank_diff = abs(winner_rank - loser_rank), close_match = rank_diff <= 20) |>
  filter(close_match) |>
  mutate(extreme_heat = T2M_MAX >= heat_cutoff)

heat_close_summary <- close_data |>
  group_by(extreme_heat) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(condition = if_else(extreme_heat, "Extreme Heat", "Normal Conditions"))

fig_upset_close_matches_extreme_heat <- ggplot(heat_close_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6, fill = "darkred") +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)), vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability in Close Matches Under Extreme Heat",
    subtitle = "Close matches defined as rank difference ≤ 20",
    x = NULL,
    y = "Upset Probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

save_plot(fig_upset_close_matches_extreme_heat, "upset_probability_close_matches_extreme_heat.png")
save_table(heat_close_summary, "upset_probability_close_matches_extreme_heat_summary.csv")

# -------------------------------------------------------------------------
# NEW FIGURE: Upset rate by rank difference for
#             high-ace underdog vs ace-vulnerable favorite
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Upset rate by rank difference for:
# 1. All matches
# 2. High-ace underdog vs ace-vulnerable favorite
# 3. High-ace favorite vs low-ace underdog
# -------------------------------------------------------------------------

player_serve_return_profile <- bind_rows(
  atp_data |>
    transmute(
      player_id = winner_id,
      aces_hit = w_ace,
      first_serves_in = w_1stIn,
      aces_allowed = l_ace,
      opp_first_serves_in = l_1stIn
    ),
  atp_data |>
    transmute(
      player_id = loser_id,
      aces_hit = l_ace,
      first_serves_in = l_1stIn,
      aces_allowed = w_ace,
      opp_first_serves_in = w_1stIn
    )
) |>
  group_by(player_id) |>
  summarise(
    ace_rate_on_1st_serve = sum(aces_hit, na.rm = TRUE) / sum(first_serves_in, na.rm = TRUE),
    aces_allowed_rate_on_opp_1st_serve = sum(aces_allowed, na.rm = TRUE) / sum(opp_first_serves_in, na.rm = TRUE),
    n_player_matches = n(),
    .groups = "drop"
  )

high_ace_cutoff <- quantile(
  player_serve_return_profile$ace_rate_on_1st_serve,
  0.75,
  na.rm = TRUE
)

low_ace_cutoff <- quantile(
  player_serve_return_profile$ace_rate_on_1st_serve,
  0.25,
  na.rm = TRUE
)

ace_vulnerable_cutoff <- quantile(
  player_serve_return_profile$aces_allowed_rate_on_opp_1st_serve,
  0.75,
  na.rm = TRUE
)

player_serve_return_profile <- player_serve_return_profile |>
  mutate(
    high_ace_server = ace_rate_on_1st_serve >= high_ace_cutoff,
    low_ace_server = ace_rate_on_1st_serve <= low_ace_cutoff,
    ace_vulnerable_returner = aces_allowed_rate_on_opp_1st_serve >= ace_vulnerable_cutoff
  )

favorite_underdog_matchups <- atp_data |>
  transmute(
    match_id,
    winner_id,
    loser_id,
    winner_rank,
    loser_rank,
    upset = winner_rank > loser_rank
  ) |>
  mutate(
    favorite_id = if_else(winner_rank < loser_rank, winner_id, loser_id),
    underdog_id = if_else(winner_rank < loser_rank, loser_id, winner_id),
    rank_diff = abs(winner_rank - loser_rank),
    upset_occurred = upset
  )

upset_ace_matchup_data <- favorite_underdog_matchups |>
  left_join(
    player_serve_return_profile |>
      select(
        player_id,
        underdog_high_ace_server = high_ace_server,
        underdog_low_ace_server = low_ace_server
      ),
    by = c("underdog_id" = "player_id")
  ) |>
  left_join(
    player_serve_return_profile |>
      select(
        player_id,
        favorite_high_ace_server = high_ace_server,
        favorite_ace_vulnerable_returner = ace_vulnerable_returner
      ),
    by = c("favorite_id" = "player_id")
  ) |>
  mutate(
    good_server_vs_bad_returner =
      coalesce(underdog_high_ace_server, FALSE) &
      coalesce(favorite_ace_vulnerable_returner, FALSE),
    
    favorite_high_ace_vs_underdog_low_ace =
      coalesce(favorite_high_ace_server, FALSE) &
      coalesce(underdog_low_ace_server, FALSE)
  )

upset_rate_all_matches <- upset_ace_matchup_data |>
  group_by(rank_diff) |>
  summarise(
    upset_rate = mean(upset_occurred, na.rm = TRUE),
    n_matches = n(),
    group = "All matches",
    .groups = "drop"
  )

upset_rate_good_server_bad_returner <- upset_ace_matchup_data |>
  filter(good_server_vs_bad_returner) |>
  group_by(rank_diff) |>
  summarise(
    upset_rate = mean(upset_occurred, na.rm = TRUE),
    n_matches = n(),
    group = "High-ace underdog vs ace-vulnerable favorite",
    .groups = "drop"
  )

upset_rate_favorite_high_ace <- upset_ace_matchup_data |>
  filter(favorite_high_ace_vs_underdog_low_ace) |>
  group_by(rank_diff) |>
  summarise(
    upset_rate = mean(upset_occurred, na.rm = TRUE),
    n_matches = n(),
    group = "High-ace favorite vs low-ace underdog",
    .groups = "drop"
  )

upset_rate_comparison <- bind_rows(
  upset_rate_all_matches,
  upset_rate_good_server_bad_returner,
  upset_rate_favorite_high_ace
) |>
  filter(rank_diff >= 1, rank_diff <= 25, n_matches >= 10) |>
  arrange(rank_diff, upset_rate)

fig_upset_rate_rankdiff_ace_matchup <- ggplot(
  upset_rate_comparison,
  aes(x = rank_diff, y = upset_rate, color = group)
) +
  geom_line(
    aes(group = rank_diff),
    color = "gray60",
    linewidth = 0.8
  ) +
  geom_point(size = 2.8) +
  labs(
    title = "Upset Probability by Ranking Difference",
    subtitle = "Upset Probability comparison across all matches, underdog ace advantages, and favorite ace advantages",
    x = "Ranking difference",
    y = "Upset probability",
    color = NULL
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

fig_upset_rate_rankdiff_ace_matchup
save_plot(fig_upset_rate_rankdiff_ace_matchup, "upset_probability_rankdiff_high_ace_vs_ace_vulnerable.png")
save_table(player_serve_return_profile, "player_serve_return_profiles.csv")
save_table(upset_rate_comparison, "upset_probability_rankdiff_high_ace_vs_ace_vulnerable_summary.csv")

# Print key figure objects in interactive sessions
fig_global_tournament_locations
fig_win_probability_by_ranking_difference
fig_upset_extreme_heat
fig_upset_extreme_wind
fig_upset_precipitation
fig_upset_close_matches_extreme_heat
fig_upset_rate_rankdiff_ace_matchup
