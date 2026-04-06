# 04_visualization_stage.R
library(tidyverse)
library(here)
library(ggplot2)
library(maps)


atp_data <- read_csv(here("data", "cleaned", "atp_matches_weather_ranking_data.csv"))

# map of tournement locations

tourney_locations <- atp_data |>
  distinct(tourney_name, latitude, longitude)

# World map background
world <- map_data("world")

Global_distribution_of_ATP_tournament_locations <- ggplot() +
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

Global_distribution_of_ATP_tournament_locations

# Figure showing that ranking differences can help predict winners and loosers

#To analyze how player rankings relate to match outcomes, the data were transformed
#from match-level format (one row per match) into player-level format (two rows per match). 
#This allows each player to be treated as an observation with a binary outcome variable indicating whether they won or lost.

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
  )

ranking_plot_data <- ranking_plot_data |>
  mutate(rank_bin = cut(rank_diff, breaks = seq(-200, 200, by = 10)))

ranking_summary <- ranking_plot_data |>
  group_by(rank_bin) |>
  summarise(
    win_rate = mean(outcome),
    mid_point = mean(rank_diff)
  )

Win_propability_by_ranking_difference <- ggplot(ranking_summary, aes(x = mid_point, y = win_rate)) +
  geom_line(color = "darkblue") +
  geom_point(size = 1.5) +
  labs(
    title = "Win Probability by Ranking Difference",
    subtitle = "Players with better rankings are significantly more likely to win",
    x = "Ranking Difference (player rank - opponent rank)",
    y = "Win Probability"
  ) +
  theme_minimal()

Win_propability_by_ranking_difference

# Weather visualization 


# Create upset variable
# Upset = worse-ranked player defeats better-ranked player
# Higher rank number = worse ranking


atp_data <- atp_data |>
  mutate(
    upset = winner_rank > loser_rank
  )

# Create extreme weather indicators
# Using 90th percentile thresholds for heat and wind
# Rain is defined as any positive precipitation

heat_cutoff <- quantile(atp_data$T2M_MAX, 0.90, na.rm = TRUE)
wind_cutoff <- quantile(atp_data$WS2M, 0.90, na.rm = TRUE)

atp_data <- atp_data |>
  mutate(
    extreme_heat = T2M_MAX >= heat_cutoff,
    extreme_wind = WS2M >= wind_cutoff,
    precipitation_event = PRECTOTCORR > 0
  )


# Summaries for plotting

heat_summary <- atp_data |>
  group_by(extreme_heat) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(
    condition = if_else(extreme_heat, "Extreme heat", "Non-extreme heat")
  )

wind_summary <- atp_data |>
  group_by(extreme_wind) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(
    condition = if_else(extreme_wind, "Extreme wind", "Non-extreme wind")
  )

precip_summary <- atp_data |>
  group_by(precipitation_event) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n_matches = n(),
    .groups = "drop"
  ) |>
  mutate(
    condition = if_else(precipitation_event, "Precipitation", "No precipitation")
  )


# Figure 1: Extreme heat and upset probability

Extreme_heat_and_upset_probability <- ggplot(heat_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)),
            vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability Under Extreme Heat",
    subtitle = "Extreme heat defined as matches in the top 10% of maximum temperature",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


# Figure 2: Extreme wind and upset probability

Extreme_wind_and_upset_probability <- ggplot(wind_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)),
            vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability Under Extreme Wind",
    subtitle = "Extreme wind defined as matches in the top 10% of wind speed",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


# Figure 3: Precipitation and upset probability


Precipitation_and_upset_probability <- ggplot(precip_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::percent(upset_rate, accuracy = 0.1)),
            vjust = -0.4, size = 4) +
  labs(
    title = "Upset Probability and Precipitation",
    subtitle = "Precipitation event defined as any positive daily precipitation",
    x = NULL,
    y = "Upset probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

# show figures 
Extreme_heat_and_upset_probability
Extreme_wind_and_upset_probability
Precipitation_and_upset_probability

# To examine whether environmental conditions affect competitive balance,
# I analyzed the probability of upsets under extreme temperature, wind, and precipitation conditions. 
# Across all specifications, the results show little evidence that extreme weather meaningfully increases upset probability.
# This suggests that while weather may influence match conditions, it does not substantially alter
# the relative advantage implied by player rankings.

# Weather effects in close matches

atp_data <- atp_data |>
  mutate(
    rank_diff = abs(winner_rank - loser_rank),
    close_match = rank_diff <= 1   # you can adjust this later
  )
close_data <- atp_data |>
  filter(close_match)
close_data <- close_data |>
  mutate(
    upset = winner_rank > loser_rank
  )
heat_cutoff <- quantile(atp_data$T2M_MAX, 0.9, na.rm = TRUE)

close_data <- close_data |>
  mutate(
    extreme_heat = T2M_MAX >= heat_cutoff
  )

heat_close_summary <- close_data |>
  group_by(extreme_heat) |>
  summarise(
    upset_rate = mean(upset, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(
    condition = if_else(extreme_heat, "Extreme Heat", "Normal Conditions")
  )

Upset_Probability_in_Close_Matches_Under_Extreme_Heat <- ggplot(heat_close_summary, aes(x = condition, y = upset_rate)) +
  geom_col(width = 0.6, fill = "darkred") +
  geom_text(
    aes(label = scales::percent(upset_rate, accuracy = 0.1)),
    vjust = -0.4,
    size = 4
  ) +
  labs(
    title = "Upset Probability in Close Matches Under Extreme Heat",
    subtitle = "Close matches defined as rank difference ≤ 1",
    x = "",
    y = "Upset Probability"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

Upset_Probability_in_Close_Matches_Under_Extreme_Heat

# effect possibly grows when ratings are close however estimates are
# extremely sensitive to changes in close matches qualification i.e <=1, <=5.


