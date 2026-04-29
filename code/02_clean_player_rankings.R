library(tidyverse)
library(here)

players <- read_csv(here("data", "raw", "tennis_ATP", "atp_players.csv"))

rankings_00s <- read_csv(here("data", "raw", "tennis_ATP", "atp_rankings_00s.csv"))
rankings_10s <- read_csv(here("data", "raw", "tennis_ATP", "atp_rankings_10s.csv"))
rankings_20s <- read_csv(here("data", "raw", "tennis_ATP", "atp_rankings_20s.csv"))
rankings_current <- read_csv(here("data", "raw", "tennis_ATP", "atp_rankings_current.csv"))

# Combine data 

rankings <- bind_rows(
  rankings_00s,
  rankings_10s,
  rankings_20s,
  rankings_current
)

# Prepare for merge 

rankings <- rankings |>
  rename(player_id = player)

rankings <- rankings |>
  mutate(ranking_date = as.Date(as.character(ranking_date), format = "%Y%m%d"))

players <- players |>
  mutate(
    dob = as.Date(as.character(dob), format = "%Y%m%d"),
    height = as.numeric(height)
  )

# Check missing data
sum(is.na(rankings$player_id))
sum(is.na(rankings$ranking_date))
sum(is.na(rankings$rank))
sum(is.na(rankings$points))


sum(is.na(players$player_id))
sum(is.na(players$dob))
sum(is.na(players$height))

# Save cleaned data pre ,merge
write_csv(rankings, here("data", "cleaned", "atp_rankings_all.csv"))
write_csv(players, here("data", "cleaned", "atp_players_clean.csv"))

rankings <- rankings |>
  arrange(player_id, ranking_date)
rankings |>
  count(player_id, ranking_date) |>
  filter(n > 1)

rankings |>
  group_by(player_id) |>
  summarise(n_obs = n()) |>
  arrange(desc(n_obs))
