# This script compares tournament samples (2000+ vs 2010+)
# and selects the 2010+ sample for further analysis due to
# improved consistency in tournament structure and surface types.

library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(here)

# list ATP singles match files
match_files <- list.files(
  path = here("data", "raw", "tennis_ATP"),
  pattern = "^atp_matches_[0-9]{4}\\.csv$",
  full.names = TRUE
)

# read match-level tournament information from all years
all_tournaments <- match_files |>
  map_dfr(\(file) {
    read_csv(file, show_col_types = FALSE) |>
      transmute(
        year = as.integer(str_extract(basename(file), "[0-9]{4}")),
        tourney_name = tourney_name,
        tourney_level = tourney_level,
        surface = surface
      )
  })

# keep main-tour events for comparing sample restrictions
main_tournaments <- all_tournaments |>
  filter(tourney_level %in% c("A", "M", "G", "F", "O"))

# create restricted samples
data_2000 <- main_tournaments |>
  filter(year >= 2000)

data_2010 <- main_tournaments |>
  filter(year >= 2010)

# compare total observations and unique tournament names
cat("Total matches, 2000+: ", nrow(data_2000), "\n", sep = "")
cat("Total matches, 2010+: ", nrow(data_2010), "\n", sep = "")
cat("Unique tournament names, 2000+: ", n_distinct(data_2000$tourney_name), "\n", sep = "")
cat("Unique tournament names, 2010+: ", n_distinct(data_2010$tourney_name), "\n", sep = "")

# summarize tournament structure in each restricted sample
summary_2000 <- data_2000 |>
  group_by(tourney_name) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    n_years = n_distinct(year),
    surfaces = paste(sort(unique(surface[!is.na(surface)])), collapse = ", "),
    n_surfaces = n_distinct(surface, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(tourney_name)

summary_2010 <- data_2010 |>
  group_by(tourney_name) |>
  summarise(
    first_year = min(year),
    last_year = max(year),
    n_years = n_distinct(year),
    surfaces = paste(sort(unique(surface[!is.na(surface)])), collapse = ", "),
    n_surfaces = n_distinct(surface, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(tourney_name)

# save comparison outputs for inspection
write_csv(summary_2000, here("data", "raw", "tennis_ATP", "tournament_summary_2000plus.csv"))
write_csv(summary_2010, here("data", "raw", "tennis_ATP", "tournament_summary_2010plus.csv"))

# decision:
# the project will use the 2010+ sample because it retains a large number of 
# matches while removing older tournament structures and obsolete surfaces
# such as carpet, making tournament-location matching and weather merging cleaner
# by restricting to a even more modern era we drop significant noise in hopes for
# a cleaner signal at the expense of a considerably smaller sample size. (75k obs. to 43k obs.)
