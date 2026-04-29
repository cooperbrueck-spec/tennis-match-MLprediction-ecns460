library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(here)

# identify all ATP singles match files
match_files <- list.files(
  path = here("data", "raw", "tennis_ATP"),
  pattern = "^atp_matches_[0-9]{4}\\.csv$",
  full.names = TRUE
)

# inspect parsing issues across all files to confirm whether they come from older years
parsing_issues <- match_files |>
  map_dfr(\(file) {
    dat <- read_csv(file, show_col_types = FALSE)
    probs <- problems(dat)
    
    if (nrow(probs) == 0) {
      return(tibble())
    }
    
    probs |>
      mutate(
        source_file = basename(file),
        year = as.integer(str_extract(basename(file), "[0-9]{4}")),
        .before = 1
      )
  })

# quick check of parsing issue years
parsing_issues |>
  count(year, source_file, sort = TRUE)

# read and combine all files
atp_matches_all <- match_files |>
  map_dfr(\(file) {
    read_csv(file, show_col_types = FALSE) |>
      mutate(
        year = as.integer(str_extract(basename(file), "[0-9]{4}"))
      )
  })

# inspect duplicate rows in the intended analysis sample before removing anything
duplicates_2010 <- atp_matches_all |>
  filter(
    year >= 2010,
    tourney_level %in% c("A", "M", "G", "F", "O")
  ) |>
  group_by(across(everything())) |>
  filter(n() > 1) |>
  ungroup()

duplicates_2010 |>
  select(year, tourney_name, match_num, winner_name, loser_name) |>
  head(10)

# restrict to the modern era and main tour events, then apply basic cleaning
atp_matches_2010 <- atp_matches_all |>
  filter(
    year >= 2010,
    tourney_level %in% c("A", "M", "G", "F", "O")
  ) |>
  mutate(
    across(where(is.character), str_squish)
  ) |>
  distinct()

# quick checks on the cleaned dataset
cat("Total matches in cleaned 2010+ sample:", nrow(atp_matches_2010), "\n")
cat("Unique tournaments in cleaned 2010+ sample:", n_distinct(atp_matches_2010$tourney_name), "\n")
cat("Duplicate rows removed:", nrow(duplicates_2010), "\n")

# Save first cleaned tennis dataset
write_csv(
  atp_matches_2010,
  here("data", "cleaned", "atp_matches_2010_clean.csv")
)
