library(tidyverse)
library(readr)
library(here)

atp_matches_with_nasa_weather <- read_csv(
  here("data", "cleaned", "atp_matches_with_nasa_weather.csv"),
  show_col_types = FALSE
)

# Inspect classes and correct classes

sapply(atp_matches_with_nasa_weather, class)

atp_matches_with_nasa_weather <- atp_matches_with_nasa_weather |>
  mutate(
    match_date = as.Date(match_date),
    tourney_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  )

#Check impossible weather values

weather_checks <- atp_matches_with_nasa_weather |>
  summarise(
    bad_tmax_tmin = sum(T2M_MAX < T2M_MIN, na.rm = TRUE),
    bad_precip = sum(PRECTOTCORR < 0, na.rm = TRUE),
    bad_wind = sum(WS2M < 0, na.rm = TRUE),
    bad_humidity = sum(RH2M < 0 | RH2M > 100, na.rm = TRUE),
    missing_tmax = sum(is.na(T2M_MAX)),
    missing_tmin = sum(is.na(T2M_MIN)),
    missing_precip = sum(is.na(PRECTOTCORR)),
    missing_wind = sum(is.na(WS2M)),
    missing_humidity = sum(is.na(RH2M))
  )

print(weather_checks)


#Check latitude/longitude too

location_checks <- atp_matches_with_nasa_weather |>
  summarise(
    bad_latitude = sum(latitude < -90 | latitude > 90, na.rm = TRUE),
    bad_longitude = sum(longitude < -180 | longitude > 180, na.rm = TRUE),
    missing_latitude = sum(is.na(latitude)),
    missing_longitude = sum(is.na(longitude))
  )

print(location_checks)

# Check for duplicates

duplicate_matches <- atp_matches_with_nasa_weather |>
  group_by(
    tourney_name,
    tourney_date,
    match_num,
    winner_name,
    loser_name,
    round
  ) |>
  filter(n() > 1) |>
  ungroup()

nrow(duplicate_matches)


surface_check <- atp_matches_with_nasa_weather |>
  count(surface, sort = TRUE)

print(surface_check)

# Save prefeature dataset

write_csv(
  atp_matches_with_nasa_weather,
  here("data", "cleaned", "atp_matches_prefeature.csv")
)
