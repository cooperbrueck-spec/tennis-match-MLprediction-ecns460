# This script pulls daily NOAA GHCND weather data for ATP tournament stations
# and merges the resulting daily weather variables onto the 2010+ ATP match dataset.
# Weather variables successfully pulled include daily maximum temperature (TMAX),
# daily minimum temperature (TMIN), and daily precipitation (PRCP).
# Average daily wind speed (AWND) was requested but was not returned by the selected
# NOAA stations, so it is not included in the final merged dataset.

library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(httr)
library(jsonlite)

# =========================
# PRE-STEP: NOAA API SETUP
# =========================

noaa_token <- "yAWAAQsLJgQtBWlUoTaNJdGYNcfMNoCV"
noaa_base_url <- "https://www.ncei.noaa.gov/cdo-web/api/v2/"

# =========================
# STEP 1: LOAD DATA
# =========================

atp_matches <- read_csv(
  "data/cleaned/atp_matches_2010_clean.csv",
  show_col_types = FALSE
)

tournament_stations <- read_csv(
  "data/reference_tables/tournament_stations.csv",
  show_col_types = FALSE
)

# =========================
# STEP 2: PREPARE MATCH DATA
# =========================

atp_matches <- atp_matches |>
  mutate(
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  )

matches_with_station <- atp_matches |>
  left_join(
    tournament_stations |>
      select(tourney_name, station_id),
    by = "tourney_name"
  )

station_dates <- matches_with_station |>
  select(tourney_name, station_id, match_date) |>
  distinct() |>
  filter(!is.na(station_id), !is.na(match_date))

station_year_requests <- station_dates |>
  mutate(year = format(match_date, "%Y")) |>
  distinct(station_id, year) |>
  arrange(station_id, year)

# =========================
# STEP 3: NOAA HELPER
# =========================

get_noaa_daily_data <- function(station_id, year, datatype_id, token, base_url, max_tries = 5) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
  for (attempt in seq_len(max_tries)) {
    res <- GET(
      paste0(base_url, "data"),
      add_headers(token = token),
      query = list(
        datasetid = "GHCND",
        stationid = station_id,
        startdate = start_date,
        enddate = end_date,
        datatypeid = datatype_id,
        units = "metric",
        limit = 1000
      )
    )
    
    if (status_code(res) == 200) {
      parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
      
      if (is.null(parsed$results)) {
        return(tibble())
      }
      
      return(
        as_tibble(parsed$results) |>
          transmute(
            station_id = station,
            date = as.Date(substr(date, 1, 10)),
            datatype = datatype,
            value = value
          )
      )
    }
    
    if (status_code(res) %in% c(429, 503)) {
      Sys.sleep(attempt * 2)
    } else {
      warning(
        paste(
          "Weather request failed:",
          station_id, year, datatype_id,
          "HTTP status", status_code(res)
        )
      )
      return(tibble())
    }
  }
  
  warning(
    paste(
      "Weather request failed after retries:",
      station_id, year, datatype_id
    )
  )
  
  tibble()
}

# =========================
# STEP 4: PULL DAILY WEATHER
# =========================

# Use sleep system to avoid API over usage

datatype_ids <- c("TMAX", "TMIN", "PRCP", "AWND")

weather_list <- vector("list", length = nrow(station_year_requests) * length(datatype_ids))
counter <- 1

for (i in seq_len(nrow(station_year_requests))) {
  station_i <- station_year_requests$station_id[i]
  year_i <- station_year_requests$year[i]
  
  for (datatype_i in datatype_ids) {
    weather_list[[counter]] <- get_noaa_daily_data(
      station_id = station_i,
      year = year_i,
      datatype_id = datatype_i,
      token = noaa_token,
      base_url = noaa_base_url
    )
    
    counter <- counter + 1
    Sys.sleep(0.25)
  }
}

weather_long <- bind_rows(weather_list)

# =========================
# STEP 5: RESHAPE WEATHER
# =========================

weather_daily <- weather_long |>
  distinct(station_id, date, datatype, .keep_all = TRUE) |>
  pivot_wider(
    names_from = datatype,
    values_from = value
  ) |>
  arrange(station_id, date)

# =========================
# STEP 6: MERGE WEATHER BACK
# =========================

location_dates_with_weather <- station_dates |>
  left_join(
    weather_daily,
    by = c("station_id", "match_date" = "date")
  )

matches_with_weather <- matches_with_station |>
  left_join(
    weather_daily,
    by = c("station_id", "match_date" = "date")
  )

# =========================
# STEP 7: QUICK CHECKS
# =========================

cat("Station-year requests:", nrow(station_year_requests), "\n")
cat("Daily weather rows pulled:", nrow(weather_daily), "\n")
cat("Location-dates missing TMAX:", sum(is.na(location_dates_with_weather$TMAX)), "\n")
cat("Location-dates missing TMIN:", sum(is.na(location_dates_with_weather$TMIN)), "\n")
cat("Location-dates missing PRCP:", sum(is.na(location_dates_with_weather$PRCP)), "\n")
cat("Location-dates missing AWND:", sum(is.na(location_dates_with_weather$AWND)), "\n")

weather_daily |>
  head(20)

matches_with_weather |>
  select(tourney_name, match_date, station_id, TMAX, TMIN, PRCP, AWND) |>
  head(20)

# =========================
# STEP 8: Save data
# =========================

# save cleaned daily weather dataset
write_csv(
  weather_daily,
  "data/cleaned/weather_daily.csv"
)

# save final match-level dataset with weather merged in
write_csv(
  matches_with_weather,
  "data/cleaned/matches_with_weather.csv"
)
