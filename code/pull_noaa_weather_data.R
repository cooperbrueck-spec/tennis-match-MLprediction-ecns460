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

get_noaa_daily_data <- function(station_id, year, datatype_id, token, base_url) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  
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
  
  if (status_code(res) == 429) {
    Sys.sleep(1)
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
  }
  
  if (status_code(res) != 200) {
    warning(
      paste(
        "Weather request failed:",
        station_id, year, datatype_id,
        "HTTP status", status_code(res)
      )
    )
    return(tibble())
  }
  
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (is.null(parsed$results)) {
    return(tibble())
  }
  
  as_tibble(parsed$results) |>
    transmute(
      station_id = station,
      date = as.Date(substr(date, 1, 10)),
      datatype = datatype,
      value = value
    )
}

# =========================
# STEP 4: PULL DAILY WEATHER
# =========================

datatype_ids <- c("TMAX", "TMIN", "PRCP")

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

weather_daily |>
  head(20)

matches_with_weather |>
  select(tourney_name, match_date, station_id, TMAX, TMIN, PRCP) |>
  head(20)