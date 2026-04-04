library(readr)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)

# =========================
# PRE-STEP: NOAA API SETUP
# =========================

noaa_token <- "yAWAAQsLJgQtBWlUoTaNJdGYNcfMNoCV"
noaa_base_url <- "https://www.ncdc.noaa.gov/cdo-web/api/v2/"

# =========================
# STEP 1: LOAD DATA
# =========================

atp_matches <- read_csv(
  "data/cleaned/atp_matches_2010_clean.csv",
  show_col_types = FALSE
)

tournament_locations <- read_csv(
  "data/reference_tables/tournament_locations_geocoded.csv",
  show_col_types = FALSE
)

# =========================
# STEP 2: PREPARE MATCH DATA
# =========================

atp_matches <- atp_matches |>
  mutate(
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d")
  )

matches_with_location <- atp_matches |>
  left_join(
    tournament_locations |>
      select(tourney_name, latitude, longitude),
    by = "tourney_name"
  )

location_dates <- matches_with_location |>
  select(tourney_name, latitude, longitude, match_date) |>
  distinct()

location_date_ranges <- location_dates |>
  group_by(tourney_name, latitude, longitude) |>
  summarise(
    start_date = min(match_date, na.rm = TRUE),
    end_date = max(match_date, na.rm = TRUE),
    n_match_dates = n_distinct(match_date),
    .groups = "drop"
  ) |>
  arrange(tourney_name)

# =========================
# STEP 3: FIND STATION CANDIDATES
# =========================

get_station_candidates <- function(lat, lon, token, base_url, limit = 10) {
  res <- GET(
    paste0(base_url, "stations"),
    add_headers(token = token),
    query = list(
      datasetid = "GHCND",
      extent = paste(lat - 0.5, lon - 0.5, lat + 0.5, lon + 0.5, sep = ","),
      limit = limit
    )
  )
  
  if (status_code(res) != 200) {
    return(tibble())
  }
  
  parsed <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)
  
  if (is.null(parsed$results)) {
    return(tibble())
  }
  
  as_tibble(parsed$results)
}

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  to_rad <- pi / 180
  
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

station_candidates_list <- vector("list", length = nrow(location_date_ranges))

for (i in seq_len(nrow(location_date_ranges))) {
  station_candidates_list[[i]] <- get_station_candidates(
    location_date_ranges$latitude[i],
    location_date_ranges$longitude[i],
    noaa_token,
    noaa_base_url
  )
  Sys.sleep(0.25)
}

station_candidates <- location_date_ranges |>
  mutate(station_results = station_candidates_list) |>
  unnest(station_results, names_sep = "_")

# compute distance
station_candidates <- station_candidates |>
  mutate(
    distance_km = haversine_km(
      latitude,
      longitude,
      station_results_latitude,
      station_results_longitude
    )
  )

# =========================
# NEW: KEEP TOP 5 STATIONS
# =========================

nearest_stations <- station_candidates |>
  group_by(tourney_name) |>
  arrange(distance_km) |>
  mutate(station_rank = row_number()) |>
  slice_head(n = 5) |>
  ungroup() |>
  select(
    tourney_name,
    tournament_latitude = latitude,
    tournament_longitude = longitude,
    start_date,
    end_date,
    n_match_dates,
    station_id = station_results_id,
    station_name = station_results_name,
    station_latitude = station_results_latitude,
    station_longitude = station_results_longitude,
    station_mindate = station_results_mindate,
    station_maxdate = station_results_maxdate,
    distance_km,
    station_rank
  )

# =========================
# FIX PARMA (ONLY ADD IF MISSING)
# =========================

nearest_stations <- nearest_stations |>
  mutate(
    station_mindate = as.Date(station_mindate),
    station_maxdate = as.Date(station_maxdate)
  )

missing_station <- location_date_ranges |>
  anti_join(nearest_stations, by = "tourney_name")

if ("Parma" %in% missing_station$tourney_name) {
  parma_station_final <- tibble(
    tourney_name = "Parma",
    tournament_latitude = missing_station$latitude[1],
    tournament_longitude = missing_station$longitude[1],
    start_date = missing_station$start_date[1],
    end_date = missing_station$end_date[1],
    n_match_dates = missing_station$n_match_dates[1],
    station_id = "GHCND:IT000016090",
    station_name = "VERONA VILLAFRANCA, IT",
    station_latitude = 45.3831,
    station_longitude = 10.8667,
    station_mindate = as.Date("1945-05-18"),
    station_maxdate = as.Date("2025-08-24"),
    distance_km = haversine_km(
      missing_station$latitude[1],
      missing_station$longitude[1],
      45.3831,
      10.8667
    ),
    station_rank = 1
  )
  
  nearest_stations <- bind_rows(nearest_stations, parma_station_final)
}

# =========================
# MERGES (UNCHANGED)
# =========================

location_date_ranges_with_station <- location_date_ranges |>
  left_join(nearest_stations, by = "tourney_name")

location_dates_with_station <- location_dates |>
  left_join(nearest_stations, by = "tourney_name")

matches_with_station <- matches_with_location |>
  left_join(nearest_stations, by = "tourney_name")

# =========================
# CHECKS
# =========================

cat("Total station rows:", nrow(nearest_stations), "\n")
cat("Tournaments:", n_distinct(nearest_stations$tourney_name), "\n")

nearest_stations |>
  count(tourney_name) |>
  print()

# =========================
# SAVE
# =========================

write_csv(
  nearest_stations,
  "data/reference_tables/tournament_stations.csv"
)
