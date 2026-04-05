library(readr)
library(dplyr)
library(stringr)
library(httr)
library(jsonlite)
library(tidyr)
library(purrr)

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
# STEP 3: HELPERS
# =========================

haversine_km <- function(lat1, lon1, lat2, lon2) {
  r <- 6371
  to_rad <- pi / 180
  
  dlat <- (lat2 - lat1) * to_rad
  dlon <- (lon2 - lon1) * to_rad
  
  a <- sin(dlat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(dlon / 2)^2
  
  2 * r * atan2(sqrt(a), sqrt(1 - a))
}

get_station_candidates_once <- function(lat, lon, token, base_url, half_box = 0.5, limit = 1000) {
  res <- GET(
    paste0(base_url, "stations"),
    add_headers(token = token),
    query = list(
      datasetid = "GHCND",
      extent = paste(
        lat - half_box,
        lon - half_box,
        lat + half_box,
        lon + half_box,
        sep = ","
      ),
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

get_station_candidates_expanding <- function(lat,
                                             lon,
                                             token,
                                             base_url,
                                             min_needed = 20,
                                             search_boxes = c(0.5, 1.0, 1.5, 2.0, 3.0),
                                             limit = 1000) {
  out <- list()
  
  for (box in search_boxes) {
    this_res <- get_station_candidates_once(
      lat = lat,
      lon = lon,
      token = token,
      base_url = base_url,
      half_box = box,
      limit = limit
    )
    
    if (nrow(this_res) > 0) {
      out[[as.character(box)]] <- this_res
      combined <- bind_rows(out) |>
        distinct(id, .keep_all = TRUE)
      
      if (nrow(combined) >= min_needed) {
        return(combined)
      }
    }
    
    Sys.sleep(0.25)
  }
  
  if (length(out) == 0) {
    return(tibble())
  }
  
  bind_rows(out) |>
    distinct(id, .keep_all = TRUE)
}

# =========================
# STEP 4: FIND STATIONS
# =========================

# =========================
# STEP 4: FIND STATIONS
# =========================

station_candidates_list <- vector("list", length = nrow(location_date_ranges))

for (i in seq_len(nrow(location_date_ranges))) {
  station_candidates_list[[i]] <- get_station_candidates_expanding(
    lat = location_date_ranges$latitude[i],
    lon = location_date_ranges$longitude[i],
    token = noaa_token,
    base_url = noaa_base_url,
    min_needed = 20,
    search_boxes = c(0.5, 1.0, 1.5, 2.0, 3.0),
    limit = 1000
  )
  
  cat("Completed station search", i, "of", nrow(location_date_ranges), "\n")
  Sys.sleep(0.25)
}

station_candidates <- location_date_ranges |>
  mutate(station_results = station_candidates_list) |>
  unnest(station_results, names_sep = "_")

# =========================
# STEP 5: SCORE STATIONS
# =========================

station_candidates <- station_candidates |>
  mutate(
    station_mindate = as.Date(station_results_mindate),
    station_maxdate = as.Date(station_results_maxdate),
    distance_km = haversine_km(
      latitude,
      longitude,
      station_results_latitude,
      station_results_longitude
    ),
    covers_period = station_mindate <= start_date & station_maxdate >= end_date,
    covers_start = station_mindate <= start_date,
    covers_end = station_maxdate >= end_date,
    active_overlap_days = pmax(
      0,
      as.numeric(pmin(station_maxdate, end_date) - pmax(station_mindate, start_date)) + 1
    ),
    period_days = as.numeric(end_date - start_date) + 1,
    overlap_share = if_else(period_days > 0, active_overlap_days / period_days, 0),
    date_score = case_when(
      covers_period ~ 4,
      covers_start & covers_end ~ 4,
      overlap_share >= 0.95 ~ 3,
      overlap_share >= 0.75 ~ 2,
      overlap_share > 0 ~ 1,
      TRUE ~ 0
    )
  )

# =========================
# STEP 6: KEEP BEST CANDIDATES
# =========================

# Keep more than 5 so the weather step has real fallback options.
# You can change n = 12 to 10 or 15 if you want.
nearest_stations <- station_candidates |>
  filter(!is.na(station_results_id)) |>
  group_by(tourney_name) |>
  arrange(
    desc(date_score),
    desc(overlap_share),
    distance_km
  ) |>
  mutate(station_rank = row_number()) |>
  slice_head(n = 12) |>
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
    station_mindate,
    station_maxdate,
    distance_km,
    covers_period,
    overlap_share,
    date_score,
    station_rank
  )

# =========================
# STEP 7: OPTIONAL PARMA FIX
# =========================

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
    covers_period = TRUE,
    overlap_share = 1,
    date_score = 4,
    station_rank = 1
  )
  
  nearest_stations <- bind_rows(nearest_stations, parma_station_final)
}

# =========================
# STEP 8: MERGES
# =========================

location_date_ranges_with_station <- location_date_ranges |>
  left_join(nearest_stations, by = "tourney_name")

location_dates_with_station <- location_dates |>
  left_join(nearest_stations, by = "tourney_name")

matches_with_station <- matches_with_location |>
  left_join(nearest_stations, by = "tourney_name")

# =========================
# STEP 9: CHECKS
# =========================

cat("Total station rows:", nrow(nearest_stations), "\n")
cat("Tournaments:", n_distinct(nearest_stations$tourney_name), "\n")

nearest_stations |>
  count(tourney_name) |>
  print(n = Inf)

nearest_stations |>
  group_by(tourney_name) |>
  summarise(
    best_distance_km = min(distance_km, na.rm = TRUE),
    best_date_score = max(date_score, na.rm = TRUE),
    best_overlap_share = max(overlap_share, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = Inf)

# =========================
# STEP 10: SAVE
# =========================

write_csv(
  nearest_stations,
  "data/reference_tables/tournament_stations.csv"
)