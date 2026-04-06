library(readr)
library(dplyr)
library(httr2)
library(jsonlite)
library(purrr)
library(tidyr)

# =========================
# LOAD DATA
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
# PREPARE MATCH DATA
# =========================

atp_matches <- atp_matches |>
  mutate(
    match_date = as.Date(as.character(tourney_date), format = "%Y%m%d"),
    tourney_year = format(match_date, "%Y")
  )

matches_with_location <- atp_matches |>
  left_join(
    tournament_locations |>
      select(tourney_name, latitude, longitude),
    by = "tourney_name"
  )

tournament_requests <- matches_with_location |>
  distinct(tourney_name, tourney_year, latitude, longitude, match_date) |>
  group_by(tourney_name, tourney_year, latitude, longitude) |>
  summarise(
    start_date = min(match_date, na.rm = TRUE),
    end_date = max(match_date, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(latitude), !is.na(longitude))

# =========================
# NASA POWER HELPER
# =========================

get_nasa_power_daily <- function(lat, lon, start_date, end_date) {
  
  url <- "https://power.larc.nasa.gov/api/temporal/daily/point"
  
  req <- request(url) |>
    req_url_query(
      parameters = "T2M_MAX,T2M_MIN,PRECTOTCORR,WS2M,RH2M",
      community = "RE",
      longitude = sprintf("%.4f", lon),
      latitude = sprintf("%.4f", lat),
      start = format(start_date, "%Y%m%d"),
      end = format(end_date, "%Y%m%d"),
      format = "JSON",
      `time-standard` = "LST"
    ) |>
    req_options(timeout = 60)
  
  resp <- req_perform(req)
  
  if (resp_status(resp) != 200) {
    warning(
      paste(
        "NASA POWER request failed:",
        "lat =", lat,
        "lon =", lon,
        "start =", as.character(start_date),
        "end =", as.character(end_date),
        "status =", resp_status(resp)
      )
    )
    return(tibble())
  }
  
  txt <- resp_body_string(resp)
  parsed <- fromJSON(txt, simplifyDataFrame = TRUE)
  
  if (is.null(parsed$properties$parameter)) {
    return(tibble())
  }
  
  param_list <- parsed$properties$parameter
  
  out <- tibble(
    date = names(param_list$T2M_MAX),
    T2M_MAX = as.numeric(param_list$T2M_MAX),
    T2M_MIN = as.numeric(param_list$T2M_MIN),
    PRECTOTCORR = as.numeric(param_list$PRECTOTCORR),
    WS2M = as.numeric(param_list$WS2M),
    RH2M = as.numeric(param_list$RH2M)
  ) |>
    mutate(
      date = as.Date(date, format = "%Y%m%d"),
      T2M_MAX = na_if(T2M_MAX, -999),
      T2M_MIN = na_if(T2M_MIN, -999),
      PRECTOTCORR = na_if(PRECTOTCORR, -999),
      WS2M = na_if(WS2M, -999),
      RH2M = na_if(RH2M, -999)
    )
  
  out
}

# =========================
# PULL WEATHER
# =========================

weather_list <- vector("list", length = nrow(tournament_requests))

for (i in seq_len(nrow(tournament_requests))) {
  this_tourney <- tournament_requests$tourney_name[i]
  this_year <- tournament_requests$tourney_year[i]
  this_lat <- tournament_requests$latitude[i]
  this_lon <- tournament_requests$longitude[i]
  this_start <- tournament_requests$start_date[i]
  this_end <- tournament_requests$end_date[i]
  
  cat(
    "Working on", i, "of", nrow(tournament_requests), "-",
    this_tourney, this_year, "\n"
  )
  flush.console()
  
  weather_list[[i]] <- tryCatch(
    get_nasa_power_daily(
      lat = this_lat,
      lon = this_lon,
      start_date = this_start,
      end_date = this_end
    ) |>
      mutate(
        tourney_name = this_tourney,
        tourney_year = this_year,
        latitude = this_lat,
        longitude = this_lon,
        .before = 1
      ),
    error = function(e) {
      cat("Failed on", this_tourney, this_year, "-", conditionMessage(e), "\n")
      flush.console()
      tibble(
        tourney_name = this_tourney,
        tourney_year = this_year,
        latitude = this_lat,
        longitude = this_lon,
        date = seq(this_start, this_end, by = "day"),
        T2M_MAX = NA_real_,
        T2M_MIN = NA_real_,
        PRECTOTCORR = NA_real_,
        WS2M = NA_real_,
        RH2M = NA_real_
      )
    }
  )
  
  Sys.sleep(0.1)
}

weather_daily <- bind_rows(weather_list) |>
  distinct()

# =========================
# MERGE TO MATCH DATA
# =========================

matches_with_weather <- matches_with_location |>
  left_join(
    weather_daily |>
      select(
        tourney_name,
        tourney_year,
        match_date = date,
        T2M_MAX,
        T2M_MIN,
        PRECTOTCORR,
        WS2M,
        RH2M
      ),
    by = c("tourney_name", "tourney_year", "match_date")
  )

# =========================
# QUICK CHECKS
# =========================

cat("\nFinished NASA POWER pull\n")
cat("Tournament instances:", nrow(tournament_requests), "\n")
cat("Weather rows:", nrow(weather_daily), "\n")
cat("Match rows:", nrow(matches_with_weather), "\n")
cat("Missing T2M_MAX:", sum(is.na(matches_with_weather$T2M_MAX)), "\n")
cat("Missing T2M_MIN:", sum(is.na(matches_with_weather$T2M_MIN)), "\n")
cat("Missing PRECTOTCORR:", sum(is.na(matches_with_weather$PRECTOTCORR)), "\n")
cat("Missing WS2M:", sum(is.na(matches_with_weather$WS2M)), "\n")
cat("Missing RH2M:", sum(is.na(matches_with_weather$RH2M)), "\n")

# =========================
# SAVE DATA
# =========================

write_csv(
  weather_daily,
  "data/cleaned/nasa_power_weather_daily.csv"
)

write_csv(
  matches_with_weather,
  "data/cleaned/atp_matches_with_nasa_weather.csv"
)