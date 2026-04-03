library(readxl)
library(dplyr)
library(stringr)
library(tidygeocoder)
library(countrycode)
library(readr)

# load tournament location reference table from the Excel workbook
# using the locations_data sheet only
tournament_locations <- read_excel(
  "data/reference_tables/Tournement_location_reference_table.xlsx",
  sheet = "locations_data"
)

# convert ISO3 country codes to full country names for better geocoding
# then create a standardized geocoding query
tournament_locations_geo <- tournament_locations |>
  mutate(
    country_name = countrycode(country, origin = "iso3c", destination = "country.name"),
    query = paste(city, country_name, sep = ", ")
  )

# geocode locations using OpenStreetMap through tidygeocoder
tournament_locations_geo <- tournament_locations_geo |>
  geocode(
    address = query,
    method = "osm",
    lat = latitude,
    long = longitude
  )

# quick checks
cat("Total locations:", nrow(tournament_locations_geo), "\n")
cat("Missing coordinates:", sum(is.na(tournament_locations_geo$latitude) | is.na(tournament_locations_geo$longitude)), "\n")

tournament_locations_geo |>
  select(tourney_name, city, country, country_name, latitude, longitude) |>
  head(20)

# inspect remaining missing locations
missing_locations <- tournament_locations_geo |>
  filter(is.na(latitude) | is.na(longitude)) |>
  select(tourney_name, city, country, country_name, query)

missing_locations

# All tournament locations have Geo codes now I save the dataset
write_csv(
  tournament_locations_geo,
  "data/reference_tables/tournament_locations_geocoded.csv"
)
