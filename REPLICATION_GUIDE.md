# Replication Guide: ATP Match + Weather + Ranking Pipeline

This guide explains exactly how to reproduce the full data pipeline in this repository, from raw ATP files to the final analysis-ready dataset used for visualization and modeling.

---

## 1) What this project builds

Starting from raw ATP files and tournament location references, this workflow builds:

- cleaned ATP match data (2010+ main-tour events)
- cleaned player + ranking data
- geocoded tournament coordinates
- daily NASA POWER weather data for tournament windows
- merged match + weather dataset
- engineered ranking features
- final match-level dataset with ranking + weather + matchup features

Final analysis-ready file:

- `data/cleaned/atp_matches_weather_ranking_data.csv`

---

## 2) Key inputs and documentation to read first

Before running code, review these files:

1. Project overview: `README.md`
2. ATP source notes: `data/raw/tennis_ATP/README.md`
3. ATP match variable dictionary: `data/raw/tennis_ATP/matches_data_dictionary.txt`

These explain data provenance, variable meanings, ranking structure, and match-level column definitions.

---

## 3) Environment setup

## Required software

- **R** (recommended 4.2+)
- Optional but helpful: **RStudio** (`tennis-match-MLprediction-ecns460.Rproj` is included)

## Required R packages

Install once:

```r
install.packages(c(
  "tidyverse", "readr", "dplyr", "purrr", "stringr", "here",
  "ggplot2", "maps", "scales", "readxl", "tidygeocoder",
  "countrycode", "httr2", "jsonlite", "tidyr", "slider", "data.table"
))
```

## Network requirements

Two scripts require internet access:

- `code/geocode_tournament_locations.R` (OpenStreetMap geocoding via `tidygeocoder`)
- `code/nasa_weather_pull.R` (NASA POWER API)

If network is blocked, those steps will fail or return incomplete results.

---

## 4) Recommended run order (full replication)

Run scripts from the repository root. Commands below assume a terminal session in this repo.

### Step 0 (optional but recommended): confirm sample-era decision

This script compares 2000+ vs 2010+ tournament structure and writes summaries.

```bash
Rscript code/identifing_modern_tournament_years.R
```

Outputs:

- `data/raw/tennis_ATP/tournament_summary_2000plus.csv`
- `data/raw/tennis_ATP/tournament_summary_2010plus.csv`

### Step 1: clean ATP match files (core match sample)

```bash
Rscript code/clean_atp_matches_2010plus.R
```

Output:

- `data/cleaned/atp_matches_2010_clean.csv`

### Step 2: clean rankings and players files

```bash
Rscript code/Player_ranking_cleaning.R
```

Outputs:

- `data/cleaned/atp_rankings_all.csv`
- `data/cleaned/atp_players_clean.csv`

### Step 3: geocode tournament locations

```bash
Rscript code/geocode_tournament_locations.R
```

Input reference workbook:

- `data/reference_tables/Tournement_location_reference_table.xlsx`

Output:

- `data/reference_tables/tournament_locations_geocoded.csv`

### Step 4: pull NASA weather and merge to matches

```bash
Rscript code/nasa_weather_pull.R
```

Outputs:

- `data/cleaned/nasa_power_weather_daily.csv`
- `data/cleaned/atp_matches_with_nasa_weather.csv`

### Step 5: weather-data QC and save prefeature dataset

```bash
Rscript code/Full_tennis_weather_data_set_cleaned_nofeatures.R
```

Output:

- `data/cleaned/atp_matches_prefeature.csv`

### Step 6: construct ranking time-series features

```bash
Rscript code/ranking_feature_contruction.R
```

Output:

- `data/cleaned/atp_rankings_features.csv`

### Step 7: merge ranking features onto match-level weather data

```bash
Rscript code/rankings_merge.R
```

Final output:

- `data/cleaned/atp_matches_weather_ranking_data.csv`

### Step 8 (analysis/figures): generate visualizations

```bash
Rscript code/Data_set_visualizations.R
```

This script reads the final merged dataset and constructs analysis plots in-session.

---

## 5) Quick dependency map (what depends on what)

- `clean_atp_matches_2010plus.R` → `atp_matches_2010_clean.csv`
- `Player_ranking_cleaning.R` → `atp_rankings_all.csv`, `atp_players_clean.csv`
- `geocode_tournament_locations.R` → `tournament_locations_geocoded.csv`
- `nasa_weather_pull.R` needs:
  - `atp_matches_2010_clean.csv`
  - `tournament_locations_geocoded.csv`
- `Full_tennis_weather_data_set_cleaned_nofeatures.R` needs:
  - `atp_matches_with_nasa_weather.csv`
- `ranking_feature_contruction.R` needs:
  - `atp_rankings_all.csv`
  - `atp_players_clean.csv`
  - (loads `atp_matches_with_nasa_weather.csv`)
- `rankings_merge.R` needs:
  - `atp_matches_prefeature.csv`
  - `atp_rankings_features.csv`
- `Data_set_visualizations.R` needs:
  - `atp_matches_weather_ranking_data.csv`

---

## 6) Reproducibility checklist

After running all steps, verify these files exist:

- `data/cleaned/atp_matches_2010_clean.csv`
- `data/cleaned/atp_rankings_all.csv`
- `data/cleaned/atp_players_clean.csv`
- `data/reference_tables/tournament_locations_geocoded.csv`
- `data/cleaned/nasa_power_weather_daily.csv`
- `data/cleaned/atp_matches_with_nasa_weather.csv`
- `data/cleaned/atp_matches_prefeature.csv`
- `data/cleaned/atp_rankings_features.csv`
- `data/cleaned/atp_matches_weather_ranking_data.csv`

Optional validation command:

```bash
ls data/cleaned data/reference_tables
```

---

## 7) Notes and caveats

- The raw ATP files include many eras and competition levels; this workflow intentionally restricts the modeling sample to **2010+** and main-tour levels in the cleaning script.
- Weather is pulled at tournament location coordinates and merged by tournament/date windows.
- Ranking features are lagged/rolling by player and merged using the **most recent ranking on or before match date** to avoid leakage.
- Re-running API/geocoding steps at different times may produce minor differences (service updates, retries, or geocoder behavior).

---

## 8) One-command run template

If you want to run the full pipeline in one go (from repo root):

```bash
Rscript code/identifing_modern_tournament_years.R && \
Rscript code/clean_atp_matches_2010plus.R && \
Rscript code/Player_ranking_cleaning.R && \
Rscript code/geocode_tournament_locations.R && \
Rscript code/nasa_weather_pull.R && \
Rscript code/Full_tennis_weather_data_set_cleaned_nofeatures.R && \
Rscript code/ranking_feature_contruction.R && \
Rscript code/rankings_merge.R && \
Rscript code/Data_set_visualizations.R
```

