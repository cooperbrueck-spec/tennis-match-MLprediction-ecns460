# RStudio Replication Guide

This project should be replicated **one way**: from **RStudio**, by running one script.

## Step 1: Open the project in RStudio

1. Open RStudio.
2. Open the repository as a project by clicking the file:
   - `tennis-match-MLprediction-ecns460.Rproj`

## Step 2: Install required packages (one-time setup)

Run this once in the RStudio Console:

```r
install.packages(c(
  "tidyverse", "readr", "dplyr", "purrr", "stringr", "here",
  "ggplot2", "maps", "scales", "readxl", "tidygeocoder",
  "countrycode", "httr2", "jsonlite", "tidyr", "slider", "data.table"
))
```

## Step 3: Run the full pipeline

Run this in the RStudio Console:

```r
source("code/run_all.R")
```

That single command runs the full workflow in order:

1. `identifing_modern_tournament_years.R`
2. `clean_atp_matches_2010plus.R`
3. `Player_ranking_cleaning.R`
4. `geocode_tournament_locations.R`
5. `nasa_weather_pull.R`
6. `Full_tennis_weather_data_set_cleaned_nofeatures.R`
7. `ranking_feature_contruction.R`
8. `rankings_merge.R`
9. `Data_set_visualizations.R`

## Important note

- The NASA weather step (`nasa_weather_pull.R`) can take time.
- Internet access is required for:
  - geocoding tournament locations
  - pulling NASA POWER weather data

## Final expected analysis-ready output

After completion, the main final dataset is:

- `data/cleaned/atp_matches_weather_ranking_data.csv`

## Data documentation references

If needed while replicating:

- Project overview: `README.md`
- ATP source notes: `data/raw/tennis_ATP/README.md`
- ATP match variable dictionary: `data/raw/tennis_ATP/matches_data_dictionary.txt`
