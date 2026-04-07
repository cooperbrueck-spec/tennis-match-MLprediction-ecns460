rm(list = ls())

# Ensure project-root working directory when sourced from RStudio
this_file <- normalizePath(sys.frame(1)$ofile)
project_root <- dirname(dirname(this_file))
setwd(project_root)

# WARNING: The NASA POWER weather pull step can take a long time to complete
# because it requests weather data for many tournament-year combinations.

message("Starting full pipeline from RStudio project root...")

source("code/identifing_modern_tournament_years.R", echo = TRUE)
source("code/clean_atp_matches_2010plus.R", echo = TRUE)
source("code/Player_ranking_cleaning.R", echo = TRUE)
source("code/geocode_tournament_locations.R", echo = TRUE)
source("code/nasa_weather_pull.R", echo = TRUE)
source("code/Full_tennis_weather_data_set_cleaned_nofeatures.R", echo = TRUE)
source("code/ranking_feature_contruction.R", echo = TRUE)
source("code/rankings_merge.R", echo = TRUE)
source("code/Data_set_visualizations.R", echo = TRUE)

message("Pipeline complete.")
