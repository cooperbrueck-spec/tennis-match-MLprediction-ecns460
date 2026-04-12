rm(list = ls())

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
source("code/build_final_modeling_dataset.R", echo = TRUE)

message("Pipeline complete.")
