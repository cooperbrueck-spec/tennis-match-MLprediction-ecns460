rm(list = ls())

library(here)

# WARNING: The NASA POWER weather pull step can take a long time to complete
# because it requests weather data for many tournament-year combinations. The XG model seleection and tuning can also take a long time. 

message("Starting full pipeline from RStudio project root...")

pipeline_scripts <- c(
  "01_clean_atp_matches_2010plus.R",
  "02_clean_player_rankings.R",
  "03_geocode_tournament_locations.R",
  "04_pull_nasa_weather.R",
  "05_validate_match_weather_dataset.R",
  "06_construct_ranking_features.R",
  "07_merge_rankings_into_matches.R",
  "08_create_dataset_visualizations.R",
  "09_build_final_modeling_dataset.R",
  "10_run_lasso_regression.R",
  "11_visualize_lasso_results.R",
  "12_run_xgboost_model.R",
  "13_final_test_set_evaluation.R",
  "14_visualize_xgboost_results.R",
  "15_visualize_test_set_diagnostics.R",
  "16_create_model_comparison_figures.R"
)

for (script_name in pipeline_scripts) {
  source(here("code", script_name), echo = TRUE)
}

message("Pipeline complete.")
