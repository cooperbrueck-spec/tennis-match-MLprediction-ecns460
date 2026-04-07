# tennis-match-MLprediction-ecns460

# ECNS 460 Term Project: Predicting Tennis Match Outcomes with Machine Learning

## Reproduction in RStudio
Use this as the single replication method: open the project in RStudio and run `source("code/run_all.R")`.

This executes the full pipeline sequentially (including the NASA weather pull step) using only project-relative paths.

---

## Name
- Cooper Brueck

## Objective
This project aims to use machine learning models to predict professional tennis match outcomes using historical match results and player and match characteristics.

## Motivation
Sports provide a useful real-world setting for prediction because extensive data is available and many unknown match outcomes can be predicted. Tennis is especially well suited for predictive modeling because match outcomes depend on measurable characteristics such as player rankings, recent performance, and surface-specific ability.

## Tentative Analysis Plan
This project will follow the predictive analysis option. The main goal is to predict match winners as accurately as possible using historical tennis data. I plan to construct a dataset from historical match results and player rankings and use it to train models. A baseline logistic regression model will be compared with more advanced machine learning methods. Model performance will be evaluated using prediction accuracy.

## Challenge
The challenge component of this project is to learn and apply advanced machine learning methods beyond the models covered in class.

---

# Datasets

## Dataset 1: Historical Professional Tennis Data

**Source:** Jeff Sackmann Tennis Data Repositories

Primary repository:  
url  = https://github.com/JeffSackmann/tennis_atp  

**Contents:**  
These repositories contain a comprehensive historical database of professional men's tennis matches and rankings. The project will use the full datasets which include match-level records for ATP tournaments as well as weekly player ranking data and additional player performance/strength metrics. I feel this data while perhaps not exactly from the raw source is still far from a research ready and prebuilt data set. It will require multiple merges, and extensive data cleaning. 

Match data include variables such as:

- tournament name and location  
- match date  
- surface type (clay, grass, hard)  
- player names  
- player rankings at the time of the match  
- match scores  
- match winner and loser  

Ranking and player files include:

- weekly ranking date  
- player ranking position  
- ranking points
- serve and swing metrics 


**Timespan:**  
Match data span from 1968 to the present however I will likely focus on a more recent subset where more advanced data is collected.

**Spatial coverage:**  
International professional ATP tournaments.

**Why it is useful:**  
These datasets provide the core match outcomes that the machine learning models will attempt to predict. They also include key contextual variables such as surface type and player rankings that can be used to construct predictive features.

---

## Dataset 2: Historical Weather Data

**Source:** NASA POWER (Prediction Of Worldwide Energy Resources)

NASA POWER API:  
https://power.larc.nasa.gov/

**Contents:**  
NASA POWER provides global, gridded meteorological data based on latitude, longitude, and date. The dataset includes daily weather variables such as:

- maximum temperature (T2M_MAX)  
- minimum temperature (T2M_MIN)  
- total precipitation (PRECTOTCORR)  
- wind speed (WS2M)  
- relative humidity (RH2M)  

These variables are derived from a combination of satellite observations, ground measurements, and atmospheric models, and are provided in a consistent format across all geographic locations.

**Timespan:**  
Daily data are available globally from 1981 to the present.

**Spatial coverage:**  
Global coverage on a gridded system (approximately 0.5° resolution).

**Why it is useful:**  
Weather conditions may influence outdoor tennis matches through factors such as temperature, humidity, precipitation, and wind. These variables can affect player endurance, ball speed, and overall match conditions. An initial station-based approach using NOAA weather data was considered. However, station-based data require selecting nearby weather stations and handling missing observations, which introduces complexity and potential inconsistencies across locations. The NASA POWER dataset allows weather to be assigned directly using tournament latitude, longitude, and match date, ensuring complete coverage and consistent measurement across all tournaments. A limitation of this approach is that the data are spatially averaged over grid cells (approximately 50 km), which may smooth localized weather variation relative to station-based observations. However, the gain in completeness and consistency is well suited for a global match-level analysis.

---

## Weather Data Merge Strategy

Weather data will be merged to the match-level dataset using tournament location and match date. Each tournament is associated with a fixed latitude and longitude, and daily weather variables are retrieved for the corresponding date range using the NASA POWER API.

Weather variables are then merged to each match by matching on:

- tournament name  
- tournament year  
- match date  

This approach assigns consistent weather conditions to each match without requiring station selection or imputation of missing observations. Because the weather data are provided as a complete daily time series for each location, all matches receive corresponding weather values.
