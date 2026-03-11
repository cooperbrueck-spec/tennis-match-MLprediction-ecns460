# tennis-match-MLprediction-ecns460

# ECNS 460 Term Project: Predicting Tennis Match Outcomes with Machine Learning

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

**Source:** National Oceanic and Atmospheric Administration (NOAA)

NOAA Global Historical Climatology Network (GHCN):  
https://www.ncei.noaa.gov/products/land-based-station/global-historical-climatology-network-daily

NOAA Climate Data Online portal:  
https://www.ncei.noaa.gov/cdo-web/

**Contents:**  
NOAA provides historical weather observations from meteorological stations worldwide. These datasets include daily weather measurements such as:

- temperature  
- precipitation  
- wind speed  
- humidity  
- atmospheric pressure  

**Timespan:**  
Weather station data span multiple decades and in many cases extend back to the early twentieth century.

**Spatial coverage:**  
Global weather station network.

**Why it is useful:**  
Weather conditions may influence outdoor tennis matches through factors such as temperature, humidity, and wind. These variables may affect player endurance, ball speed, and match conditions. Incorporating weather data provides an additional external dataset that can be merged with tennis match records. This additional information on its own will not predict the winner but will help reduce ommitted variable bias and allow for interaction effects with certain player charecteristics. For example a player with a strong serve may face a massive disadvantage playing in windy weather to a conservative baseline player. 

---

## Plausible Merge Strategy

The ATP historical tennis database will serve as the base match-level dataset. Within the repro Match level data will have to be merged with ranking data by player name and the relative date. Each row will represents a single match between two players and includes the tournament name, match date, and playing surface along with both players data.

Weather data from NOAA will be merged onto the dataset using the tournament location and match date. Each match will be associated with weather observations from the closest available weather station near the tournament location.

This process will allow the construction of additional contextual predictors such as match-day temperature, humidity, and wind conditions alongside player-level predictors derived from the tennis datasets. 
