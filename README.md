# Tennis Match Outcome Prediction Using Machine Learning

## Project Overview

This project develops machine learning models to predict professional ATP tennis match outcomes. The goal is to evaluate whether modern predictive methods can improve upon simple ranking-based predictions of winners.

Professional tennis provides an ideal prediction setting because outcomes are influenced by measurable pre-match factors such as player rankings, Elo ratings, recent form, surface specialization, head-to-head history, workload, and weather conditions. At the same time, match outcomes remain uncertain enough that upsets occur regularly.

---

## Data Sources

The final modeling dataset was built by combining multiple sources:

### 1. ATP Match Data

Historical ATP tour-level match data from Jeff Sackmann’s publicly available tennis datasets. These files include:

- Match winners and losers  
- Tournament name, level, surface, round, and date  
- Match statistics (aces, serve percentages, etc.)  
- Rankings and ranking points  

### 2. Tournament Location Data

Tournament locations were manually standardized and geocoded in order to merge geographic and weather information.

### 3. NASA POWER Weather Data

Daily historical weather observations were collected using tournament coordinates and match dates, including:

- Temperature  
- Humidity  
- Wind speed  
- Precipitation  

### 4. Player Ranking Histories

Weekly ATP rankings and points were processed to create lagged ranking features available before each match.

---

## Data Cleaning and Construction

The project required extensive data preparation:

- Combined yearly ATP match files into a one dataset  
- Restricted analysis to the modern tennis era  
- Standardized tournament names and locations  
- Geocoded tournament cities and countries  
- Merged weather data by location and date  
- Constructed lagged ranking and points variables  
- Removed post-match information to prevent data leakage  
- Created a final modeling dataset using only pre-match predictors  

---

## Feature Engineering

A large set of pre-match predictors was engineered, including:

### Player Quality Measures

- ATP ranking difference  
- Ranking points difference  
- Elo rating difference  
- Surface-specific Elo difference  

### Recent performance Measures

- Wins in recent matches  
- Rolling win percentages  
- Ranking momentum and recent changes  

### Matchup Measures

- Head-to-head record  
- Prior meetings count  
- Serve versus return matchup variables  

### Workload / Fatigue Measures

- Matches played recently  
- Surface experience  
- Recent activity volume  

### Tournament and Context Variables

- Surface (hard, clay, grass)  
- Tournament level  
- Round  
- Best-of format  

### Weather Variables

- Temperature  
- Humidity  
- Wind speed  
- Precipitation  

---

## Prediction Setup

To create a consistent binary classification problem:

- **Player A** was defined as the **higher-ranked player entering the match**
- **Player B** was the lower-ranked player

Target variable:

- `1` = Player A wins  
- `0` = Player A loses  

---

## Training and Testing Design

The dataset was randomly divided into:

- **80% Training Data**
- **20% Testing Data**

The split used **stratification on the outcome variable**, preserving the proportion of Player A wins and losses in both samples.

This ensured that the training and testing sets had similar class balance and allowed fair model evaluation.

All model tuning was performed using **cross-validation on the training data only**. The test set was reserved for final out-of-sample evaluation.

---

## Models Estimated

## 1. Naive Benchmark

Always predict that Player A (the higher-ranked player) wins.

This provides a simple baseline showing how predictive rankings already are.

---

## 2. LASSO Logistic Regression

A penalized logistic regression model was estimated using LASSO regularization.

---

## 3. Gradient Boosted Trees (XGBoost)

A nonlinear model was estimated using gradient boosted decision trees.

Advantages:

- Captures nonlinear relationships  
- Learns interactions automatically  
- Handles complex thresholds  
- Often strong predictive performance  


---

## Evaluation Metrics

Models were compared on the held-out test set using:

- Accuracy  
- ROC AUC  
- Sensitivity  
- Specificity  
- Mean Log Loss  

---

## Final Test Set Results

| Model | Accuracy | ROC AUC | Sensitivity | Specificity |
|------|----------|---------|------------|------------|
| Naive (Always Player A) | 0.652 | 0.500 | 1.000 | 0.000 |
| LASSO | 0.650 | 0.612 | 0.795 | 0.379 |
| XGBoost | 0.669 | 0.679 | 0.893 | 0.251 |

---

## Main Findings

### Rankings Alone Are Strong

Simply predicting the higher-ranked player won **65.2%** of test matches.

### Machine Learning Added Modest Predictive Gains

The XGBoost model improved accuracy to **66.9%** and produced the strongest overall performance.

### Nonlinear Methods Outperformed Linear Methods

The boosted tree model outperformed LASSO on:

- Accuracy  
- ROC AUC  
- Probability calibration  

### Upsets Remain Difficult to Predict

LASSO was somewhat better at identifying upset outcomes, but no model predicted surprises consistently well.

### Most Important Predictors Included

- Elo rating advantage  
- Surface Elo advantage  
- Ranking measures  
- Serve / return matchup indicators  
- Weatehr was not predictive  
