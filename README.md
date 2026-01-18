## Predictive Aerobiological Model Development ##

This repository provides the analysis workflow and example data required to reproduce the modeling framework presented in the associated manuscript.

## Overview ##
The study applies statistical techniques, including Spearman correlation test, multiple linear regression (MLR), and combination of MLR and Principal Component Analysis (PCA), 
to evaluate the relationship between airborne fungal spore concentrations and meteorological parameters in a paddy agroecosystem across different phenological stages of paddy growth.

Due to institutional data-sharing restrictions, the full raw dataset used in the study cannot be publicly released. 
However, all analysis scripts and a representative example dataset are provided to ensure transparency and reproducibility of the modeling workflow.

## Repository contents ##
We have provided the following files that can be used for model development to predict daily concentration of Curvularia sp. above the paddy field:

-->data, which contain 4 files:
	1.curvularia_data_afterAICselection (csv)
	2.curvularia_data_afterAICselection (Excel)
	3.curvularia_data_original (csv)
	4.curvularia_data_original (Excel)

-->code, which contain 4 files:
	1. 01_data_preprocessing_spearman
	2. 02_data_preprocessing_AIC
	3. 03_model_development_MLR
	4. 04_model_development_PCR

## Reproducibility
R scripts are provided and running this script in numerical order (`01` → `04`) will reproduce the full modeling workflow using the example dataset.

## Requirements
The analysis was conducted using R (version ≥ 4.2.0) with the following packages:
- library(stats)
- library(dplyr)
- library(readxl)
- library(caret)
- library(car)
- library(lmtest)
- library(ggplot2)
- library(randomForest)

## Data availability statement

The full raw datasets generated during the current study are not publicly available due to institutional restrictions but may be made available from the corresponding author upon reasonable request and subject to approval.


