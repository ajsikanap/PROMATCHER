# PROMATCHER
README — Prognostic Score-Based Paired Randomization (ProMatcheR)
Overview

This repository contains the R code used for the simulation study presented in the manuscript (Unpublished):
“Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes.”
Authors: Ajsi Kanapari, Giulia Lorenzoni, Dario Gregori — University of Padova.

The project evaluates a paired randomization design based on prognostic scores estimated from historical data using machine learning models (GLM, Random Forest, XGBoost, and Super Learner). Simulations quantify effects on statistical power and Type I error under various modeling and matching conditions.

Repository Structure
01_ Registry and Data Generation

01_generateRegistry_with_nonlinear_r.R
Generates the historical registry with nonlinear covariate–outcome relationships.

03_RCT_dataset_generation.R
Simulates the RCT dataset from the same population structure for controlled experiments.

02_ Model Training (Prognostic Score Estimation)

02_a_train_glm.R – Fits Generalized Linear Models to historical data.

02_b_train_rf.R – Fits Random Forest models.

02_c_train_xgboost.R – Fits XGBoost models.

02_d_train_superlearner.R – Combines models via Super Learner ensemble.

Each script outputs predicted prognostic scores used for matching.

04_ Simulation Study (Main Scenarios)

04_a_simulation_glm.R

04_b_simulation_rf.R

04_c_simulation_xgboost.R

04_d_simulation_sl.R

Implements Monte Carlo simulations (10,000 iterations) for each model to evaluate power and Type I error under well-specified and misspecified models.

05_ Summary Tables

05_a_summary_tables_pw_typeI.R
Aggregates simulation outputs and computes summary statistics (power, Type I error, SE, SMD).

06_ Sensitivity and Extended Simulations

06_a_simulation_glm_ss50.R

06_b_simulation_rf_ss50.R

06_c_simulation_xgboost_ss50.R

06_d_simulation_glm_xgboost_ss20_49.R
Tests alternative sample sizes and caliper widths (0.01, 0.05, 0.10) to validate robustness.

06_tables_for_paper.R
Produces final simulation tables (used in Tables 1–2 of the paper).

07_ Plotting

07_a_plot_for_paper.R
Generates power and Type I error figures (Figures 1–2 of the paper).

07_b_temp_plot_for_paper.R
Intermediate plotting utilities for visual inspection.

08_ Auxiliary and Analytical Tools

08_h0_samplesize.R
Analytical power and sample size calculations for paired vs. unpaired randomization.

20250806_continuous_output_paired_...R
Implements theoretical formulae for the variance reduction and analytical power curves.

_temp_models_delete.R
Temporary utilities for clearing model cache.

Output Summary

Each simulation script produces:

Summary tables of empirical power and Type I error.

Model performance metrics (R², RMSE).

Figures illustrating power/type I error across calipers and analysis models.

Datasets for tabular reporting and manuscript integration.

Reproducibility

Scripts are modular. Execution order:

01_generateRegistry_with_nonlinear_r.R

03_RCT_dataset_generation.R

02_*_train_*.R

04_*_simulation_*.R

05_a_summary_tables_pw_typeI.R

06_*_simulation_* + 06_tables_for_paper.R

07_a_plot_for_paper.R

08_h0_samplesize.R

All scripts require:

library(tidyverse)
library(caret)
library(SuperLearner)
library(xgboost)
library(randomForest)
library(lme4)



Kanapari A., Lorenzoni G., Gregori D.
Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes.
University of Padova, 2025.
