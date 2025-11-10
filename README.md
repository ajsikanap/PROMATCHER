#  Prognostic Score-Based Paired Randomization (ProMatcheR)

### Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes  
**Authors:** Ajsi Kanapari, Giulia Lorenzoni, Dario Gregori  
**Affiliation:** Unit of Biostatistics, Epidemiology and Public Health, University of Padova  
**Year:** 2025  

---

##  Overview

This repository contains the full simulation code and case study implementation for the article:  
**“Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes.”**

The project introduces **ProMatcheR**, a prognostic score–based *paired randomization design* that integrates information from **historical data** through **machine learning models** (GLM, Random Forest, XGBoost, and Super Learner) to increase trial efficiency.  
The study evaluates its effect on **statistical power**, **type I error**, and **variance reduction** under different model specifications and caliper choices.

---

## 🗂 Repository Structure

### **0_data_generation/**
Scripts for simulating historical and trial datasets used in all scenarios.

| File | Description |
|------|--------------|
| `00_generate_registry.R` | Generates the historical registry with nonlinear covariate–outcome structure. |
| `01_generate_trial.R` | Simulates the randomized trial dataset from the same population. |
| `02_simulation_parameters.R` | Defines global simulation constants (sample sizes, calipers, effect sizes, etc.). |

---

### **1_model_training/**
Training of prognostic models on historical data.

| File | Description |
|------|--------------|
| `train_glm.R` | Fits Generalized Linear Models (baseline model). |
| `train_rf.R` | Fits Random Forest models capturing nonlinear relationships. |
| `train_xgboost.R` | Fits XGBoost gradient-boosted models. |
| `train_superlearner.R` | Builds a Super Learner ensemble combining all models. |

Each script outputs predicted **prognostic scores**, later used for subject pairing.

---

### **2_pairing_and_randomization/**
Implements the matching and allocation mechanism of ProMatcheR.

| File | Description |
|------|--------------|
| `form_pairs.R` | Forms subject pairs based on prognostic scores. |
| `apply_calipers.R` | Applies distance thresholds (calipers = 0.01, 0.05, 0.10). |
| `assign_treatment.R` | Randomly allocates treatment within each pair (1:1). |

---

### **3_simulations/**
Monte Carlo simulation study assessing operating characteristics.

| File | Description |
|------|--------------|
| `sim_glm.R` | Simulation using GLM-based scores. |
| `sim_rf.R` | Simulation using Random Forest–based scores. |
| `sim_xgboost.R` | Simulation using XGBoost–based scores. |
| `sim_superlearner.R` | Simulation using Super Learner ensemble scores. |
| `sim_sensitivity_sample_size.R` | Varying sample sizes (n = 16–49). |
| `sim_sensitivity_caliper.R` | Varying caliper thresholds to assess robustness. |

> Each scenario is repeated up to **10,000 iterations** (1,000 for Super Learner) under both null and alternative hypotheses.

---

### **4_analysis/**
Post-simulation analytical scripts for summarizing results and deriving theoretical benchmarks.

| File | Description |
|------|--------------|
| `summarize_results.R` | Aggregates Monte Carlo outputs into summary tables. |
| `compute_power_typeI.R` | Calculates empirical power and type I error. |
| `analytical_power_formula.R` | Implements the theoretical power and variance formulas. |
| `compare_models.R` | Compares performance across ML algorithms and calipers. |

---

### **5_visualization/**
Publication-ready plotting scripts reproducing the figures from the paper.

| File | Description |
|------|--------------|
| `plot_power_typeI.R` | Generates power and type I error bar plots (Figures 1–2). |
| `plot_model_performance.R` | Visualizes R², RMSE, and model performance metrics. |
| `plot_case_study.R` | Plots results from the real-data application. |

---

### **6_case_study/**
Application of ProMatcheR to the twin WOMAC osteoarthritis trials.

| File | Description |
|------|--------------|
| `apply_promatcher_womac.R` | Applies prognostic-score pairing to the WOMAC data. |
| `summarize_case_results.R` | Produces the re-analysis tables used in the paper (Table 2). |

---

### **7_utils/**
Helper functions and support scripts.

| File | Description |
|------|--------------|
| `helper_functions.R` | Core utility functions (e.g., performance metrics, matching helpers). |
| `package_loading.R` | Loads required R packages and global options. |
| `model_cleanup.R` | Clears model cache and temporary outputs. |

---

##  Execution Order

1. `0_data_generation`  
2. `1_model_training`  
3. `2_pairing_and_randomization`  
4. `3_simulations`  
5. `4_analysis`  
6. `5_visualization`  
7. `6_case_study`

---

##  Required R Packages

```r
library(tidyverse)
library(caret)
library(SuperLearner)
library(xgboost)
library(randomForest)
library(lme4)

