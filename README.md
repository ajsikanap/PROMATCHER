#  Prognostic Score-Based Paired Randomization (PROMATCHER)

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
| `01_generate_registry.R` | Generates the historical registry with nonlinear covariate–outcome structure. |
| `02_generate_trial.R` | Simulates the randomized trial dataset from the same population. |

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

### **2_functions/**
Function used for the simulation when pairing and randomizations are perfomed.

| File | Description |
|------|--------------|
| `PROMATCHER_MC_function.R` | Function to perform Monte Carlo Simulations, that computes prognostic scores and matched subjects and computed benchmark simple randomization. |
| `make_tbl_balance_function.R` | tbd |
| `make_tbk_caliper_function.R` | tdb |
| `split_results_function.R` | tdb |
| `summary_results_function.R` | tdb |

---

### **3_simulations/**
Monte Carlo simulation study assessing operating characteristics.

| File | Description |
|------|--------------|
| `sim_glm.R` | Simulation using GLM-based scores, with varying sample sizes. |
| `sim_rf.R` | Simulation using Random Forest–based scores. |
| `sim_xgboost.R` | Simulation using XGBoost–based scores, with varying sample sizes.|
| `sim_superlearner.R` | Simulation using Super Learner ensemble scores. |


> Each scenario is repeated up to **10,000 iterations** (1,000 for Super Learner) under both null and alternative hypotheses.

---

### **4_analysis_and_visualization/**
Post-simulation analytical scripts for summarizing results and deriving theoretical benchmarks, and publication-ready plotting scripts reproducing the figures from the paper.

| File | Description |
|------|--------------|
| `41_analytical_power_formula.R` | Uses formulas to assess target power, effect size and sample size, with analytical formulas. |
| `42_summarize_results_power_typeI.R` |  Calculates empirical power and type I error. |
| `43_tables_for_paper.R` | Produces tables for paper. |
| `44_figures_for_paper.R` | Produces figures for paper. |

---

##  Execution Order

1. `0_data_generation`  
2. `1_model_training`  
3. `2_pairing_and_randomization`  
4. `3_simulations`  
5. `4_analysis_and_visualization`  

---

##  Required R Packages

```r
library(tidyverse)
library(caret)
library(SuperLearner)
library(xgboost)
library(randomForest)
library(lme4)
```
