#  Prognostic Score-Based Paired Randomization (PROMATCHER)

### Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes  
**Authors:** Ajsi Kanapari, Giulia Lorenzoni, Dario Gregori  
**Affiliation:** Unit of Biostatistics, Epidemiology and Public Health, University of Padova  
**Year:** Original 2025. Updated 2026.

---

##  Overview

This repository contains the full simulation code and case study implementation for the article:  
**“Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes.”**

The project introduces **PROMATCHER**, a prognostic score–based *paired randomization design* that integrates information from **historical data** through **machine learning models** (GLM, Random Forest, XGBoost, and Super Learner) to increase trial efficiency.  
The study evaluates its effect on **statistical power**, **type I error**, and **variance reduction** under different model specifications and caliper choices.

---

## 🗂 Repository Structure

### **0_data_generation/**
Scripts for simulating historical and trial datasets used in all scenarios.

| File | Description |
|------|--------------|
| `01_generate_registry.R` | Generates the historical registry with nonlinear covariate–outcome structure. |
| `02_generate_trial.R` | Simulates the randomized trial dataset from the same population. Additionally it containes the trial dataset with the True score value. |

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
### **2_functions/**

R functions used to run and post-process the PROMATCHER simulation study. These scripts implement the main paired-randomization simulations, sensitivity analyses, sequential enrollment settings, stratified randomization, analytical formula validation, and summary-table generation.

| File                                           | Description                                                                                                                                                                                                                                                                                                                                                                                                   |
| ---------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `PROMATCHER_MC_function.R`                     | Main Monte Carlo simulation function for prognostic score-based paired randomization. It computes prognostic scores from trained models, performs caliper-based matching, randomizes treatment within pairs, and compares paired randomization with simple randomization on the same selected subjects. Analyses include paired t-test, prognostic-score adjusted GLM, and LMM with pair as random intercept. |
| `PROMATCHER_MC_function_sensitivity.R`         | Sensitivity version of the main Monte Carlo simulation, using the same paired-randomization framework and adding alternative adjusted-analysis benchmarks for comparison.                                                                                                                                                                                                                                     |
| `PROMATCHER_COV_ADJ_MC_function_sensitivity.R` | Sensitivity simulation evaluating covariate-adjusted analyses. It compares paired randomization and equal randomization using unadjusted analysis, prognostic-score adjustment, and adjustment for trial covariates.                                                                                                                                                                                          |
| `PROMATCHER_STRATA_MC_function.R`              | Sensitivity simulation for prognostic score-based stratified randomization. Subjects are assigned to score strata, randomized within strata, and compared with equal randomization. Analyses include unadjusted GLM, score-adjusted GLM, and mixed models with stratum as random effect.                                                                                                                      |
| `PROMATCHER_SEQUENTIAL_MC_function.R`          | Sequential enrollment simulation. Subjects arrive sequentially from a pool, are placed in a waiting queue, matched greedily within the caliper, and randomized only after pairing. The function tracks matched pairs, unmatched subjects, arrived subjects, wasted patients, and includes a sign-flip permutation test.                                                                                       |
| `PROMATCHER_FORMULA_VALIDATION_MC_function.R`  | Simulation function for validating the analytical power and sample-size formulas. It computes oracle quantities such as true prognostic score, within-pair correlation, variance components, trial R², empirical attenuation factor, and predicted versus empirical power.                                                                                                                                    |
| `summary_results_function.R`                   | Helper functions to summarize simulation outputs. It computes power/type I error, Monte Carlo standard errors, coefficient bias, MSE, standard errors, balance summaries, and formatted tables for final reporting.                                                                                                                                                                                           |
| `split_results_function.R`                     | Utility function to split summarized simulation results into separate tables for operating characteristics, balance diagnostics, and bias/MSE metrics.                                                                                                                                                                                                                                                        |
| `make_tbl_balance.R`                           | Utility function to create caliper-specific balance tables, including caliper distance, prognostic-score SMD, and covariate SMD for equal versus paired randomization.                                                                                                                                                                                                                                        |
| `make_tbl_caliper_function.R`                  | Utility function to create caliper-specific formatted tables comparing power/type I error and standard errors between equal and paired randomization.                                                                                                                                                                                                                                                         |

---

---

### **3_simulations/**


Simulation scripts used to run the Monte Carlo experiments for the PROMATCHER study. These scripts execute the main paired-randomization simulations, model-specific scenarios, sensitivity analyses, stratified randomization, sequential enrollment, and analytical formula validation.

| File                               | Description                                                                                                                                                                                                                                                           |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `sim_glm.R`                    | Main simulation script for GLM-based prognostic scores. It evaluates paired randomization under different calipers, includes well-specified and misspecified score models, and runs both the large-sample scenario and varying sample-size/effect-size scenarios. |
| `sim_xgboost.R`                    | Main simulation script for XGBoost-based prognostic scores. It evaluates paired randomization under different calipers, includes well-specified and misspecified score models, and runs both the large-sample scenario and varying sample-size/effect-size scenarios. |
| `sim_rf.R`                         | Main simulation script for Random Forest-based prognostic scores. It evaluates paired randomization under different calipers for both well-specified and misspecified Random Forest models.                                                                           |
| `sim_superlearner.R`               | Main simulation script for Super Learner-based prognostic scores. It evaluates paired randomization under different calipers for both well-specified and misspecified ensemble models.                                                                                |
| `sim_glm_xgboost_20_49_cov_adj.R`  | Sensitivity simulation for GLM- and XGBoost-based prognostic scores across small sample-size scenarios. It compares paired randomization and equal randomization using unadjusted analysis, prognostic-score adjustment, and adjustment for trial covariates.         |
| `sim_glm_xgboost_20_49_strata.R`   | Sensitivity simulation for prognostic score-based stratified randomization. GLM and XGBoost scores are used to define score strata based on registry-derived deciles, and randomization is performed within strata.                                                   |
| `sim_glm_xgboost_sequential.R`     | Sequential enrollment simulation for GLM- and XGBoost-based scores. Subjects arrive sequentially from a fixed pool, are matched greedily within the caliper, randomized after pairing, and the number of unmatched or wasted subjects is recorded.                    |
| `sim_xgboost_formula_validation.R` | Formula-validation simulation for the analytical power and sample-size derivations. It evaluates empirical power against oracle quantities such as within-pair correlation, trial R², score variance, attenuation factor, and predicted power.                        |

---


> Each scenario is repeated up to **10,000 iterations** (1,000 for Super Learner) under both null and alternative hypotheses.

---

### **4_analysis_and_visualization/**

Post-simulation analytical and visualization scripts used to summarize Monte Carlo results, validate analytical benchmarks, quantify sequential-enrollment waste, and generate publication-ready tables and figures for the manuscript and Supplementary Materials.

| File                                      | Description                                                                                                                                                                                                                                                                                         |
| ----------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `analytical_power_formula.R`              | Implements analytical power and sample-size calculations for paired randomization. It includes standard unpaired calculations, paired t-test calculations accounting for within-pair correlation, and extensions linking power to prognostic score R².                                              |
| `tables_pw_typeI_300_main.R`              | Summarizes the main N = 300 per-arm simulations for GLM, Random Forest, XGBoost, and Super Learner scores. It produces formatted tables of empirical power, type I error, Monte Carlo standard errors, estimated standard errors, and balance summaries for well-specified and misspecified models. |
| `summary_tables_pw_typeI_20_49_cov_adj.R` | Summarizes the small-sample sensitivity simulations for GLM and XGBoost scores. It compares paired and equal randomization across sample sizes using unadjusted analysis, prognostic-score adjustment, and trial-covariate adjustment.                                                              |
| `summary_tables_pw_typeI_20_49_strata.R`  | Summarizes the stratified-randomization sensitivity analysis. It produces tables comparing equal and prognostic score-stratified randomization across sample sizes, including power, type I error, standard errors, and number of effective strata.                                                 |
| `tables_for_paper_waste.R`                | Summarizes the sequential-enrollment simulations. It quantifies matched pairs, unmatched subjects, excluded subjects, proportion of wasted patients, power, type I error, and permutation-test results. It also generates waste and operating-characteristic figures for the sequential setting.    |
| `tables_validation.R`                     | Produces formula-validation tables comparing empirical power with analytical predictions. It summarizes within-pair correlation, trial R², score-based attenuation factors, empirical attenuation factors, predicted power, empirical power, and power gaps.                                        |
| `plot_for_paper__f1_f2_f3.R`              | Generates the manuscript figures for the main simulation results, small-sample covariate-adjustment analysis, and stratified-randomization sensitivity analysis. It formats power and type I error plots by model, caliper, randomization strategy, and analysis method.                            |

---


##  Execution Order

1. `0_data_generation`  
2. `1_model_training`  
3. `2_pairing_and_randomization`  
4. `3_simulations`  
5. `4_analysis`  

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
