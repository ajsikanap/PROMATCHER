# Prognostic Score-Based Paired Randomization (PROMATCHER)

### Impact of Prognostic Score-Based Paired Randomization on the Operating Characteristics of Clinical Trials with Continuous Outcomes  
**Authors:** Ajsi Kanapari, Giulia Lorenzoni, Dario Gregori  
**Affiliation:** Unit of Biostatistics, Epidemiology and Public Health, University of Padova  
**Year:** 2025  
**Status:** Not submitted yet.

---

## Overview

This repository contains the R code for the simulation study presented in the paper above.  
The project introduces **ProMatcheR**, a prognostic score–based **paired randomization design** aimed at increasing the statistical efficiency of clinical trials by leveraging **historical data and machine learning models** (GLM, Random Forest, XGBoost, Super Learner).

Simulations quantify how model performance, caliper width, and analytical strategy affect **power** and **Type I error**, both under correct and misspecified models.

---

## 🗂 Repository Structure

### **01_ Registry and Data Generation**
| File | Description |
|------|--------------|
| `01_generateRegistry_with_nonlinear_r.R` | Generates a historical registry with nonlinear covariate–outcome relationships. |
| `03_RCT_dataset_generation.R` | Simulates the RCT dataset from the same population structure. |

---

### **02_ Model Training (Prognostic Score Estimation)**
| File | Description |
|------|--------------|
| `02_a_train_glm.R` | Fits Generalized Linear Models (baseline benchmark). |
| `02_b_train_rf.R` | Fits Random Forest models. |
| `02_c_train_xgboost.R` | Fits XGBoost models. |
| `02_d_train_superlearner.R` | Combines all models using the Super Learner ensemble. |

Each script outputs predicted **prognostic scores** used for subject matching in the simulated trials.

---

### **04_ Simulation Study (Main Scenarios)**
| File | Description |
|------|--------------|
| `04_a_simulation_glm.R` | Simulation using GLM scores. |
| `04_b_simulation_rf.R` | Simulation using Random Forest scores. |
| `04_c_simulation_xgboost.R` | Simulation using XGBoost scores. |
| `04_d_simulation_sl.R` | Simulation using Super Learner ensemble scores. |

> Each scenario is repeated **10,000 times** (1,000 for Super Learner) to estimate **power** and **Type I error** under the null and alternative hypotheses.

---

### **05_ Summary Tables**
| File | Description |
|------|--------------|
| `05_a_summary_tables_pw_typeI.R` | Aggregates results and computes power, Type I error, standard errors, and standardized mean differences (SMD). |

---

### **06_ Sensitivity and Extended Simulations**
| File | Description |
|------|--------------|
| `06_a_simulation_glm_ss50.R` | Sensitivity with increased sample size (n=50). |
| `06_b_simulation_rf_ss50.R` | Same as above for RF. |
| `06_c_simulation_xgboost_ss50.R` | Same as above for XGBoost. |
| `06_d_simulation_glm_xgboost_ss20_49.R` | Varying sample sizes (n=20–49). |
| `06_tables_for_paper.R` | Produces final tables included in the manuscript. |


---

### **07_ Auxiliary and Analytical Tools**
| File | Description |
|------|--------------|
| `08_h0_samplesize.R` | Analytical power and sample size computation for paired vs. unpaired designs. |
| `20250806_continuous_output_paired_...R` | Implements variance and analytical power formulae. |


---

## 📊 Output Summary

The simulation script produces:
- Power and Type I error estimates (per model, caliper, and analysis).  
- Model performance metrics (R², RMSE, MAE).  
- Data tables for manuscript inclusion (Tables 1–2).  

---

## Reproducibility

**Execution order:**
1. `01_generateRegistry_with_nonlinear_r.R`  
2. `03_RCT_dataset_generation.R`  
3. `02_*_train_*.R`  
4. `04_*_simulation_*.R`  
5. `05_a_summary_tables_pw_typeI.R`  
6. `06_*_simulation_*` + `06_tables_for_paper.R`  
7. `07_a_plot_for_paper.R`  
8. `08_h0_samplesize.R`

**Required R packages:**
```r
library(tidyverse)
library(caret)
library(SuperLearner)
library(xgboost)
library(randomForest)
library(lme4)
```
