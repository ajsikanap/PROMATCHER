# ======================================================================
# Script: summarize_all_simulations.R
# Purpose: Summarize results from all simulation outputs (good/bad models)
# Outputs: Power, Type I error, SE, SMD, Bias, MSE summaries
# ======================================================================

library(flextable)

# --- Load summary and helper functions --------------------------------
source("2_functions/PROMATCHER_MC_function.R")
source("2_functions/split_results_function.R")
source("2_functions/summary_results_function.R")

# --- Simulation parameters --------------------------------------------
iterations <- 10000
sample_size <- 300

# ======================================================================
# GLM
# ======================================================================
load("output/sim_output_glm.rda")
load("output/sim_output_glm_bad.rda")

power_results_glm <- summarize_results(sim_output_glm, 
                                       sample_size, 
                                       iteractions = iterations) |>
  mutate(model = "glm")

tables_glm <- split_results_tables(power_results_glm)
tables_glm <- lapply(tables_glm, \(x) mutate(x, model = "glm"))

power_results_glm_bad <- summarize_results(sim_output_glm_bad, sample_size, iteractions = iterations) |>
  mutate(model = "glm_bad")

tables_glm_bad <- split_results_tables(power_results_glm_bad)
tables_glm_bad <- lapply(tables_glm_bad, \(x) mutate(x, model = "glm_bad"))

# Save results
# save(power_results_glm, file = "results/power_results_glm.rda")
# save(power_results_glm_bad, file = "results/power_results_glm_bad.rda")

# ======================================================================
# RANDOM FOREST
# ======================================================================
load("output/sim_output_rf.rda")
load("output/sim_output_rf_bad.rda")

power_results_rf <- summarize_results(sim_output_rf, sample_size, iteractions = iterations) |>
  mutate(model = "rf")

tables_rf <- split_results_tables(power_results_rf)
tables_rf <- lapply(tables_rf, \(x) mutate(x, model = "rf"))

power_results_rf_bad <- summarize_results(sim_output_rf_bad, sample_size, iteractions = iterations) |>
  mutate(model = "rf_bad")

tables_rf_bad <- split_results_tables(power_results_rf_bad)
tables_rf_bad <- lapply(tables_rf_bad, \(x) mutate(x, model = "rf_bad"))

# Save results
# save(power_results_rf, file = "results/power_results_rf.rda")
# save(power_results_rf_bad, file = "results/power_results_rf_bad.rda")

# ======================================================================
# XGBOOST
# ======================================================================
load("output/sim_output_xgboost.rda")
load("output/sim_output_xgboost_bad.rda")

power_results_xgboost <- summarize_results(sim_output_xgboost, sample_size, iteractions = iterations) |>
  mutate(model = "xgboost")

tables_xgboost <- split_results_tables(power_results_xgboost)
tables_xgboost <- lapply(tables_xgboost, \(x) mutate(x, model = "xgboost"))

power_results_xgboost_bad <- summarize_results(sim_output_xgboost_bad, sample_size, iteractions = iterations) |>
  mutate(model = "xgboost_bad")

tables_xgboost_bad <- split_results_tables(power_results_xgboost_bad)
tables_xgboost_bad <- lapply(tables_xgboost_bad, \(x) mutate(x, model = "xgboost_bad"))

# Save results
# save(power_results_xgboost, file = "results/power_results_xgboost.rda")
# save(power_results_xgboost_bad, file = "results/power_results_xgboost_bad.rda")

# ======================================================================
# SUPER LEARNER
# ======================================================================
load("output/sim_output_sl.rda")
load("output/sim_output_sl_bad.rda")

iterations <- 1000  # reduced due to computational cost

power_results_sl <- summarize_results(sim_output_sl, sample_size, iteractions = iterations) |>
  mutate(model = "sl")

tables_sl <- split_results_tables(power_results_sl)
tables_sl <- lapply(tables_sl, \(x) mutate(x, model = "sl"))

power_results_sl_bad <- summarize_results(sim_output_sl_bad, sample_size, iteractions = iterations) |>
  mutate(model = "sl_bad")

tables_sl_bad <- split_results_tables(power_results_sl_bad)
tables_sl_bad <- lapply(tables_sl_bad, \(x) mutate(x, model = "sl_bad"))

# Save results
# save(power_results_sl, file = "results/power_results_sl.rda")
# save(power_results_sl_bad, file = "results/power_results_sl_bad.rda")

# ======================================================================
# AGGREGATE ALL RESULTS
# ======================================================================
result <- rbind(
  power_results_glm,
  power_results_rf,
  power_results_xgboost,
  power_results_sl
)

#save(result, file = "results/power_results_all.rda")
