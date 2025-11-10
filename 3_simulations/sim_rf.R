###############################################################
# Title: Simulation Study - Prognostic Score Paired Matching (Random Forest)
# Author: Ajsi Kanapari
# Description:
#This script simulates paired randomization scenarios using Random Forest-based prognostic scores under different calipers, effect sizes, and sample sizes. Outputs include simulation results for power and type I error assessment.
###############################################################

# =============================================================
#   1. Setup Environment
# =============================================================
  
rm(list = ls())
set.seed(1)

#Load data and functions
load("data/data_continuous_10000_rcts.rda")
source("2_functions/pairing_and_matching_MC_function.R")

#Load prognostic models
load("models/rf_model.rda")
load("models/rf_model_bad.rda")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
  
# do not run
#iteractions <- 10000
#
# toy example
iteractions<-10
calipers <- c(0.01, 0.05, 0.10)
sample_size_large <- 300
sample_sizes_small <- c(30, 40, 50)

# =============================================================
#   3. Define Simulation Wrapper Function
# =============================================================
  
  run_simulation <- function(model, n, delta, iter, caliper, label) {
    message(sprintf("Running %s | N = %d | delta = %.2f | caliper = %s",
                    label, n, delta, paste(caliper, collapse = ", ")))
    my_simulation_continuous_function(
      delta = delta,
      n = n,
      iter = iter,
      method = label,
      caliper_assigned = caliper
    )
  }

# =============================================================
#   4. Run Simulations
# =============================================================
#   ---------- Scenario 1: N = 300, delta = 0.2 ----------
  
  list_mod_continuous_outcome <- list(rf_model)
names(list_mod_continuous_outcome) <- c("rf_model")

sim_output_rf <- run_simulation(rf_model, sample_size_large, 0.2, iteractions, calipers, "rf_model")

list_mod_continuous_outcome <- list(rf_model_bad)
names(list_mod_continuous_outcome) <- c("rf_model")

sim_output_rf_bad <- run_simulation(rf_model_bad, sample_size_large, 0.2, iteractions, calipers, "rf_model")

#save
save(sim_output_rf, file = "output/sim_output_rf_N_300.rda")
save(sim_output_rf_bad, file = "output/sim_output_rf_bad_N_300.rda")

#---------- Scenario 2: N = 30, 40, 50; delta = 0.5; caliper = 0.05 ----------
  
list_mod_continuous_outcome <- list(rf_model)
names(list_mod_continuous_outcome) <- c("rf_model")

sim_output_rf_ss50 <- lapply(sample_sizes_small, function(n) {
  run_simulation(rf_model, n, 0.5, iteractions, 0.05, "rf_model")
})

list_mod_continuous_outcome <- list(rf_model_bad)
names(list_mod_continuous_outcome) <- c("rf_model")

sim_output_rf_bad_ss50 <- lapply(sample_sizes_small, function(n) {
  run_simulation(rf_model_bad, n, 0.5, iteractions, 0.05, "rf_model")
})

# save
save(sim_output_rf_ss50, file = "output/sim_output_rf_N_50.rda")
save(sim_output_rf_bad_ss50, file = "output/sim_output_rf_bad_N_50.rda")