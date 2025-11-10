###############################################################
# Title: Simulation Study - Prognostic Score Paired Matching (Super Learner)
# Author: Ajsi Kanapari
# Description:
# This script simulates paired randomization scenarios using Super Learner-based prognostic scores under different calipers and sample sizes. Outputs include simulation results for power and type I error assessment.
###############################################################

# =============================================================
#   1. Setup Environment
# =============================================================
#   
 rm(list = ls())
set.seed(1)

#Load data and functions
load("data/data_continuous_toy_example_N_10.rda")
source("2_functions/pairing_and_matching_MC_function.R")

#Load prognostic models
load("models/sl_model.rda")
load("models/sl_model_bad.rda")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
  
  
# do not run
#iteractions <- 10000

 # toy example
iteractions<-10
sample_size_to_try <- 300
calipers <- c(0.01, 0.05, 0.10)

# =============================================================
#   3. Define Simulation Wrapper Function
# =============================================================
#   
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
#=============================================================
#  ---------- Scenario 1: Super Learner (well-specified model) ----------
  
  list_mod_continuous_outcome <- list(sl_model)
names(list_mod_continuous_outcome) <- c("sl_model")

sim_output_sl <- run_simulation(sl_model, sample_size_to_try, 0.2, iteractions, calipers, "sl_model")

# ---------- Scenario 2: Super Learner (mis-specified model) ----------
  
  list_mod_continuous_outcome <- list(sl_model_bad)
names(list_mod_continuous_outcome) <- c("sl_model")

sim_output_sl_bad <- run_simulation(sl_model_bad, sample_size_to_try, 0.2, iteractions, calipers, "sl_model")

# =============================================================
#   5. Save Outputs
# =============================================================
  
save(sim_output_sl, file = "output/sim_output_sl_N_300.rda")
save(sim_output_sl_bad, file = "output/sim_output_sl_bad_N_300.rda")