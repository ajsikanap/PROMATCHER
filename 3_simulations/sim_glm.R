#########################################################################
# Title: Simulation Study - Prognostic Score Paired Matching (GLM)
# Author: Ajsi Kanapari
# Description: 
# This script simulates paired randomization scenarios with GLM-based  prognostic scores under different calipers, effect sizes, and sample sizes. Outputs simulation results for power  and type I error assessment.
##########################################################################

# =============================================================
# 1. Setup Environment
# =============================================================

rm(list = ls())
set.seed(1)

# Load required data and functions
load("data/data_continuous_toy_example_N_10.rda") 

# Load prognostic models
load("models/glm_model.rda")
load("models/glm_model_bad.rda")

# Load simulation function
source("2_functions/pairing_and_matching_MC_function.R")


# =============================================================
# 2. Define Simulation Parameters
# =============================================================

# do not run
#iteractions <- 10000
#
# toy example
iteractions<-10
calipers <- c(0.01, 0.05, 0.10)

# Scenarios
scenarios <- list(
  list(name = "large_n", n = 300, delta = 0.2, caliper = calipers),
  list(name = "small_n", n = c(30, 40, 50), delta = 0.5, caliper = 0.05),
  list(name = "varying_effect", 
       params = data.frame(
         n = c(16, 20, 26, 34, 49),
         delta = c(0.9, 0.8, 0.7, 0.6, 0.5),
         caliper = 0.05))
)


# =============================================================
# 3. Define Function for running scenarios
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
# 4. Run Simulations
# =============================================================

# ---------- Scenario 1: N = 300, delta = 0.2 ----------
list_mod_continuous_outcome <- list(glm_model)
names(list_mod_continuous_outcome) <- c("glm_model")

sim_output_glm <- run_simulation(glm_model, 300, 0.2, iteractions, calipers, "glm_model")

list_mod_continuous_outcome <- list(glm_model_bad)
names(list_mod_continuous_outcome) <- c("glm_model")

sim_output_glm_bad <- run_simulation(glm_model_bad, 300, 0.2, iteractions, calipers, "glm_model")

save(sim_output_glm, file = "output/sim_output_glm_N_300.rda")
save(sim_output_glm_bad, file = "output/sim_output_glm_bad_N_300.rda")


# ---------- Scenario 2: N = 30, 40, 50; delta = 0.5; caliper = 0.05 ----------
list_mod_continuous_outcome <- list(glm_model)
names(list_mod_continuous_outcome) <- c("glm_model")

sim_output_glm_ss50 <- lapply(scenarios[[2]]$n, function(n) {
  run_simulation(glm_model, n, scenarios[[2]]$delta, iteractions, scenarios[[2]]$caliper, "glm_model")
})

list_mod_continuous_outcome <- list(glm_model_bad)
names(list_mod_continuous_outcome) <- c("glm_model")

sim_output_glm_bad_ss50 <- lapply(scenarios[[2]]$n, function(n) {
  run_simulation(glm_model_bad, n, scenarios[[2]]$delta, iteractions, scenarios[[2]]$caliper, "glm_model")
})

save(sim_output_glm_ss50, file = "output/sim_output_glm_N_50.rda")
save(sim_output_glm_bad_ss50, file = "output/sim_output_glm_bad_N_50.rda")


# ---------- Scenario 3: Varying N and Effect Size ----------
list_mod_continuous_outcome <- list(glm_model)
names(list_mod_continuous_outcome) <- c("glm_model")

sim_outputs_varying <- list()

for (i in seq_len(nrow(scenarios[[3]]$params))) {
  pars <- scenarios[[3]]$params[i, ]
  sim_outputs_varying[[paste0("n", pars$n)]] <- run_simulation(
    glm_model, pars$n, pars$delta, iteractions, pars$caliper, "glm_model"
  )
}

# Save outputs
for (name in names(sim_outputs_varying)) {
  save(sim_outputs_varying[[name]], file = paste0("output/sim_output_glm_", name, ".rda"))
}