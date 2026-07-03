###############################################################
# Title: Simulation Study - Covariate-adjusted analysis (sensitivity)
# Author: Ajsi Kanapari
# Description:
# Paired randomization with prognostic score matching, sensitivity
# analysis where the third analysis (slot "lmer") is a GLM adjusted
# for all covariates in BOTH arms. Varying sample size and effect size.
###############################################################

# =============================================================
#   1. Setup Environment
# =============================================================
rm(list = ls())
set.seed(1)
library(caret)

# --- do not run (full simulation) ---
# iteractions <- 10000
# load("data/data_continuous_10000_rcts.rda")
# --- toy example ---
iteractions <- 10
load("data/data_continuous_toy_example_N_10.rda")
data_continuous<-data_toy
# Load prognostic models
load("models/glm_model.rda")
load("models/xgboost_model.rda")

# Load simulation function (covariate-adjusted / sensitivity variant)
source("2_functions/PROMATCHER_COV_ADJ_MC_function_sensitivity.R")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
calipers <- 0.05

# n paired / effect-size pairs (as in the original 20/49 + 16/26/34 blocks)
scenarios <- data.frame(
  n     = c(16, 20, 26, 34, 49),
  delta = c(0.9, 0.8, 0.7, 0.6, 0.5)
)

models     <- c("glm_model", "xgboost_model")
model_objs <- list(glm_model = glm_model, xgboost_model = xgboost_model)

# =============================================================
#   3. Simulation Wrapper Function
# =============================================================
run_simulation <- function(model, n, delta, iter, caliper, label) {
  message(sprintf("Running %s | n = %d | delta = %.2f | caliper = %s",
                  label, n, delta, paste(caliper, collapse = ", ")))
  my_simulation_continuous_function(
    delta            = delta,
    n                = n,
    iter             = iter,
    method           = label,
    caliper_assigned = caliper
  )
}

# =============================================================
#   4. Run Simulations
# =============================================================
for (lab in models) {
  short <- sub("_model$", "", lab)
  list_mod_continuous_outcome <- list(model_objs[[lab]])
  names(list_mod_continuous_outcome) <- lab
  
  for (i in seq_len(nrow(scenarios))) {
    out <- run_simulation(model_objs[[lab]], scenarios$n[i], scenarios$delta[i],
                          iteractions, calipers, lab)
    obj <- paste0("sim_output_", short, "_n", scenarios$n[i])
    assign(obj, out)
    save(list = obj, file = file.path("output", paste0(obj, ".rda")))
  }
}

