###############################################################
# Title: Simulation Study - SEQUENTIAL enrollment randomization
# Author: Ajsi Kanapari
# Description:
# Sequential arrival from a fixed pool, greedy nearest-neighbour matching
# with caliper on the prognostic score, randomization AFTER pairing.
# Within-pair sign-flip permutation test. GLM and XGBoost scores.
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

# Load sequential simulation function
source("2_functions/PROMATCHER_SEQUENTIAL_MC_function.R")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
caliper_val <- c(0.01, 0.05, 0.10)
N_pool      <- 200

# n target (pairs) / effect-size pairs
scenarios <- data.frame(
  n     = c(16, 20, 26, 34, 49),
  delta = c(0.9, 0.8, 0.7, 0.6, 0.5)
)

models     <- c("glm_model", "xgboost_model")
model_objs <- list(glm_model = glm_model, xgboost_model = xgboost_model)

# =============================================================
#   3. Simulation Wrapper Function
# =============================================================
run_sequential <- function(n_target, delta, iter, caliper, N_pool, sd_ps_hist, label) {
  message(sprintf("Sequential %s | n_target = %d | delta = %.2f | caliper = %s",
                  label, n_target, delta, paste(caliper, collapse = ", ")))
  my_simulation_sequential_function(
    delta_effect     = delta,
    iter             = iter,
    method           = label,
    caliper_assigned = caliper,
    N_pool           = N_pool,
    sd_ps_hist       = sd_ps_hist,
    n_target         = n_target
  )
}

# =============================================================
#   4. Run Simulations
# =============================================================
for (lab in models) {
  short <- sub("_model$", "", lab)
  list_mod_continuous_outcome <- list(model_objs[[lab]])
  names(list_mod_continuous_outcome) <- lab
  sd_ps_hist_value <- sd(predict(model_objs[[lab]], type = "raw"))
  
  for (i in seq_len(nrow(scenarios))) {
    out <- run_sequential(scenarios$n[i], scenarios$delta[i], iteractions,
                          caliper_val, N_pool, sd_ps_hist_value, lab)
    obj <- paste0("sim_seq_", short, "_n", scenarios$n[i])
    assign(obj, out)
    save(list = obj, file = file.path("output", paste0(obj, ".rda")))
  }
}

cat("All done.\n")

# --- Quick summary check (esempio) ---
# summarize_results_sequential(sim_seq_xgboost_n20, iteractions = iteractions)