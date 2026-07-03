###############################################################
# Title: Simulation Study - Prognostic-score STRATA randomization
# Author: Ajsi Kanapari
# Description:
# Paired randomization within deciles of the prognostic score (strata
# variant). Varying sample size and effect size, GLM and XGBoost scores.
###############################################################

# =============================================================
#   1. Setup Environment
# =============================================================
rm(list = ls())
set.seed(1)
library(caret)
library(lmerTest)

# --- do not run (full simulation) ---
# iteractions <- 10000
# load("data/data_continuous_10000_rcts.rda")
# --- toy example ---
iteractions <- 3
load("data/data_continuous_toy_example_N_10.rda")
data_continuous<-data_toy

# Load prognostic models
load("models/glm_model.rda")
load("models/xgboost_model.rda")

# Load simulation function (strata variant)
source("2_functions/PROMATCHER_STRATA_MC_function.R")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
calipers <- 0.05

# Deciles of the (registry) prognostic score used by the strata function.
ps_glm <- predict(glm_model,     type = "raw")
ps_xg  <- predict(xgboost_model, type = "raw")
quantiles_list <- list(
  glm_model     = quantile(ps_glm, probs = 0:10 / 10),
  xgboost_model = quantile(ps_xg,  probs = 0:10 / 10)   # FIX: prima usava i decili GLM
)
# save(quantiles_list, file = "data/quantiles_list.rda")

# n paired / effect-size pairs
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
# NB: output salvato in output2/ per non sovrascrivere i risultati cov_adj
#     (stessi nomi oggetto/file).
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