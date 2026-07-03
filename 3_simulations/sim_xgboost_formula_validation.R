###############################################################
# Title: Simulation Study - Power FORMULA VALIDATION
# Author: Ajsi Kanapari
# Description:
# Validation of the analytical power formula: logs oracle quantities
# (Var(S), rho_wp, R2_trial, rho_s, lambda) alongside empirical power,
# for well- and mis-specified GLM/XGBoost scores. n = 49, delta = 0.5.
# Requires the score-known dataset (covariates V11-V15 kept) to fill the
# S-block; otherwise those quantities stay NA (S is never fabricated).
###############################################################

# =============================================================
#   1. Setup Environment
# =============================================================
rm(list = ls())
set.seed(1)
library(caret)

# --- do not run (full simulation) ---
# iteractions <- 10000
# load("data/data_continuous_10000_rcts_score_known.rda")
# --- toy example ---
# NB: per validare il blocco-S serve il dataset con score noto (V11-V15).
#     Con il toy standard il resto gira, ma le quantita' S restano NA.
iteractions <- 10
load("data/data_continuous_toy_example_N_10_score_known.rda")
#data_continuous2

# Load prognostic models (good + bad)
load("models/glm_model.rda")
load("models/glm_model_bad.rda")
load("models/xgboost_model.rda")
load("models/xgboost_model_bad.rda")

# Load simulation function (formula-validation variant)
source("2_functions/PROMATCHER_FORMULA_VALIDATION_MC_function.R")

# =============================================================
#   2. Define Simulation Parameters
# =============================================================
n_val    <- 49
delta_val <- 0.5
calipers <- c(0.01, 0.05, 0.10)

# label = etichetta metodo; spec = good/bad; r2_hist = R2 CV registry-side
val_scenarios <- data.frame(
  label   = c("xgboost_model", "xgboost_model", "glm_model", "glm_model"),
  spec    = c("good", "bad", "good", "bad"),
  r2_hist = c(0.851, 0.576, 0.822, 0.586),
  stringsAsFactors = FALSE
)

model_objs <- list(
  xgboost_good = xgboost_model, xgboost_bad = xgboost_model_bad,
  glm_good     = glm_model,     glm_bad     = glm_model_bad
)

# =============================================================
#   3. Simulation Wrapper Function
# =============================================================
run_validation <- function(model, n, delta, iter, caliper, label, r2_hist) {
  message(sprintf("Validation %s (r2_hist = %.3f) | n = %d | delta = %.2f",
                  label, r2_hist, n, delta))
  my_simulation_continuous_function(
    delta            = delta,
    n                = n,
    iter             = iter,
    method           = label,
    r2_hist          = r2_hist,
    caliper_assigned = caliper
  )
}

# =============================================================
#   4. Run Simulations
# =============================================================
for (i in seq_len(nrow(val_scenarios))) {
  lab   <- val_scenarios$label[i]
  spec  <- val_scenarios$spec[i]
  short <- sub("_model$", "", lab)
  obj_model <- model_objs[[paste0(short, "_", spec)]]
  
  list_mod_continuous_outcome <- list(obj_model)
  names(list_mod_continuous_outcome) <- lab   # etichetta sempre "..._model"
  
  set.seed(1)
  out <- run_validation(obj_model, n_val, delta_val, iteractions,
                        calipers, lab, val_scenarios$r2_hist[i])
  
  # nomi oggetto/file identici all'originale (usati da 07_e)
  obj <- paste0("sim_output_", short, if (spec == "bad") "_bad" else "")
  assign(obj, out)
  save(list = obj, file = file.path("output",
                                    paste0("sim_output_", short, "_validation_", spec, ".rda")))
}