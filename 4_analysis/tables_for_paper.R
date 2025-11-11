# ======================================================================
# Script: summarize_results_complete.R
# Purpose: Summarize model performance, operating characteristics, and
#          multiple sample size simulations for paper results.
# ======================================================================

library(dplyr)
library(tidyr)
library(flextable)
library(knitr)

# --- Load summary functions -------------------------------------------
source("functions/summary_results_function.R")
source("functions/continuous_function_paired.R")

# ======================================================================
# LOAD MAIN RESULTS AND MODELS
# ======================================================================

# --- Load trained models ----------------------------------------------
load("models/glm_model.rda")
load("models/glm_model_bad.rda")
load("models/rf_model.rda")
load("models/rf_model_bad.rda")
load("models/xgboost_model.rda")
load("models/xgboost_model_bad.rda")
load("models/sl_model.rda")
load("models/sl_model_bad.rda")

load("output/sl_list.rda")
load("output/sl_list_bad.rda")

# ======================================================================
# MODEL PERFORMANCE TABLE
# ======================================================================

results <- data.frame(
  Model = c("GLM", "GLM (bad)", "RF", "RF (bad)",
            "XGBoost", "XGBoost (bad)", "SuperLearner", "SuperLearner (bad)"),
  RMSE = c(69.47, 106.09, 69.39, 107.73, 62.77, 105.27, 65.39, 106.12),
  Rsquared = c(0.822, 0.586, 0.832, 0.556, 0.851, 0.576, NA, NA),
  MAE = c(54.29, 81.64, 51.56, 82.49, 49.26, 80.82, NA, NA)
)

# results |> flextable() |> save_as_docx(path = "results/table_model_performance.docx")

# ======================================================================
# GOOD MODELS – POWER AND TYPE I ERROR
# ======================================================================

load("results/power_results_glm.rda")
load("results/power_results_rf.rda")
load("results/power_results_xgboost.rda")
load("results/power_results_sl.rda")

result <- rbind(
  power_results_glm,
  power_results_rf,
  power_results_xgboost,
  power_results_sl
)

results_all <- result |>
  rename(power_glmadj = power_glm_adj, sd_glmadj = sd_glm_adj) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  select(paired, caliper, method, delta, power_ttest, sd_ttest,
         power_glmadj, sd_glmadj, power_lmer, sd_lmer,
         model, initial_size_mean)

tbl_results_good <- bind_rows(
  make_tbl_caliper(results_all, 0.01),
  make_tbl_caliper(results_all, 0.05),
  make_tbl_caliper(results_all, 0.1)
)

#save(tbl_results_good, file = "results/tbl_results_good.rda")


# ======================================================================
# BAD MODELS – POWER AND TYPE I ERROR
# ======================================================================

load("results/power_results_glm_bad.rda")
load("results/power_results_rf_bad.rda")
load("results/power_results_xgboost_bad.rda")
load("results/power_results_sl_bad.rda")

result_all_bad <- rbind(
  power_results_glm_bad,
  power_results_rf_bad,
  power_results_xgboost_bad,
  power_results_sl_bad
)

results_all_bad <- result_all_bad |>
  rename(power_glmadj = power_glm_adj, sd_glmadj = sd_glm_adj) |>
  mutate(
    model = sub("_bad$", "", model),
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  select(paired, caliper, method, delta, power_ttest, sd_ttest,
         power_glmadj, sd_glmadj, power_lmer, sd_lmer,
         model, initial_size_mean)

tbl_results_bad <- bind_rows(
  make_tbl_caliper(results_all_bad, 0.01),
  make_tbl_caliper(results_all_bad, 0.05),
  make_tbl_caliper(results_all_bad, 0.1)
)

#save(tbl_results_bad, file = "results/tbl_results_bad.rda")


# ======================================================================
# MULTIPLE SAMPLE SIZE SCENARIOS (TABLE 3)
# ======================================================================

# --- Load simulation outputs for GLM and XGBoost across sizes ----------
load("output/sim_output_glm.rda")
load("output/sim_output_glm_n49.rda")
load("output/sim_output_glm_n34.rda")
load("output/sim_output_glm_n26.rda")
load("output/sim_output_glm_n20.rda")
load("output/sim_output_glm_n16.rda")

load("output/sim_output_xgboost.rda")
load("output/sim_output_xgboost_n49.rda")
load("output/sim_output_xgboost_n34.rda")
load("output/sim_output_xgboost_n26.rda")
load("output/sim_output_xgboost_n20.rda")
load("output/sim_output_xgboost_n16.rda")

iterations <- 10000

# --- Summarize results -------------------------------------------------
sizes <- c(300, 49, 34, 26, 20, 16)
models <- c("glm", "xgboost")

get_results <- function(model, size) {
  obj_name <- paste0("sim_output_", model, ifelse(size == 300, "", paste0("_n", size)))
  sim_data <- get(obj_name)
  summarize_results(sim_data, size, iteractions = iterations) |>
    mutate(model = model)
}

results_all_sizes <- bind_rows(
  lapply(models, function(m) bind_rows(lapply(sizes, function(s) get_results(m, s))))
)

results_all_sizes <- results_all_sizes |>
  filter(caliper == 0.05) |>
  rename(power_glmadj = power_glm_adj, sd_glmadj = sd_glm_adj) |>
  mutate(model = paste(model, "_model", sep = ""),
         paired = ifelse(method %in% model, "paired", "equal")) |>
  select(paired, caliper, method, delta, power_ttest, sd_ttest,
         power_glmadj, sd_glmadj, power_lmer, sd_lmer,
         model, initial_size_mean, smd_matched_cov_mean)

# --- Build Table -------------------------------------------------------
tbl_caliper005_1 <- results_all %>%
  filter(caliper == 0.05) %>%
  pivot_longer(
    cols = c(power_ttest, power_glmadj, power_lmer,
             sd_ttest, sd_glmadj, sd_lmer),
    names_to = c(".value", "analysis"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(delta = ifelse(delta == 0, 0, 1)) %>%
  distinct(caliper, initial_size_mean, model, analysis, paired, delta, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = delta,
    values_from = c(power, sd, smd_matched_cov_mean),
    names_glue = "{.value}_{delta}",
    values_fill = NA
  ) %>%
  pivot_wider(
    names_from = paired,
    values_from = c(power_0, power_1, sd_1, smd_matched_cov_mean_1),
    names_glue = "{paired}_{.value}"
  ) %>%
  select(
    caliper, initial_size_mean, model, analysis,
    equal_power_1, equal_power_0, equal_sd_1, equal_smd_matched_cov_mean_1,
    paired_power_1, paired_power_0, paired_sd_1, paired_smd_matched_cov_mean_1
  ) %>%
  arrange(caliper, model, initial_size_mean)

# --- Split equal and paired sections ----------------------------------
tbl_equal <- tbl_caliper005_1 %>%
  select(caliper, initial_size_mean, model, analysis,
         equal_power_1, equal_power_0, equal_sd_1, equal_smd_matched_cov_mean_1) %>%
  drop_na(equal_smd_matched_cov_mean_1)

tbl_paired <- tbl_caliper005_1 %>%
  select(caliper, initial_size_mean, model, analysis,
         paired_power_1, paired_power_0, paired_sd_1, paired_smd_matched_cov_mean_1) %>%
  drop_na(paired_smd_matched_cov_mean_1)

# --- Merge and finalize ----------------------------------------------
tbl_final <- merge(
  tbl_equal, tbl_paired,
  by = c("caliper", "initial_size_mean", "model", "analysis")
) %>%
  mutate(across(where(is.numeric), round, 4)) %>%
  filter(initial_size_mean != 300) %>%
  select(-caliper) %>%
  mutate(
    analysis = factor(analysis, levels = c("ttest", "glmadj", "lmer"))
  ) %>%
  arrange(initial_size_mean, model, analysis)




