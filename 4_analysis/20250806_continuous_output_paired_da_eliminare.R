# Load necessary libraries
library(dplyr)
library(tidyr)
library(purrr)

# THE CODE CONTAINS ALL OUTPUTS FOR ALL SCENARIOS IN continuous OUTCOME.

# 1- Load models

load("data/list_mod_continuous_20250505.rda")
names(list_mod_continuous_outcome) <- c("glm_model", "rf_model", "xgboost_model", "sl_model")
list_mod_continuous_outcome


# 2- Load function
source("continuous_function_paired.R")

# 3- load data RCT
load("data_simulations/data_continuous.rda")


# 4-  run scenarios



## 1. XGBOOST -----
power_levels <- list()
sample_size_to_try <- c(300)
for (i in 1:length(sample_size_to_try)) {
  power_levels[[i]] <- my_simulation_continuous_function(
    delta = 0.2,
    N = 2000,
    n = sample_size_to_try[i],
    iter = 10000,
    method = "xgboost_model",
    caliper_assigned = c(0.01, 0.05, 0.10)
  )
}



all_output_xg <- power_levels



power_levels_temp <- list()
for (i in 1:length(sample_size_to_try)) {
  power_levels_temp[[i]] <- power_levels[[i]]$final_output |>
    as.data.frame() |>
    group_by(caliper, method, delta) |>
    summarize(
      caliper_dist = mean(caliper_distance, na.rm = T),
      smd_matched_ps_mean = mean(smd_matched_ps, na.rm = T),
      smd_matched_cov_mean = mean(smd_matched_cov, na.rm = T),
      initial_size_mean = mean(initial_size),
      reduced_size_mean = mean(reduced_size),

      # ttest
      power_ttest = mean(p_sign_ttest),
      coef_ttest = mean(coef_ttest),
      sd_ttest = mean(sd_ttest),

      # glm adj
      power_glm_adj = mean(p_sign_glm_adj),
      coef_glm_adj = mean(coef_glm_adj),
      sd_glm_adj = mean(sd_glm_adj),

      # lmer subclass
      power_lmer = mean(p_sign_lmer, na.rm = T), # glmer
      coef_lmer = mean(coef_lmer, na.rm = T),
      sd_lmer = mean(sd_lmer, na.rm = T)
    )
}


power_levels_with_size <- map2(power_levels_temp, sample_size_to_try, ~ mutate(.x, sample_size = .y))
power_results_combined <- bind_rows(power_levels_with_size)
power_results_xg <- power_results_combined |>
  mutate(model = "xgboost")




## 3 rf ---------------
power_levels <- list()
sample_size_to_try <- c(300)
for (i in 1:length(sample_size_to_try)) {
  power_levels[[i]] <- my_simulation_continuous_function(
    delta = 0.2,
    N = 2000,
    n = sample_size_to_try[i],
    iter = 10000,
    method = "rf_model",
    caliper_assigned = c(0.01, 0.05, 0.10)
  )
}



all_output_rf <- power_levels



power_levels_temp <- list()
for (i in 1:length(sample_size_to_try)) {
  power_levels_temp[[i]] <- power_levels[[i]]$final_output |>
    as.data.frame() |>
    group_by(caliper, method, delta) |>
    summarize(
      caliper_dist = mean(caliper_distance, na.rm = T),
      smd_matched_ps_mean = mean(smd_matched_ps, na.rm = T),
      smd_matched_cov_mean = mean(smd_matched_cov, na.rm = T),
      initial_size_mean = mean(initial_size),
      reduced_size_mean = mean(reduced_size),

      # ttest
      power_ttest = mean(p_sign_ttest),
      coef_ttest = mean(coef_ttest),
      sd_ttest = mean(sd_ttest),

      # glm adj
      power_glm_adj = mean(p_sign_glm_adj),
      coef_glm_adj = mean(coef_glm_adj),
      sd_glm_adj = mean(sd_glm_adj),

      # lmer subclass
      power_lmer = mean(p_sign_lmer, na.rm = T), # glmer
      coef_lmer = mean(coef_lmer, na.rm = T),
      sd_lmer = mean(sd_lmer, na.rm = T)
    )
}


power_levels_with_size <- map2(power_levels_temp, sample_size_to_try, ~ mutate(.x, sample_size = .y))
power_results_combined <- bind_rows(power_levels_with_size)
power_results_rf <- power_results_combined |>
  mutate(model = "rf")


## 4. glm ---------------
power_levels <- list()
sample_size_to_try <- c(300)
for (i in 1:length(sample_size_to_try)) {
  power_levels[[i]] <- my_simulation_continuous_function(
    delta = 0.2,
    N = 2000,
    n = sample_size_to_try[i],
    iter = 10000,
    method = "glm_model",
    caliper_assigned = c(0.01, 0.05, 0.10)
  )
}



all_output_glm <- power_levels



power_levels_temp <- list()
power_levels_temp <- list()
for (i in 1:length(sample_size_to_try)) {
  power_levels_temp[[i]] <- power_levels[[i]]$final_output |>
    as.data.frame() |>
    group_by(caliper, method, delta) |>
    summarize(
      caliper_dist = mean(caliper_distance, na.rm = T),
      smd_matched_ps_mean = mean(smd_matched_ps, na.rm = T),
      smd_matched_cov_mean = mean(smd_matched_cov, na.rm = T),
      initial_size_mean = mean(initial_size),
      reduced_size_mean = mean(reduced_size),

      # ttest
      power_ttest = mean(p_sign_ttest),
      coef_ttest = mean(coef_ttest),
      sd_ttest = mean(sd_ttest),

      # glm adj
      power_glm_adj = mean(p_sign_glm_adj),
      coef_glm_adj = mean(coef_glm_adj),
      sd_glm_adj = mean(sd_glm_adj),

      # lmer subclass
      power_lmer = mean(p_sign_lmer, na.rm = T), # glmer
      coef_lmer = mean(coef_lmer, na.rm = T),
      sd_lmer = mean(sd_lmer, na.rm = T)
    )
}

power_levels_with_size <- map2(power_levels_temp, sample_size_to_try, ~ mutate(.x, sample_size = .y))
power_results_combined <- bind_rows(power_levels_with_size)
power_results_glm <- power_results_combined |>
  mutate(model = "glm")


# sl
power_levels <- list()
sample_size_to_try <- c(300)
for (i in 1:length(sample_size_to_try)) {
  power_levels[[i]] <- my_simulation_continuous_function(
    delta = 0.2,
    N = 2000,
    n = sample_size_to_try[i],
    iter = 1000,
    method = "sl_model",
    caliper_assigned = c(0.01, 0.05, 0.10)
  )
}



all_output_sl <- power_levels



power_levels_temp <- list()
for (i in 1:length(sample_size_to_try)) {
  power_levels_temp[[i]] <- power_levels[[i]]$final_output |>
    as.data.frame() |>
    group_by(caliper, method, delta) |>
    summarize(
      caliper_dist = mean(caliper_distance, na.rm = T),
      smd_matched_ps_mean = mean(smd_matched_ps, na.rm = T),
      smd_matched_cov_mean = mean(smd_matched_cov, na.rm = T),
      initial_size_mean = mean(initial_size),
      reduced_size_mean = mean(reduced_size),

      # ttest
      power_ttest = mean(p_sign_ttest),
      coef_ttest = mean(coef_ttest),
      sd_ttest = mean(sd_ttest),

      # glm adj
      power_glm_adj = mean(p_sign_glm_adj),
      coef_glm_adj = mean(coef_glm_adj),
      sd_glm_adj = mean(sd_glm_adj),

      # lmer subclass
      power_lmer = mean(p_sign_lmer, na.rm = T), # glmer
      coef_lmer = mean(coef_lmer, na.rm = T),
      sd_lmer = mean(sd_lmer, na.rm = T)
    )
}


power_levels_with_size <- map2(power_levels_temp, sample_size_to_try, ~ mutate(.x, sample_size = .y))
power_results_combined <- bind_rows(power_levels_with_size)
power_results_sl <- power_results_combined |>
  mutate(model = "sl")

power_results_sl

save(power_results_sl, file = "C:/Users/AjsiKanapari/Unit of Biostatistics Epidemiology and Public Health/LACED - Randomizzation by PS - 1 - Randomizzation by PS - 1/script5_sct_paper/output/continuous_output/sl_continuous_1000.rda")

writexl::write_xlsx(power_results_sl, path = "C:/Users/AjsiKanapari/Unit of Biostatistics Epidemiology and Public Health/LACED - Randomizzation by PS - 1 - Randomizzation by PS - 1/script5_sct_paper/output/continuous_output/sl_continuous_1000.xlsx")



# final bind

result <- rbind(
  power_results_xg,
  power_results_rf,
  power_results_glm
)


save(result, file = "output2/output_continuous/results_complete.rda")
# load(file = "output2/output_continuous/results_complete.rda")

library(readxl)
library(readr)
write_excel_csv(result, file = "output2/output_continuous/results_complete.csv")
