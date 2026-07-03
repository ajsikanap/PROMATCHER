# table 4 with different sizes
library(flextable)

# glm
load("output2/sim_output_glm_n49.rda")
load("output2/sim_output_glm_n20.rda")
load("output2/sim_output_glm_n34.rda")
load("output2/sim_output_glm_n26.rda")
load("output2/sim_output_glm_n16.rda")
load("output2/sim_output_xgboost_n49.rda")
load("output2/sim_output_xgboost_n20.rda")
load("output2/sim_output_xgboost_n34.rda")
load("output2/sim_output_xgboost_n26.rda")
load("output2/sim_output_xgboost_n16.rda")


# carica funzione
source("2_functions/continuous_function_strata.R")
source("2_functions/summary_results_function.R")

iteractions=10000
sample_size_to_try=49


# 1a) n49
sample_size_to_try=49
power_results_glm_49  <- summarize_results(sim_output_glm_n49, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost_49  <- summarize_results(sim_output_xgboost_n49, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")


# 2) n20
sample_size_to_try=20
power_results_glm_20  <- summarize_results(sim_output_glm_n20, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost_20  <- summarize_results(sim_output_xgboost_n20, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

# 3) n=300
sample_size_to_try=300
power_results_glm <- summarize_results(sim_output_glm, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost  <- summarize_results(sim_output_xgboost, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

# 4) 34
sample_size_to_try=34
power_results_glm_34  <- summarize_results(sim_output_glm_n34, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost_34 <- summarize_results(sim_output_xgboost_n34, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

# 5) 26

sample_size_to_try=26
power_results_glm_26  <- summarize_results(sim_output_glm_n26, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost_26  <- summarize_results(sim_output_xgboost_n26, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

# 6) 16
sample_size_to_try=16
power_results_glm_16  <- summarize_results(sim_output_glm_n16, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

power_results_xgboost_16  <- summarize_results(sim_output_xgboost_n16, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")



results_49_34_26_20_16<-rbind(
  power_results_glm_49,
  power_results_glm_34,
  power_results_glm_26,
  power_results_glm_20,
  power_results_glm_16,

  power_results_xgboost_49,
  power_results_xgboost_34,
  power_results_xgboost_26,
  power_results_xgboost_20,
  power_results_xgboost_16
)



results_49_34_26_20_16<-results_49_34_26_20_16 |> filter(caliper==0.05)


results_all <- results_49_34_26_20_16 |>
  rename(
    power_glmadj      = power_glm_adj,
    sd_glmadj         = sd_glm_adj,
    mcse_power_glmadj = mcse_power_glm_adj        # <— 1. rinomina anche la mcse (no underscore interno)
  ) |>
  mutate(
    model  = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  mutate(                                          # <— 2. incolla mcse dentro la power (come la funzione)
    power_ttest  = paste0(round(power_ttest, 4),  " (", round(mcse_power_ttest, 4),  ")"),
    power_glmadj = paste0(round(power_glmadj, 4), " (", round(mcse_power_glmadj, 4), ")"),
    power_lmer   = paste0(round(power_lmer, 4),   " (", round(mcse_power_lmer, 4),   ")")
  ) |>
  dplyr::select(                                   # <— 3. NON includere le mcse_* qui: così vengono droppate
    paired, caliper, method, delta,
    power_ttest, sd_ttest,
    power_glmadj, sd_glmadj,
    power_lmer, sd_lmer,
    model, initial_size_mean,
    sample_size, reduced_size_mean, smd_matched_cov_mean
  )

# ----- da qui in poi è il TUO codice, invariato -----
tbl_caliper005_1 <- results_all %>%
  filter(caliper == 0.05) %>%
  pivot_longer(
    cols = c(power_ttest, power_glmadj, power_lmer,
             sd_ttest, sd_glmadj, sd_lmer),
    names_to = c(".value", "analysis"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(delta = ifelse(delta == 0, 0, 1)) %>%
  distinct(caliper, sample_size, initial_size_mean, model, analysis, paired, delta, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = delta,
    values_from = c(power, sd),
    names_glue = "{.value}_{delta}",
    values_fill = NA
  ) %>%
  pivot_wider(
    names_from = paired,
    values_from = c(power_0, power_1, sd_1),
    names_glue = "{paired}_{.value}"
  ) %>%
  dplyr::select(
    caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
    equal_power_1, equal_power_0, equal_sd_1,
    paired_power_1, paired_power_0, paired_sd_1,
  ) %>%
  arrange(caliper, model, initial_size_mean)

tbl_equal <- tbl_caliper005_1 |>
  dplyr::select(caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
                equal_power_1, equal_power_0, equal_sd_1) |>
  distinct(analysis, model, sample_size, .keep_all = TRUE)

tbl_paired <- tbl_caliper005_1 |>
  dplyr::select(caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
                paired_power_1, paired_power_0, paired_sd_1) |>
  drop_na(paired_power_1)

tbl3_final <- right_join(
  tbl_equal, tbl_paired,
  by = c("caliper", "initial_size_mean", "sample_size", "reduced_size_mean", "model", "analysis")
)

tbl3_final <- tbl3_final |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  filter(sample_size != 300) |>
  select(-caliper) |>
  mutate(analysis = factor(analysis, levels = c("ttest", "glmadj", "lmer"))
                  ) |>
  arrange(sample_size, model, analysis) |>
  relocate(initial_size_mean,sample_size,model, analysis,
           equal_power_1, equal_power_0, equal_sd_1,
           paired_power_1, paired_power_0, paired_sd_1
  )



tbl3_final|>
  flextable::flextable() |> flextable::save_as_docx( path = "results/Table_S5_16_49_strata.docx")





results_all <- results_49_34_26_20_16 |>
  rename(
    power_glmadj      = power_glm_adj,
    sd_glmadj         = sd_glm_adj,
    mcse_power_glmadj = mcse_power_glm_adj        # <— 1. rinomina anche la mcse (no underscore interno)
  ) |>
  mutate(
    model  = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  dplyr::select(                                   # <— 3. NON includere le mcse_* qui: così vengono droppate
    paired, caliper, method, delta,
    power_ttest, sd_ttest,
    power_glmadj, sd_glmadj,
    power_lmer, sd_lmer,
    model, initial_size_mean,
    sample_size, reduced_size_mean, smd_matched_cov_mean
  )

tbl_caliper005_1 <- results_all %>%
  filter(caliper == 0.05) %>%
  pivot_longer(
    cols = c(power_ttest, power_glmadj, power_lmer,
             sd_ttest, sd_glmadj, sd_lmer),
    names_to = c(".value", "analysis"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  mutate(delta = ifelse(delta == 0, 0, 1)) %>%
  distinct(caliper, sample_size, initial_size_mean, model, analysis, paired, delta, .keep_all = TRUE) %>%
  pivot_wider(
    names_from = delta,
    values_from = c(power, sd),
    names_glue = "{.value}_{delta}",
    values_fill = NA
  ) %>%
  pivot_wider(
    names_from = paired,
    values_from = c(power_0, power_1, sd_1),
    names_glue = "{paired}_{.value}"
  ) %>%
  dplyr::select(
    caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
    equal_power_1, equal_power_0, equal_sd_1,
    paired_power_1, paired_power_0, paired_sd_1,
  ) %>%
  arrange(caliper, model, initial_size_mean)

tbl_equal <- tbl_caliper005_1 |>
  dplyr::select(caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
                equal_power_1, equal_power_0, equal_sd_1) |>
  distinct(analysis, model, sample_size, .keep_all = TRUE)

tbl_paired <- tbl_caliper005_1 |>
  dplyr::select(caliper, initial_size_mean, sample_size, reduced_size_mean, model, analysis,
                paired_power_1, paired_power_0, paired_sd_1) |>
  drop_na(paired_power_1)

tbl3_final <- right_join(
  tbl_equal, tbl_paired,
  by = c("caliper", "initial_size_mean", "sample_size", "reduced_size_mean", "model", "analysis")
)

tbl4_final <- tbl3_final |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  filter(sample_size != 300) |>
  select(-caliper) |>
  mutate(analysis = factor(analysis, levels = c("ttest", "glmadj", "lmer"))
  ) |>
  arrange(sample_size, model, analysis) |>
  relocate(initial_size_mean,sample_size,model, analysis,
           equal_power_1, equal_power_0, equal_sd_1,
           paired_power_1, paired_power_0, paired_sd_1
  )

save(tbl4_final, file="output/tbl_caliper005_49_20_strata.rda")
