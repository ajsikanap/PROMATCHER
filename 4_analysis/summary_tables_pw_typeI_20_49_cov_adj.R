
# table 4 with different sizes
library(flextable)

#sim_output_glm
load("output/sim_output_glm_n49.rda")
load("output/sim_output_glm_n20.rda")
load("output/sim_output_glm_n34.rda")
load("output/sim_output_glm_n26.rda")
load("output/sim_output_glm_n16.rda")
load("output/sim_output_xgboost_n49.rda")
load("output/sim_output_xgboost_n20.rda")
load("output/sim_output_xgboost_n34.rda")
load("output/sim_output_xgboost_n26.rda")
load("output/sim_output_xgboost_n16.rda")


# carica funzione
source("2_functions/continuous_function_paired_glm_adj.R")
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
    power_glm_adj = power_glm_adj,
    sd_glm_adj = sd_glm_adj
  ) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal"),
    smd_matched_cov_mean = round(smd_matched_cov_mean,4)
  ) |>
  dplyr::select(paired, caliper, method, delta,
                power_ttest,mcse_power_ttest, sd_ttest,
                power_glm_adj,mcse_power_glm_adj, sd_glm_adj,
                power_lmer,mcse_power_lmer, sd_lmer,
                model, initial_size_mean, smd_matched_cov_mean)



# Table 1 Operating chr---------------
tbl3_final<- make_tbl_caliper_table(results_all, 0.05) |>
  mutate(analysis=ifelse(analysis=="lmer","trial_cov_adj", analysis),
    analysis=factor(analysis, levels = c("ttest","glm_adj","trial_cov_adj")),
    equal_sd_alt=round(equal_sd_alt,4),
    paired_sd_alt=round(paired_sd_alt,4)) |>
  arrange(initial_size_mean, model, analysis) |>
  relocate(initial_size_mean,caliper,model, analysis,
           equal_power_alt, equal_power_null, equal_sd_alt, equal_smd,
           paired_power_alt, paired_power_null, paired_sd_alt, paired_smd
           )



tbl3_final|>
  flextable::flextable() |> flextable::save_as_docx( path = "results/Table_S4_16_49_cov_adj.docx")


tbl3_final<- make_tbl_caliper_figure(results_all, 0.05) |>
  mutate(analysis=ifelse(analysis=="lmer","trial_cov_adj", analysis),
         analysis=factor(analysis, levels = c("ttest","glm_adj","trial_cov_adj")),
         equal_sd_alt=round(equal_sd_alt,4),
         paired_sd_alt=round(paired_sd_alt,4)) |>
  arrange(initial_size_mean, model, analysis) |>
  relocate(initial_size_mean,caliper,model, analysis,
           equal_power_alt, equal_power_null, equal_sd_alt, equal_smd,
           paired_power_alt, paired_power_null, paired_sd_alt, paired_smd
  )

save(tbl3_final, file="output/tbl_caliper005_49_20_adjusted.rda")

