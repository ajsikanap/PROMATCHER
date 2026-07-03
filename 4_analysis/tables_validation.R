

load("output/sim_output_xgboost_validation_good.rda")
load("output/sim_output_glm_validation_good.rda")
load("output/sim_output_xgboost_validation_bad.rda")
load("output/sim_output_glm_validation_bad.rda")
source("2_functions/continuous_function_paired_lmer_validation.R")

# 1) aggrega l'output replica-per-replica in summary per (caliper, method, delta)
sim_xgboost_val_49 <- summarize_results(sim_output_xgboost, sample_size_to_try, iteractions = iteractions) |>
  mutate(model="xgboost_good",
        r2_hist=0.851)
sim_glm_val_49 <- summarize_results(sim_output_glm, sample_size_to_try, iteractions = iteractions) |>
  mutate(model="glm_good",
         r2_hist=0.822)

sim_xgboost_val_49_bad <- summarize_results(sim_output_xgboost_bad, sample_size_to_try, iteractions = iteractions) |>
  mutate(model="xgboost_good_bad",
         r2_hist=0.576)
sim_glm_val_49_bad <- summarize_results(sim_output_glm_bad, sample_size_to_try, iteractions = iteractions) |>
  mutate(model="glm_good_bad",
         r2_hist=0.586)

# 2) costruisci la tabella di validazione (le due gambe), ancorata al paired t-test
val_xgboost <- summarize_validation(sim_xgboost_val_49, alpha = 0.05) |>   mutate(model="good")
val_glm <- summarize_validation(sim_glm_val_49, alpha = 0.05) |>   mutate(model="good")

val_xgboost_bad <- summarize_validation(sim_xgboost_val_49_bad, alpha = 0.05) |>   mutate(model="bad")
val_glm_bad <- summarize_validation(sim_glm_val_49_bad, alpha = 0.05) |>    mutate(model="bad")

main_tab<-rbind(
  sim_glm_val_49, sim_xgboost_val_49,
  sim_glm_val_49_bad, sim_xgboost_val_49_bad
) |>
  mutate(paired=ifelse(method=="equal","equal","paired"))

val_tab<-rbind(
  val_xgboost, val_glm,
  val_xgboost_bad, val_glm_bad
) |> arrange( model, caliper, method ) |>
  mutate(r2_trial=round(r2_trial,4),
         rho_wp=round(rho_wp, 4),
         rho_wp_se=round(rho_wp_se,4),
         rho_wp_vardecomp=round(rho_wp_vardecomp,4),
         rho_s=round(rho_s, 4),
         lambda_emp=round(lambda_emp,4),
         lambda_decomp=round(lambda_decomp, 4),
         power_pred=round(power_pred,4),
         power_emp=round(power_emp,4),
         power_gap=round(power_gap,4)
         )

val_tab |>
  flextable::flextable() |> flextable::save_as_docx( path = "results/Table_S11_validation.docx")

source("2_functions/summary_results_function.R")

results_all <- main_tab |>
  rename(
    power_glm_adj = power_glm_adj,
    sd_glm_adj = sd_glm_adj
  ) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    #paired = ifelse(method %in% model, "paired", "equal"),
    smd_matched_cov_mean = round(smd_matched_cov_mean,4)
  ) |>
  dplyr::select(paired, caliper, method, delta,
                power_ttest,mcse_power_ttest, sd_ttest,
                power_glm_adj,mcse_power_glm_adj, sd_glm_adj,
                power_lmer,mcse_power_lmer, sd_lmer,

                model, initial_size_mean, smd_matched_cov_mean)


# Table 1 Operating chr---------------
tbl10<-rbind(make_tbl_caliper_table2(results_all, 0.01),
             make_tbl_caliper_table2(results_all, 0.05),
             make_tbl_caliper_table2(results_all, 0.10))


tbl10_final<- tbl10 |>
  mutate(analysis=ifelse(analysis=="lmer","trial_cov_adj", analysis),
         analysis=factor(analysis, levels = c("ttest","glm_adj","trial_cov_adj")),
         equal_sd_alt=round(equal_sd_alt,4),
         paired_sd_alt=round(paired_sd_alt,4)) |>
  arrange(initial_size_mean, model, analysis) |>
  relocate(initial_size_mean,caliper,model, analysis,
           equal_power_alt, equal_power_null, equal_sd_alt,
           paired_power_alt, paired_power_null, paired_sd_alt
  ) |>
  arrange(caliper, model,analysis)



tbl10_final|>
  flextable::flextable() |> flextable::save_as_docx( path = "results/Table_S10_validation.docx")

