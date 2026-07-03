
# Script che riassume i vari risultati di tutte le simulazioni
# good and bad, i risultati vengono poi salvati e riassunti in 06 per
# paper format
# create outputs specific for 1) power, type I error, se, 2) balance, smd 3)bias, mse, se bias
library(flextable)

# glm
load("output/sim_output_glm.rda")
load("output/sim_output_glm_bad.rda")

# carica funzione
source("2_functions/PROMATCHER_MC_function.R")
source("2_functions/summary_results_function.R")

iteractions=3
sample_size_to_try=300


# 1a) glm
power_results_glm  <- summarize_results(sim_output_glm, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")


# 2a glm bad
power_results_glm_bad <- summarize_results(sim_output_glm_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm_bad")

# rf
load("output/sim_output_rf.rda")
load("output/sim_output_rf_bad.rda")


# 1a) rf
power_results_rf  <- summarize_results(sim_output_rf, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "rf")

# 2a rf bad
power_results_rf_bad <- summarize_results(sim_output_rf_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "rf_bad")


# xgboost
# xgboost
load("output/sim_output_xgboost.rda")
load("output/sim_output_xgboost_bad.rda")

# 1a) xgboost
power_results_xgboost  <- summarize_results(sim_output_xgboost, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

# 2a xgboost bad
power_results_xgboost_bad <- summarize_results(sim_output_xgboost_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost_bad")

# sl
load("output/sim_output_sl.rda")
load("output/sim_output_sl_bad.rda")

# sim_output_sl<-sim_output_rf # temporaneo
# sim_output_sl_bad<-sim_output_rf_bad # temporaneo


# 1a) sl
power_results_sl  <- summarize_results(sim_output_sl, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "sl")

# 2a sl bad
power_results_sl_bad <- summarize_results(sim_output_sl_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "sl_bad")


# Table S1 - good

result <- rbind(
  power_results_glm,
  power_results_rf,
  power_results_xgboost,
  power_results_sl
)


results_all <- result  |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal"),
    smd_matched_cov_mean = round(smd_matched_cov_mean,4)
  ) |>
  dplyr::select(paired, caliper, method, delta,
                power_ttest,mcse_power_ttest, sd_ttest,
                power_glm_adj,mcse_power_glm_adj, sd_glm_adj,
                power_lmer,mcse_power_lmer, sd_lmer,
                model, initial_size_mean,smd_matched_cov_mean)


library(dplyr)
library(tidyr)
tbl_caliper001 <- make_tbl_caliper_table(results_all, 0.01)
tbl_caliper005 <- make_tbl_caliper_table(results_all, 0.05)
tbl_caliper01  <- make_tbl_caliper_table(results_all, 0.1)

tbl_results_good<-rbind(tbl_caliper001,
                        tbl_caliper005,
                        tbl_caliper01)





tbl_results_good   |>
  mutate(paired_sd_alt=paired_sd_alt |> round(4),
         equal_sd_alt= equal_sd_alt |> round(4)) |>
  relocate(initial_size_mean,caliper,model, analysis,
           equal_power_alt, equal_power_null, equal_sd_alt, equal_smd,
           paired_power_alt, paired_power_null, paired_sd_alt, paired_smd
  ) |>
  flextable::flextable() |>
  flextable::save_as_docx( path = "results/Table_S2_good_pw.docx")


tbl_caliper001 <- make_tbl_caliper_figure(results_all, 0.01)
tbl_caliper005 <- make_tbl_caliper_figure(results_all, 0.05)
tbl_caliper01  <- make_tbl_caliper_figure(results_all, 0.1)

tbl_results_good<-rbind(tbl_caliper001,
                        tbl_caliper005,
                        tbl_caliper01)

save(tbl_results_good,file="output/tbl_results_good.rda")

# Table S2 Bad


result_all_bad <- rbind(
  power_results_glm_bad,
  power_results_rf_bad,
  power_results_xgboost_bad,
  power_results_sl_bad
)

results_all_bad <- result_all_bad  |>
  mutate(
    model = sub("_bad$", "", model),
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal"),
    smd_matched_cov_mean = round(smd_matched_cov_mean,4)
  ) |>
  dplyr::select(paired, caliper, method, delta,
                power_ttest,mcse_power_ttest, sd_ttest,
                power_glm_adj,mcse_power_glm_adj, sd_glm_adj,
                power_lmer,mcse_power_lmer, sd_lmer,
                model, initial_size_mean,smd_matched_cov_mean)


tbl_caliper001 <- make_tbl_caliper_table(results_all_bad, 0.01)
tbl_caliper005 <- make_tbl_caliper_table(results_all_bad, 0.05)
tbl_caliper01  <- make_tbl_caliper_table(results_all_bad, 0.1)



tbl_results_bad<-rbind(tbl_caliper001,
                       tbl_caliper005,
                       tbl_caliper01)


tbl_results_bad |>
  mutate(paired_sd_alt=paired_sd_alt |> round(4),
         equal_sd_alt= equal_sd_alt |> round(4)) |>
  relocate(initial_size_mean,caliper,model, analysis,
           equal_power_alt, equal_power_null, equal_sd_alt, equal_smd,
           paired_power_alt, paired_power_null, paired_sd_alt, paired_smd
  ) |>
  flextable::flextable() |>
  flextable::save_as_docx( path = "results/Table_S3_bad_pw.docx")



tbl_caliper001 <- make_tbl_caliper_figure(results_all_bad, 0.01)
tbl_caliper005 <- make_tbl_caliper_figure(results_all_bad, 0.05)
tbl_caliper01  <- make_tbl_caliper_figure(results_all_bad, 0.1)
tbl_results_bad<-rbind(tbl_caliper001,
                       tbl_caliper005,
                       tbl_caliper01)


save(tbl_results_bad,file="output/tbl_results_bad.rda")


#-----------------------------------
# additional tables - UNCOMMENT TO RUN THEM
#
# tables_glm <- split_results_tables(power_results_glm)
# tables_glm$operating <-tables_glm$operating |> mutate(model = "glm")
# tables_glm$balance <-tables_glm$balance |>  mutate(model = "glm")
# tables_glm$bias<-tables_glm$bias  |>  mutate(model = "glm")
# tables_glm_bad <- split_results_tables(power_results_glm_bad)
# tables_glm_bad$operating <-tables_glm_bad$operating |> mutate(model = "glm_bad")
# tables_glm_bad$balance <-tables_glm_bad$balance |>  mutate(model = "glm_bad")
# tables_glm_bad$bias<-tables_glm_bad$bias  |>  mutate(model = "glm_bad")
# tables_rf <- split_results_tables(power_results_rf)
# tables_rf$operating <-tables_rf$operating |> mutate(model = "rf")
# tables_rf$balance <-tables_rf$balance |>  mutate(model = "rf")
# tables_rf$bias<-tables_rf$bias  |>  mutate(model = "rf")
# tables_rf_bad <- split_results_tables(power_results_rf_bad)
# tables_rf_bad$operating <-tables_rf_bad$operating |> mutate(model = "rf_bad")
# tables_rf_bad$balance <-tables_rf_bad$balance |>  mutate(model = "rf_bad")
# tables_rf_bad$bias<-tables_rf_bad$bias  |>  mutate(model = "rf_bad")
# tables_xgboost <- split_results_tables(power_results_xgboost)
# tables_xgboost$operating <-tables_xgboost$operating |> mutate(model = "xgboost")
# tables_xgboost$balance <-tables_xgboost$balance |>  mutate(model = "xgboost")
# tables_xgboost$bias<-tables_xgboost$bias  |>  mutate(model = "xgboost")
# tables_xgboost_bad <- split_results_tables(power_results_xgboost_bad)
# tables_xgboost_bad$operating <-tables_xgboost_bad$operating |> mutate(model = "xgboost_bad")
# tables_xgboost_bad$balance <-tables_xgboost_bad$balance |>  mutate(model = "xgboost_bad")
# tables_xgboost_bad$bias<-tables_xgboost_bad$bias  |>  mutate(model = "xgboost_bad")
# tables_sl <- split_results_tables(power_results_sl)
# tables_sl$operating <-tables_sl$operating |> mutate(model = "sl")
# tables_sl$balance <-tables_sl$balance |>  mutate(model = "sl")
# tables_sl$bias<-tables_sl$bias  |>  mutate(model = "sl")
# tables_sl_bad <- split_results_tables(power_results_sl_bad)
# tables_sl_bad$operating <-tables_sl_bad$operating |> mutate(model = "sl_bad")
# tables_sl_bad$balance <-tables_sl_bad$balance |>  mutate(model = "sl_bad")
# tables_sl_bad$bias<-tables_sl_bad$bias  |>  mutate(model = "sl_bad")
#


