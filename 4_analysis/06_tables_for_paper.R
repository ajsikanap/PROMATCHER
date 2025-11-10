load("C:/Users/AjsiKanapari/Unit of Biostatistics Epidemiology and Public Health/LACED - Randomizzation by PS - 1 - Randomizzation by PS - 1/output2/output_continuous/results_complete.rda")

source("functions/summary_results_function.R")
library(knitr)
library(flextable)

# 1) TABLE MODELLI TRAINED
load("models/glm_model.rda")
load("models/glm_model_bad.rda")
load("models/rf_model.rda")
load("models/rf_model_bad.rda")
load("models/xgboost_model.rda")
load("models/xgboost_model_bad.rda")
load("models/sl_mode.rda")
load("models/sl_model_bad.rda")

load("output/sl_list.rda")
load("output/sl_list_bad.rda")

glm_model
glm_model_bad

rf_model
rf_model_bad

xgboost_model$bestTune
xgboost_model$results|> filter(nrounds==120, max_depth==3, eta==0.1, gamma==0.5, colsample_bytree==0.6, min_child_weight==5, subsample==0.75)
xgboost_model_bad$bestTune
xgboost_model_bad$results |> filter(nrounds==100, max_depth==2, eta==0.1, gamma==0, colsample_bytree==0.6, min_child_weight==2, subsample==0.75)

cbind(sl_model$libraryNames,sl_model$cvRisk |> round(3), sl_model$coef |> round(3)) |> as.data.frame()|> flextable()

cbind(sl_model_bad$libraryNames,sl_model_bad$cvRisk |> round(3), sl_model_bad$coef |> round(3)) |> as.data.frame()|> flextable()

sl_list[[2]]  |>  summary()
sqrt(4276.7) # RMSE
sl_list_bad[[2]] |> summary()
sqrt(11263)



# Riassunto performance modelli
results <- data.frame(
  Model = c("GLM", "GLM (bad)",
            "RF", "RF (bad)",
            "XGBoost", "XGBoost (bad)",
            "SuperLearner", "SuperLearner (bad)"),
  RMSE = c(69.47, 106.09,
           69.39, 107.73,
           62.77, 105.27,
           65.39, 106.12),   # valori mediati dai best tuning
  Rsquared = c(0.822, 0.586,
               0.832, 0.556,
               0.851, 0.576,
               NA, NA),       # SL usa "risk", non R²
  MAE = c(54.29, 81.64,
          51.56, 82.49,
          49.26, 80.82,
          NA, NA)
)

print(results)
results |> flextable()




# 1) TABLE RISULTATI PW E ALPHA


# carica tabella good
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
  rename(
    power_glmadj = power_glm_adj,
    sd_glmadj = sd_glm_adj
  ) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  dplyr::select(paired, caliper, method, delta, power_ttest, sd_ttest, power_glmadj, sd_glmadj, power_lmer, sd_lmer, model, initial_size_mean)



# Table 1 Operating chr---------------
library(dplyr)
library(tidyr)
tbl_caliper001 <- make_tbl_caliper(results_all, 0.01)
tbl_caliper005 <- make_tbl_caliper(results_all, 0.05)
tbl_caliper01  <- make_tbl_caliper(results_all, 0.1)

tbl_results_good<-rbind(tbl_caliper001,
                        tbl_caliper005,
                        tbl_caliper01)

tbl_results_good   |>
  mutate(paired_sd_0.2=paired_sd_0.2 |> round(4),
                          equal_sd_0.2= equal_sd_0.2 |> round(4)) |> flextable::flextable() |> flextable::save_as_docx( path = "results/table_1_good_pw.docx")


# save output rda
save(tbl_results_good, file="results/tbl_results_good.rda")

#
# library(writexl)
# sheets <- list("caliper001" = tbl_caliper001, "caliper005" = tbl_caliper005, "caliper01" = tbl_caliper01) # assume sheet1 and sheet2 are data frames
# write_xlsx(sheets, "results/results_good_paper_format.xlsx")


# # balance
# tbl_balance_001 <- make_tbl_balance(results_all, 0.01)
# tbl_balance_005 <- make_tbl_balance(results_all, 0.05)
# tbl_balance_01  <- make_tbl_balance(results_all, 0.1)


#------------------------------------

# carica tabelle bad
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
  rename(
    power_glmadj = power_glm_adj,
    sd_glmadj = sd_glm_adj
  ) |>
  mutate(
    model = sub("_bad$", "", model),
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  dplyr::select(paired, caliper, method, delta, power_ttest, sd_ttest, power_glmadj, sd_glmadj, power_lmer, sd_lmer, model,initial_size_mean)



tbl_caliper001 <- make_tbl_caliper(results_all_bad, 0.01)
tbl_caliper005 <- make_tbl_caliper(results_all_bad, 0.05)
tbl_caliper01  <- make_tbl_caliper(results_all_bad, 0.1)



tbl_results_bad<-rbind(tbl_caliper001,
                        tbl_caliper005,
                        tbl_caliper01)

# save output rda
save(tbl_results_bad, file="results/tbl_results_bad.rda")

tbl_results_bad |>
  mutate(paired_sd_0.2=paired_sd_0.2 |> round(4),
         equal_sd_0.2= equal_sd_0.2 |> round(4)) |> flextable::flextable() |> flextable::save_as_docx( path = "results/table_2_bad_pw.docx")





# table 4 with different sizes
library(flextable)

# glm
load("output/sim_output_glm.rda")
#sim_output_glm
load("output/sim_output_glm_n49.rda")
#sim_output_glm_n49
load("output/sim_output_glm_n20.rda")
#sim_output_glm_n20
load("output/sim_output_glm_n34.rda")
load("output/sim_output_glm_n26.rda")
load("output/sim_output_glm_n16.rda")

load("output/sim_output_xgboost.rda")
#sim_output_xgboost
load("output/sim_output_xgboost_n49.rda")
#sim_output_xgboost_n49
load("output/sim_output_xgboost_n20.rda")
#sim_output_xgboost_n20
load("output/sim_output_xgboost_n34.rda")
load("output/sim_output_xgboost_n26.rda")
load("output/sim_output_xgboost_n16.rda")
# carica funzione
source("functions/continuous_function_paired.R")
source("functions/summary_results_function.R")

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



results_300_49_34_26_20_16<-rbind(
  power_results_glm,
  power_results_glm_49,
  power_results_glm_34,
  power_results_glm_26,
  power_results_glm_20,
  power_results_glm_16,

  power_results_xgboost,
  power_results_xgboost_49,
  power_results_xgboost_34,
  power_results_xgboost_26,
  power_results_xgboost_20,
  power_results_xgboost_16
)



results_300_49_34_26_20_16<-results_300_49_34_26_20_16 |> filter(caliper==0.05)

results_all <- results_300_49_34_26_20_16 |>
  rename(
    power_glmadj = power_glm_adj,
    sd_glmadj = sd_glm_adj
  ) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  dplyr::select(paired, caliper, method, delta, power_ttest, sd_ttest, power_glmadj, sd_glmadj, power_lmer, sd_lmer, model, initial_size_mean, smd_matched_cov_mean)

results_all |>
  dplyr::summarise(n = dplyr::n(), .by = c(paired, caliper, method, model, initial_size_mean, delta)) |>
  dplyr::filter(n > 1L)


# Table 1 Operating chr---------------

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
  # Pivot per delta prima
  pivot_wider(
    names_from = delta,
    values_from = c(power, sd, smd_matched_cov_mean),
    names_glue = "{.value}_{delta}",
    values_fill = NA
  ) %>%
  # Ora pivot per paired: equal vs paired nella stessa riga
  pivot_wider(
    names_from = paired,
    values_from = c(power_0, power_1, sd_1, smd_matched_cov_mean_1),
    names_glue = "{paired}_{.value}"
  ) %>%
  # Seleziono solo le colonne finali, tutte riempite
  dplyr::select(
    caliper, initial_size_mean, model, analysis,
    equal_power_1, equal_power_0, equal_sd_1, equal_smd_matched_cov_mean_1,
    paired_power_1, paired_power_0, paired_sd_1, paired_smd_matched_cov_mean_1
  ) %>%
  arrange(caliper, model, initial_size_mean)

tbl_equal<-tbl_caliper005_1 |> dplyr::select( caliper, initial_size_mean, model, analysis,
                                              equal_power_1, equal_power_0, equal_sd_1, equal_smd_matched_cov_mean_1) |> drop_na(equal_smd_matched_cov_mean_1)
tbl_paired<-tbl_caliper005_1 |> dplyr::select( caliper, initial_size_mean, model, analysis,
                                               paired_power_1, paired_power_0, paired_sd_1, paired_smd_matched_cov_mean_1) |> drop_na(paired_smd_matched_cov_mean_1)

tbl3_final<-merge(tbl_equal,tbl_paired, by=c("caliper","initial_size_mean", "model", "analysis" ))
tbl3_final<-tbl3_final |>
  mutate(across(where(is.numeric), ~ round(., 4))) |>
  filter(initial_size_mean!=300) |>
  select(-caliper) |>
  mutate(analysis=factor(analysis, levels=c("ttest","glmadj","lmer"))) |>
  arrange(initial_size_mean,model,analysis)
tbl3_final|>
  flextable::flextable() |> flextable::save_as_docx( path = "results/table_3_multiple_ss.docx")



 save(tbl3_final,file="output/tbl_caliper005_300_49_20.rda")


