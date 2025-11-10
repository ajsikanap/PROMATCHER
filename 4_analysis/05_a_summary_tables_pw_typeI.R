
# Script che riassume i vari risultati di tutte le simulazioni
# good and bad, i risultati vengono poi salvati e riassunti in 06 per
# paper format
# create outputs specific for 1) power, type I error, se, 2) balance, smd 3)bias, mse, se bias
library(flextable)

# glm
load("output/sim_output_glm.rda")
load("output/sim_output_glm_bad.rda")

# carica funzione
source("functions/continuous_function_paired.R")
source("functions/summary_results_function.R")

iteractions=10000
sample_size_to_try=300


# 1a) glm
power_results_glm  <- summarize_results(sim_output_glm, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm")

tables_glm <- split_results_tables(power_results_glm)
tables_glm$operating <-tables_glm$operating |> mutate(model = "glm")
tables_glm$balance <-tables_glm$balance |>  mutate(model = "glm")
tables_glm$bias<-tables_glm$bias  |>  mutate(model = "glm")


# 2a glm bad
power_results_glm_bad <- summarize_results(sim_output_glm_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "glm_bad")

tables_glm_bad <- split_results_tables(power_results_glm_bad)
tables_glm_bad$operating <-tables_glm_bad$operating |> mutate(model = "glm_bad")
tables_glm_bad$balance <-tables_glm_bad$balance |>  mutate(model = "glm_bad")
tables_glm_bad$bias<-tables_glm_bad$bias  |>  mutate(model = "glm_bad")

tables_glm_bad$balance |> flextable()
tables_glm_bad$bias[,-c(8,12,16)] |> flextable()

save(power_results_glm, file="results/power_results_glm.rda")
save(power_results_glm_bad, file="results/power_results_glm_bad.rda")

# rf
load("output/sim_output_rf.rda")
load("output/sim_output_rf_bad.rda")


iteractions=10000
sample_size_to_try=300


# 1a) rf
power_results_rf  <- summarize_results(sim_output_rf, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "rf")

tables_rf <- split_results_tables(power_results_rf)
tables_rf$operating <-tables_rf$operating |> mutate(model = "rf")
tables_rf$balance <-tables_rf$balance |>  mutate(model = "rf")
tables_rf$bias<-tables_rf$bias  |>  mutate(model = "rf")


# 2a rf bad
power_results_rf_bad <- summarize_results(sim_output_rf_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "rf_bad")

tables_rf_bad <- split_results_tables(power_results_rf_bad)
tables_rf_bad$operating <-tables_rf_bad$operating |> mutate(model = "rf_bad")
tables_rf_bad$balance <-tables_rf_bad$balance |>  mutate(model = "rf_bad")
tables_rf_bad$bias<-tables_rf_bad$bias  |>  mutate(model = "rf_bad")


save(power_results_rf, file="results/power_results_rf.rda")
save(power_results_rf_bad, file="results/power_results_rf_bad.rda")


# xgboost
# xgboost
load("output/sim_output_xgboost.rda")
load("output/sim_output_xgboost_bad.rda")

# sim_output_xgboost<-sim_output_rf # temporaneo
# sim_output_xgboost_bad<-sim_output_rf_bad # temporaneo


iteractions=10000
sample_size_to_try=300


# 1a) xgboost
power_results_xgboost  <- summarize_results(sim_output_xgboost, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost")

tables_xgboost <- split_results_tables(power_results_xgboost)
tables_xgboost$operating <-tables_xgboost$operating |> mutate(model = "xgboost")
tables_xgboost$balance <-tables_xgboost$balance |>  mutate(model = "xgboost")
tables_xgboost$bias<-tables_xgboost$bias  |>  mutate(model = "xgboost")


# 2a xgboost bad
power_results_xgboost_bad <- summarize_results(sim_output_xgboost_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "xgboost_bad")

tables_xgboost_bad <- split_results_tables(power_results_xgboost_bad)
tables_xgboost_bad$operating <-tables_xgboost_bad$operating |> mutate(model = "xgboost_bad")
tables_xgboost_bad$balance <-tables_xgboost_bad$balance |>  mutate(model = "xgboost_bad")
tables_xgboost_bad$bias<-tables_xgboost_bad$bias  |>  mutate(model = "xgboost_bad")


save(power_results_xgboost, file="results/power_results_xgboost.rda")
save(power_results_xgboost_bad, file="results/power_results_xgboost_bad.rda")


# sl
load("output/sim_output_sl.rda")
load("output/sim_output_sl_bad.rda")

# sim_output_sl<-sim_output_rf # temporaneo
# sim_output_sl_bad<-sim_output_rf_bad # temporaneo


iteractions=1000
sample_size_to_try=300


# 1a) sl
power_results_sl  <- summarize_results(sim_output_sl, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "sl")

tables_sl <- split_results_tables(power_results_sl)
tables_sl$operating <-tables_sl$operating |> mutate(model = "sl")
tables_sl$balance <-tables_sl$balance |>  mutate(model = "sl")
tables_sl$bias<-tables_sl$bias  |>  mutate(model = "sl")


# 2a sl bad
power_results_sl_bad <- summarize_results(sim_output_sl_bad, sample_size_to_try, iteractions=iteractions) |>
  mutate(model = "sl_bad")

tables_sl_bad <- split_results_tables(power_results_sl_bad)
tables_sl_bad$operating <-tables_sl_bad$operating |> mutate(model = "sl_bad")
tables_sl_bad$balance <-tables_sl_bad$balance |>  mutate(model = "sl_bad")
tables_sl_bad$bias<-tables_sl_bad$bias  |>  mutate(model = "sl_bad")


save(power_results_sl, file="results/power_results_sl.rda")
save(power_results_sl_bad, file="results/power_results_sl_bad.rda")


# aggrega tabelle per output - table 1
result<-power_results_glm

result <- rbind(
  power_results_glm,
  power_results_rf,
  power_results_xgboost,
  power_results_sl
)

