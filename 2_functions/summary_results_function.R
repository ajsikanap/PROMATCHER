

mcse_prop <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  p <- mean(x)
  sqrt(p * (1 - p) / length(x))
}

mcse_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) <= 1) return(NA_real_)
  sd(x) / sqrt(length(x))
}


# funzione da usare in 05 per i summary dei vari modelli
summarize_results <- function(power_levels, sample_size_to_try, iteractions = 10000) {

  power_levels_temp <- vector("list", length(sample_size_to_try))

  for (i in seq_along(sample_size_to_try)) {
    power_levels_temp[[i]] <- power_levels[[i]]$final_output |>
      as.data.frame() |>
      group_by(caliper, method, delta) |>
      summarize(
        caliper_dist = mean(caliper_distance, na.rm = TRUE),
        smd_matched_ps_mean = mean(smd_matched_ps, na.rm = TRUE),
        smd_matched_cov_mean = mean(smd_matched_cov, na.rm = TRUE),
        initial_size_mean = mean(initial_size, na.rm = TRUE),
        reduced_size_mean = mean(reduced_size, na.rm = TRUE),

        # t-test
        power_ttest = mean(p_sign_ttest, na.rm = TRUE),
        mcse_power_ttest = mcse_prop(p_sign_ttest),
        coef_ttest  = mean(coef_ttest, na.rm = TRUE),
        bias_ttest  = mean(coef_ttest, na.rm = TRUE) - unique(delta),
        mse_ttest   = mean((coef_ttest - unique(delta))^2, na.rm = TRUE),
        se_bias_ttest = sd(coef_ttest, na.rm = TRUE) / sqrt(iteractions),
        sd_ttest    = mean(sd_ttest, na.rm = TRUE),

        # glm adj
        power_glm_adj = mean(p_sign_glm_adj, na.rm = TRUE),
        mcse_power_glm_adj = mcse_prop(p_sign_glm_adj),
        coef_glm_adj  = mean(coef_glm_adj, na.rm = TRUE),
        bias_glm_adj  = mean(coef_glm_adj, na.rm = TRUE) - unique(delta),
        mse_glm_adj   = mean((coef_glm_adj - unique(delta))^2, na.rm = TRUE),
        se_bias_glm_adj = sd(coef_glm_adj, na.rm = TRUE) / sqrt(iteractions),
        sd_glm_adj    = mean(sd_glm_adj, na.rm = TRUE),

        # lmer subclass
        power_lmer = mean(p_sign_lmer, na.rm = TRUE),
        mcse_power_lmer = mcse_prop(p_sign_lmer),
         coef_lmer  = mean(coef_lmer, na.rm = TRUE),
        bias_lmer  = mean(coef_lmer, na.rm = TRUE) - unique(delta),
        mse_lmer   = mean((coef_lmer - unique(delta))^2, na.rm = TRUE),
        se_bias_lmer = sd(coef_lmer, na.rm = TRUE) / sqrt(iteractions),
        sd_lmer    = mean(sd_lmer, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(sample_size = sample_size_to_try[i])
  }

  # Combina tutti i risultati
  bind_rows(power_levels_temp)
}


# funzione per dividere i risultati
split_results_tables <- function(results_df) {

  # 1) Power, type I error, SE
  results_operating <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  power_ttest, power_glm_adj, power_lmer,
                  mcse_power_ttest,mcse_power_glm_adj, mcse_power_lmer,
                  sd_ttest, sd_glm_adj, sd_lmer)

  # 2) Balance
  results_balance <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  caliper_dist, smd_matched_ps_mean, smd_matched_cov_mean) %>%
    dplyr::filter(delta == 1)   # tieni solo scenario con effetto

  # 3) Bias, MSE, SE bias
  results_bias <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  coef_ttest, bias_ttest, mse_ttest, se_bias_ttest,
                  coef_glm_adj, bias_glm_adj, mse_glm_adj, se_bias_glm_adj,
                  coef_lmer, bias_lmer, mse_lmer, se_bias_lmer) %>%
    dplyr::filter(delta == 1)

  # restituisci lista
  list(
    operating = results_operating,
    balance   = results_balance,
    bias      = results_bias
  )
}



# function for final tables
library(dplyr)
library(tidyr)

make_tbl_caliper_table <- function(data, caliper_value) {

  # 1. SMD per braccio (una riga per scenario × arm; non dipende da analisi/delta)
  smd <- data %>%
    ungroup() %>%
    filter(caliper == caliper_value) %>%
    distinct(caliper, initial_size_mean, model, paired, smd_matched_cov_mean) %>%
    pivot_wider(names_from = paired, values_from = smd_matched_cov_mean,
                names_glue = "{paired}_smd")

  # 2. tabella power/sd
  tab <- data %>%
    ungroup() %>%
    filter(caliper == caliper_value) %>%
    mutate(
      power_ttest   = paste0(power_ttest,   " (", round(mcse_power_ttest,   4), ")"),
      power_glm_adj = paste0(power_glm_adj, " (", round(mcse_power_glm_adj, 4), ")"),
      power_lmer    = paste0(power_lmer,    " (", round(mcse_power_lmer,    4), ")"),
      delta = ifelse(delta == 0, "null", "alt")
    ) %>%
    pivot_longer(
      cols = c(power_ttest, power_glm_adj, power_lmer,
               sd_ttest,    sd_glm_adj,    sd_lmer),
      names_to      = c(".value", "analysis"),
      names_pattern = "(power|sd)_(.*)"
    ) %>%
    select(caliper, initial_size_mean, model, analysis, paired, delta, power, sd) %>%
    distinct() %>%
    pivot_wider(names_from = delta,  values_from = c(power, sd),
                names_glue = "{.value}_{delta}") %>%
    pivot_wider(names_from = paired, values_from = c(power_null, power_alt, sd_null, sd_alt),
                names_glue = "{paired}_{.value}")

  # 3. unisci
  tab %>%
    left_join(smd, by = c("caliper", "initial_size_mean", "model")) %>%
    select(
      caliper, initial_size_mean, model, analysis,
      equal_smd, paired_smd,
      equal_power_alt, equal_power_null, equal_sd_alt,
      paired_power_alt, paired_power_null, paired_sd_alt
    )
}


make_tbl_caliper_figure<- function(data, caliper_value) {

  # 1. SMD per braccio (una riga per scenario × arm; non dipende da analisi/delta)
  smd <- data %>%
    ungroup() %>%
    filter(caliper == caliper_value) %>%
    distinct(caliper, initial_size_mean, model, paired, smd_matched_cov_mean) %>%
    pivot_wider(names_from = paired, values_from = smd_matched_cov_mean,
                names_glue = "{paired}_smd")

  # 2. tabella power/sd
  tab <- data %>%
    ungroup() %>%
    filter(caliper == caliper_value) %>%
    mutate(
      delta = ifelse(delta == 0, "null", "alt")
    ) %>%
    pivot_longer(
      cols = c(power_ttest, power_glm_adj, power_lmer,
               sd_ttest,    sd_glm_adj,    sd_lmer),
      names_to      = c(".value", "analysis"),
      names_pattern = "(power|sd)_(.*)"
    ) %>%
    select(caliper, initial_size_mean, model, analysis, paired, delta, power, sd) %>%
    distinct() %>%
    pivot_wider(names_from = delta,  values_from = c(power, sd),
                names_glue = "{.value}_{delta}") %>%
    pivot_wider(names_from = paired, values_from = c(power_null, power_alt, sd_null, sd_alt),
                names_glue = "{paired}_{.value}")

  # 3. unisci
  tab %>%
    left_join(smd, by = c("caliper", "initial_size_mean", "model")) %>%
    select(
      caliper, initial_size_mean, model, analysis,
      equal_smd, paired_smd,
      equal_power_alt, equal_power_null, equal_sd_alt,
      paired_power_alt, paired_power_null, paired_sd_alt
    )
}


make_tbl_balance <- function(data, caliper_value) {
  data %>%
    filter(caliper == caliper_value) %>%
    mutate(
      paired = ifelse(method == "equal", "equal", "paired")
    ) %>%
    select(caliper, sample_size, model, delta, paired,
           caliper_dist, smd_matched_ps_mean, smd_matched_cov_mean) %>%
    pivot_wider(
      names_from = paired,
      values_from = c(caliper_dist, smd_matched_ps_mean, smd_matched_cov_mean),
      names_glue = "{paired}_{.value}"
    )
}




make_tbl_caliper_table2 <- function(data, caliper_value) {


  # 2. tabella power/sd
  tab <- data %>%
    ungroup() %>%
    filter(caliper == caliper_value) %>%
    mutate(
      power_ttest   = paste0(power_ttest,   " (", round(mcse_power_ttest,   4), ")"),
      power_glm_adj = paste0(power_glm_adj, " (", round(mcse_power_glm_adj, 4), ")"),
      power_lmer    = paste0(power_lmer,    " (", round(mcse_power_lmer,    4), ")"),
      delta = ifelse(delta == 0, "null", "alt")
    ) %>%
    pivot_longer(
      cols = c(power_ttest, power_glm_adj, power_lmer,
               sd_ttest,    sd_glm_adj,    sd_lmer),
      names_to      = c(".value", "analysis"),
      names_pattern = "(power|sd)_(.*)"
    ) %>%
    select(caliper, initial_size_mean, model, analysis, paired, delta, power, sd) %>%
    distinct() %>%
    pivot_wider(names_from = delta,  values_from = c(power, sd),
                names_glue = "{.value}_{delta}") %>%
    pivot_wider(names_from = paired, values_from = c(power_null, power_alt, sd_null, sd_alt),
                names_glue = "{paired}_{.value}")

  # 3. unisci
  tab %>%
    left_join(smd, by = c("caliper", "initial_size_mean", "model")) %>%
    select(
      caliper, initial_size_mean, model, analysis,
            equal_power_alt, equal_power_null, equal_sd_alt,
      paired_power_alt, paired_power_null, paired_sd_alt
    )
}
