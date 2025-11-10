
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
        coef_ttest  = mean(coef_ttest, na.rm = TRUE),
        bias_ttest  = mean(coef_ttest, na.rm = TRUE) - unique(delta),
        mse_ttest   = mean((coef_ttest - unique(delta))^2, na.rm = TRUE),
        se_bias_ttest = sd(coef_ttest, na.rm = TRUE) / sqrt(iteractions),
        sd_ttest    = mean(sd_ttest, na.rm = TRUE),

        # glm adj
        power_glm_adj = mean(p_sign_glm_adj, na.rm = TRUE),
        coef_glm_adj  = mean(coef_glm_adj, na.rm = TRUE),
        bias_glm_adj  = mean(coef_glm_adj, na.rm = TRUE) - unique(delta),
        mse_glm_adj   = mean((coef_glm_adj - unique(delta))^2, na.rm = TRUE),
        se_bias_glm_adj = sd(coef_glm_adj, na.rm = TRUE) / sqrt(iteractions),
        sd_glm_adj    = mean(sd_glm_adj, na.rm = TRUE),

        # lmer subclass
        power_lmer = mean(p_sign_lmer, na.rm = TRUE),
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
                  sd_ttest, sd_glm_adj, sd_lmer)

  # 2) Balance
  results_balance <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  caliper_dist, smd_matched_ps_mean, smd_matched_cov_mean) %>%
    dplyr::filter(delta == 0.2)   # tieni solo scenario con effetto

  # 3) Bias, MSE, SE bias
  results_bias <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  coef_ttest, bias_ttest, mse_ttest, se_bias_ttest,
                  coef_glm_adj, bias_glm_adj, mse_glm_adj, se_bias_glm_adj,
                  coef_lmer, bias_lmer, mse_lmer, se_bias_lmer) %>%
    dplyr::filter(delta == 0.2)

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

make_tbl_caliper <- function(data, caliper_value) {
  data %>%
    filter(caliper == caliper_value) %>%
    pivot_longer(
      cols = c(power_ttest, power_glmadj, power_lmer,
               sd_ttest, sd_glmadj, sd_lmer),
      names_to = c(".value", "analysis"),
      names_pattern = "(.*)_(.*)"
    ) %>%
    pivot_wider(
      names_from = delta,
      values_from = c(power, sd),
      names_glue = "{.value}_{delta}",
      values_fill = NA
    ) %>%
    ungroup() %>%
    dplyr::select(!method) %>%
    pivot_wider(
      names_from = paired,
      values_from = c(sd_0, sd_0.2, power_0, power_0.2),
      names_glue = "{paired}_{.value}"
    ) %>%
    dplyr::select(
      caliper, initial_size_mean, model, analysis,
      equal_power_0.2, equal_power_0, equal_sd_0.2,
      paired_power_0.2, paired_power_0, paired_sd_0.2
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
