
# FUnction to summarize results
summarize_results <- function(power_levels, sample_size_to_try, iteractions = interactions) {

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





