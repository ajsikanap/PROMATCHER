
packages_required<- c("tidyverse", "dplyr")

for (pkg in packages_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
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
