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
