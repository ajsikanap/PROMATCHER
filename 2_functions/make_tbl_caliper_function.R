# Function for final tables
#
#
packages_required<- c("tidyr", "dplyr")

for (pkg in packages_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}


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


