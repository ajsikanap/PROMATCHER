# ======================================================================
# FIGURES 1–4 — CLEANED AND ORDERED SCRIPT
# ======================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(cowplot)
library(ggpubr)

# ======================================================================
# 1. FIGURE 1 — WELL SPECIFIED SCENARIO
# ======================================================================

load("results/tbl_results_good.rda")
continuous_table <- tbl_results_good

names(continuous_table) <- c(
  "Caliper", "n/2", "Model", "Analysis", "Power_eq",
  "Coverage_eq", "SE_eq", "Power_p", "Coverage_p", "SE_p"
)

df_long <- continuous_table %>%
  pivot_longer(
    cols = c(Power_eq, Power_p),
    names_to = "Power_type",
    values_to = "Power"
  ) %>%
  mutate(
    Power_type = recode(Power_type,
                        Power_eq = "Equal randomization",
                        Power_p  = "Paired randomization"),
    Model = recode(Model,
                   glm_model     = "GLM",
                   rf_model      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl_model      = "Super Learner"),
    Model = factor(Model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    Analysis = recode(Analysis,
                      ttest  = "t-test",
                      glmadj = "GLM adjustment",
                      lmer   = "Linear mixed model (pair RE)"),
    Analysis = factor(Analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)"))
  )

df_long_cov <- continuous_table %>%
  pivot_longer(
    cols = c(Coverage_eq, Coverage_p),
    names_to = "Coverage_type",
    values_to = "Coverage"
  ) %>%
  mutate(
    Coverage_type = recode(Coverage_type,
                           Coverage_eq = "Equal randomization",
                           Coverage_p  = "Paired randomization"),
    Model = recode(Model,
                   glm_model     = "GLM",
                   rf_model      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl_model      = "Super Learner"),
    Model = factor(Model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    Analysis = recode(Analysis,
                      ttest  = "t-test",
                      glmadj = "GLM adjustment",
                      lmer   = "Linear mixed model (pair RE)"),
    Analysis = factor(Analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)"))
  )

analysis_colors <- c(
  "t-test" = "#0072B2",
  "GLM adjustment" = "#E69F00",
  "Linear mixed model (pair RE)" = "#009E73"
)

pw_line_good <- ggplot(df_long, aes(
  x = Caliper, y = Power,
  color = Analysis, linetype = Power_type,
  shape = Power_type, group = interaction(Analysis, Power_type)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1)) +
  scale_color_manual(values = analysis_colors) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Power vs Caliper by Model - Well specified Scenario",
       x = "Caliper", y = "Power") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

cov_line_good <- ggplot(df_long_cov, aes(
  x = Caliper, y = Coverage,
  color = Analysis, linetype = Coverage_type,
  shape = Coverage_type, group = interaction(Analysis, Coverage_type)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1)) +
  scale_color_manual(values = analysis_colors) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Type I error vs Caliper by Model - Well specified Scenario",
       x = "Caliper", y = "Type I error") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

legend <- cowplot::get_plot_component(
  cov_line_good +
    theme(legend.position = "bottom",
          legend.title.align = 0.5,
          legend.text.align = 0.5) +
    guides(
      color = guide_legend(nrow = 3, byrow = TRUE, title.position = "top"),
      shape = guide_legend(nrow = 3, byrow = TRUE, title.position = "top"),
      linetype = guide_legend(nrow = 3, byrow = TRUE, title.position = "top")
    ),
  "guide-box", return_all = TRUE
)

figure1_good <- plot_grid(
  plot_grid(pw_line_good, cov_line_good, nrow = 2),
  legend[[3]], ncol = 1, rel_heights = c(1, 0.2)
)

# ggsave("figures/figure1_good.pdf", figure1_good, device = cairo_pdf, width = 10, height = 8, dpi = 600)
# ggsave("figures/figure1_good.svg", figure1_good, device = "svg", width = 10, height = 8, dpi = 600)
# ggsave("figures/figure1_good.png", figure1_good, device = "png", width = 10, height = 8, dpi = 600)

# ======================================================================
# 2. FIGURE 2 — MISSPECIFIED SCENARIO
# ======================================================================

load("results/tbl_results_bad.rda")
continuous_table <- tbl_results_bad

names(continuous_table) <- c(
  "Caliper", "n/2", "Model", "Analysis", "Power_eq",
  "Coverage_eq", "SE_eq", "Power_p", "Coverage_p", "SE_p"
)

df_long <- continuous_table %>%
  pivot_longer(
    cols = c(Power_eq, Power_p),
    names_to = "Power_type",
    values_to = "Power"
  ) %>%
  mutate(
    Power_type = recode(Power_type,
                        Power_eq = "Equal randomization",
                        Power_p  = "Paired randomization"),
    Model = recode(Model,
                   glm_model     = "GLM",
                   rf_model      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl_model      = "Super Learner"),
    Model = factor(Model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    Analysis = recode(Analysis,
                      ttest  = "t-test",
                      glmadj = "GLM adjustment",
                      lmer   = "Linear mixed model (pair RE)")
  )

df_long_cov <- continuous_table %>%
  pivot_longer(
    cols = c(Coverage_eq, Coverage_p),
    names_to = "Coverage_type",
    values_to = "Coverage"
  ) %>%
  mutate(
    Coverage_type = recode(Coverage_type,
                           Coverage_eq = "Equal randomization",
                           Coverage_p  = "Paired randomization"),
    Model = recode(Model,
                   glm_model     = "GLM",
                   rf_model      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl_model      = "Super Learner"),
    Model = factor(Model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    Analysis = recode(Analysis,
                      ttest  = "t-test",
                      glmadj = "GLM adjustment",
                      lmer   = "Linear mixed model (pair RE)")
  )

pw_line_bad <- ggplot(df_long, aes(
  x = Caliper, y = Power,
  color = Analysis, linetype = Power_type,
  shape = Power_type, group = interaction(Analysis, Power_type)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1)) +
  scale_color_manual(values = analysis_colors) +
  geom_hline(yintercept = 0.80, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Power vs Caliper by Model - Misspecified Scenario",
       x = "Caliper", y = "Power") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

cov_line_bad <- ggplot(df_long_cov, aes(
  x = Caliper, y = Coverage,
  color = Analysis, linetype = Coverage_type,
  shape = Coverage_type, group = interaction(Analysis, Coverage_type)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1)) +
  scale_color_manual(values = analysis_colors) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red", linewidth = 0.8) +
  labs(title = "Type I error vs Caliper by Model - Misspecified Scenario",
       x = "Caliper", y = "Type I error") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

figure2_bad <- plot_grid(
  plot_grid(pw_line_bad, cov_line_bad, nrow = 2),
  legend[[3]], ncol = 1, rel_heights = c(1, 0.2)
)

# ggsave("figures/figure2_bad.pdf", figure2_bad, device = cairo_pdf, width = 10, height = 8, dpi = 600)
# ggsave("figures/figure2_bad.svg", figure2_bad, device = "svg", width = 10, height = 8, dpi = 600)
# ggsave("figures/figure2_bad.png", figure2_bad, device = "png", width = 10, height = 8, dpi = 600)
