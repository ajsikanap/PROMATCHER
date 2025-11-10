
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# 1. FIGURE 1 --------------------

# carica results good
load("results/tbl_results_good.rda")
continuous_table<-tbl_results_good

names(continuous_table)<-c("Caliper", "n/2", "Model", "Analysis", "Power_eq",
                           "Coverage_eq", "SE_eq", "Power_p", "Coverage_p",
                           "SE_p")
names(continuous_table)


df_long <- continuous_table %>%
  pivot_longer(
    cols = c(Power_eq, Power_p),
    names_to = "Power_type",
    values_to = "Power"
  )%>%
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
    Analysis=factor(Analysis, levels=c("t-test","GLM adjustment","Linear mixed model (pair RE)"))
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
                           Coverage_p = "Paired randomization"),
    # Caliper = factor(Caliper, levels = c(0.01, 0.05, 0.1)),  # livelli fissi

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
    Analysis=factor(Analysis, levels=c("t-test","GLM adjustment","Linear mixed model (pair RE)"))
  )




# Definizione palette (Okabe-Ito)
analysis_colors <- c(
  "t-test"  = "#0072B2", # blu
  "GLM adjustment" = "#E69F00", # arancione
  "Linear mixed model (pair RE)"   = "#009E73"  # verde
)

# POWER
library(ggplot2)
pw_line_good <- ggplot(df_long, aes(x = Caliper, y = Power,
                               color = Analysis,
                               linetype = Power_type,
                               shape = Power_type,
                               group = interaction(Analysis, Power_type))) +
  #geom_line(linewidth = 1, alpha = 0.9) +
 # geom_point(size = 2) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1))+
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01))+
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  labs(
    title = "Power vs Caliper by Model - Well specified Scenario",
    x = "Caliper",
    y = "Power"
  ) +
  scale_color_manual(values = analysis_colors) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "red", linewidth = 0.8)+
  guides(
    color    = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape    = guide_legend(nrow = 2, byrow = TRUE)
  )

pw_line_good

# type I error
cov_line_good <- ggplot(df_long_cov, aes(x = Caliper, y = Coverage,
                                    color = Analysis,
                                    linetype = Coverage_type,
                                    shape = Coverage_type,
                                    group = interaction(Analysis, Coverage_type))) +
 geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01))+
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  labs(
    title = "Type I error vs Caliper by Model - Well specified Scenario",
    x = "Caliper",
    y = "Type I error",
    color = "Analysis Method",
    linetype = "Power / Type I error",
    shape = "Power / Type I error"
  ) +
  scale_color_manual(values = analysis_colors) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1))+
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8)+
  guides(
    color    = guide_legend(order=1,nrow = 3, byrow = TRUE),
    linetype = guide_legend(order=2,nrow = 2, byrow = TRUE),
    shape    = guide_legend(order=2,nrow = 2, byrow = TRUE)
  )

cov_line_good


library(cowplot)
legend <- cowplot::get_plot_component(
  cov_line_good +
    theme(
      legend.position = "bottom",
      legend.title.align = 0.5,         # centers the legend title
      legend.text.align = 0.5           # optional, centers the text under each key
    ) +
    guides(
      color = guide_legend(
        nrow = 3,
        byrow = TRUE,
        title.position = "top"           # puts the legend title above the keys
      ),
      shape = guide_legend(
        nrow = 3,
        byrow = TRUE,
        title.position = "top"
      ),
      linetype = guide_legend(
        nrow = 3,
        byrow = TRUE,
        title.position = "top"
      )
    )

    , 'guide-box', return_all = TRUE)

library(ggpubr)
as_ggplot(legend[[3]])

figure1_good <- cowplot::plot_grid(
  cowplot::plot_grid(pw_line_good, cov_line_good, nrow = 2),
  legend[[3]],
  ncol = 1,
  rel_heights = c(1, 0.2)
)



ggsave(
  filename = "figures/figure1_good.pdf",   # nome file
  plot = figure1_good,        # oggetto del plot
  device = cairo_pdf,          # usa Cairo per testo vettoriale pulito
  width = 10,                  # larghezza in pollici (adatta a colonna singola/doppia)
  height = 8,                 # altezza in pollici
  dpi = 600                   # risoluzione (anche se EPS è vettoriale, utile per embedded raster)
)

ggsave(
  filename = "figures/figure1_good.svg",
  plot = figure1_good,
  device = "svg",
  width = 10,
  height = 8,
  dpi = 600
  )

ggsave(
  filename = "figures/figure1_good.png",
  plot = figure1_good,
  device = "png",
  width = 10,
  height = 8,
  dpi = 600
)







# 2. FIGURE 2: bad model ------------------------------

load("results/tbl_results_bad.rda")
continuous_table<-tbl_results_bad

names(continuous_table)<-c("Caliper", "n/2", "Model", "Analysis", "Power_eq",
                           "Coverage_eq", "SE_eq", "Power_p", "Coverage_p",
                           "SE_p")


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
                           Coverage_p = "Paired randomization"),
    # Caliper = factor(Caliper, levels = c(0.01, 0.05, 0.1)),  # livelli fissi

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



# POWER
pw_line_bad <- ggplot(df_long, aes(x = Caliper, y = Power,
                               color = Analysis,
                               linetype = Power_type,
                               shape = Power_type,
                               group = interaction(Analysis, Power_type))) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1))+
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01))+
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  labs(
    title = "Power vs Caliper by Model - Misspecified Scenario",
    x = "Caliper",
    y = "Power"
  ) +
  scale_color_manual(values = analysis_colors) +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "red", linewidth = 0.8)+
  guides(
    color    = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape    = guide_legend(nrow = 2, byrow = TRUE)
  )

pw_line_bad

# COVERAGE
cov_line_bad <- ggplot(df_long_cov, aes(x = Caliper, y = Coverage,
                                    color = Analysis,
                                    linetype = Coverage_type,
                                    shape = Coverage_type,
                                    group = interaction(Analysis, Coverage_type))) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01))+
  facet_wrap(~Model, nrow = 1, labeller = label_both) +
  labs(
    title = "Type I error vs Caliper by Model - Misspecified Scenario",
    x = "Caliper",
    y = "Type I error",
    color = "Analysis Method",
    linetype = "Power / Type I error",
    shape = "Power / Type I error"
  ) +
  scale_color_manual(values = analysis_colors) +
  scale_x_continuous(breaks = c(0.01, 0.05, 0.1))+
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position="none") +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8)+
  guides(
    color    = guide_legend(order=1,nrow = 3, byrow = TRUE),
    linetype = guide_legend(order=2,nrow = 2, byrow = TRUE),
    shape    = guide_legend(order=2,nrow = 2, byrow = TRUE)
  )

cov_line_bad


# library(cowplot)
# legend <- cowplot::get_plot_component(
#   cov_line_bad  +
#     theme(
#       legend.position = "bottom",
#       legend.title.align = 0.5,         # centers the legend title
#       legend.text.align = 0.5           # optional, centers the text under each key
#     ) +
#     guides(
#       color = guide_legend(
#         nrow = 3,
#         byrow = TRUE,
#         title.position = "top"           # puts the legend title above the keys
#       ),
#       shape = guide_legend(
#         nrow = 3,
#         byrow = TRUE,
#         title.position = "top"
#       ),
#       linetype = guide_legend(
#         nrow = 3,
#         byrow = TRUE,
#         title.position = "top"
#       )
#     )
#
#   , 'guide-box', return_all = TRUE)

library(ggpubr)

figure2_bad <- cowplot::plot_grid(
  cowplot::plot_grid(pw_line_bad, cov_line_bad, nrow = 2),
  legend[[3]],
  ncol = 1,
  rel_heights = c(1, 0.2)
)



ggsave(
  filename = "figures/figure2_bad.pdf",   # nome file
  plot = figure2_bad,        # oggetto del plot
  device = cairo_pdf,          # usa Cairo per testo vettoriale pulito
  width = 10,                  # larghezza in pollici (adatta a colonna singola/doppia)
  height = 8,                 # altezza in pollici
  dpi = 600                   # risoluzione (anche se EPS è vettoriale, utile per embedded raster)
)

ggsave(
  filename = "figures/figure2_bad.svg",
  plot = figure2_bad,
  device = "svg",
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  filename = "figures/figure2_bad.png",
  plot = figure2_bad,
  device = "png",
  width = 10,
  height = 8,
  dpi = 600
)





# 3. FIGURA 3-------
load("output/tbl_caliper005_300_49_20.rda")
tbl3_final |> flextable()

library(dplyr)
library(tidyr)
library(ggplot2)


# 1. Riformatta il dataset per plot publication-ready
plot_df <- tbl3_final %>%
  select(initial_size_mean, model, analysis,
         equal_power_1, paired_power_1) %>%
  pivot_longer(
    cols = c(equal_power_1, paired_power_1),
    names_to = "method",
    values_to = "power"
  ) %>%
  mutate(
    method = recode(method,
                    equal_power_1      = "Equal randomization",
                    paired_power_1 = "Paired randomization"),
    analysis = recode(analysis,
                      ttest   = "t-test",
                      glmadj = "GLM adjustment",
                      lmer    = "Linear mixed model (pair RE)"),
    model = recode(model,
                   glm_model   = "GLM",
                   rf      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl      = "Super Learner")
  ) %>%
  distinct(initial_size_mean, model, analysis, method, .keep_all = TRUE) %>%
  mutate(
    model = factor(model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    analysis = factor(analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)")),
    method = factor(method, levels = c("Equal randomization", "Paired randomization"))
  )

# 2. Palette colori per analisi
analysis_colors <- c(
  "t-test" = "#0072B2",           # blu
  "GLM adjustment" = "#E69F00",    # arancione
  "Linear mixed model (pair RE)" = "#009E73"  # verde
)

# 3. Plot coerente con gli altri
pw_small <- ggplot(plot_df, aes(
  x = initial_size_mean,
  y = power,
  color = analysis,
  linetype = method,
  shape = method,
  group = interaction(analysis, method, model)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~model, nrow = 1, labeller = label_both) +
  geom_hline(yintercept = 0.80, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  scale_color_manual(values = analysis_colors) +
  scale_shape_manual(values = c(
    "Equal randomization" = 16,   # cerchio pieno
    "Paired randomization" = 17   # triangolo pieno
  )) +
  labs(
    title = "Power vs Sample Size by Model",
    x = "Sample size",
    y = "Power",
    color = "Analysis",
    shape = "Randomization method",
    linetype = "Randomization method"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  guides(
    color    = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape    = guide_legend(nrow = 2, byrow = TRUE)
  )

pw_small



plot_df2 <- tbl3_final %>%   # sostituisci con il nome del tuo oggetto
  select(initial_size_mean, model, analysis,
         equal_power_0, paired_power_0) %>%
  pivot_longer(
    cols = c(equal_power_0, paired_power_0),
    names_to = "group",
    values_to = "typeIerror"
  ) |>
  mutate(
    group = ifelse(group == "equal_power_0", "equal", "paired"),
    method = recode(group,
                    equal  = "Equal randomization",
                    paired = "Paired randomization"),
    analysis = recode(analysis,
                      ttest   = "t-test",
                      glmadj = "GLM adjustment",
                      lmer    = "Linear mixed model (pair RE)"),
    model = recode(model,
                   glm_model   = "GLM",
                   rf      = "Random Forest",
                   xgboost_model = "XGBoost",
                   sl      = "Super Learner")
  ) %>%
  distinct(initial_size_mean, model, analysis, method, .keep_all = TRUE) %>%
  mutate(
    model = factor(model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    analysis = factor(analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)")),
    method = factor(method, levels = c("Equal randomization", "Paired randomization"))
  )

typeI_small<- ggplot(plot_df2, aes(
  x = initial_size_mean,
  y = typeIerror,
  color = analysis,
  linetype = method,
  shape = method,
  group = interaction(analysis, method, model)
)) +
  geom_point(size = 2, position = position_dodge(width = 0.01)) +
  geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
  facet_wrap(~model, nrow = 1, labeller = label_both) +
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  scale_color_manual(values = analysis_colors) +
  scale_shape_manual(values = c(
    "Equal randomization" = 16,   # cerchio pieno
    "Paired randomization" = 17   # triangolo pieno
  )) +
  labs(
    title = "Type I error vs Sample Size by Model",
    x = "Sample size",
    y = "Type I error",
    color = "Analysis",
    shape = "Randomization method",
    linetype = "Randomization method"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  guides(
    color    = guide_legend(nrow = 3, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE),
    shape    = guide_legend(nrow = 2, byrow = TRUE)
  )

typeI_small



# library(cowplot)
# legend <- cowplot::get_plot_component(
#   typeI_small +
#     theme(legend.position="bottom")+
#     guides(
#       color    = guide_legend(order=1,nrow = 3, byrow = TRUE),
#       linetype = guide_legend(order=2,nrow = 2, byrow = TRUE),
#       shape    = guide_legend(order=2,nrow = 2, byrow = TRUE))
#
#   , 'guide-box', return_all = TRUE)
#
library(ggpubr)
as_ggplot(legend[[3]])

figure3 <- cowplot::plot_grid(
  cowplot::plot_grid(pw_small, typeI_small, nrow = 2),
  legend[[3]],
  ncol = 1,
  rel_heights = c(1, 0.2)
)



ggsave(
  filename = "figures/figure3.pdf",   # nome file
  plot = figure3,        # oggetto del plot
  device = cairo_pdf,          # usa Cairo per testo vettoriale pulito
  width = 10,                  # larghezza in pollici (adatta a colonna singola/doppia)
  height = 8,                 # altezza in pollici
  dpi = 600                   # risoluzione (anche se EPS è vettoriale, utile per embedded raster)
)

ggsave(
  filename = "figures/figure3.svg",
  plot = figure3,
  device = "svg",
  width = 10,
  height = 8,
  dpi = 600
)

ggsave(
  filename = "figures/figure3.png",
  plot = figure3,
  device = "png",
  width = 10,
  height = 8,
  dpi = 600
)






# 4. SUPPLEMENTARY------------------
#
# Other measures
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
#
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

library(dplyr)
library(tidyr)
library(ggplot2)

# good
# bad

# --- Preparazione dati --- good
library(dplyr)
library(tidyr)

df_long_good <- result %>%
  select(caliper, method, model,
         bias_ttest, mse_ttest,
         bias_glm_adj, mse_glm_adj,
         bias_lmer, mse_lmer,
         smd_matched_ps_mean, sd_ttest, sd_glm_adj, sd_lmer) %>%
  pivot_longer(
    cols = starts_with(c("bias", "mse", "sd")),
    names_to = c(".value", "analysis"),
    names_pattern = "(bias|mse|sd)_(.*)"
  ) %>%
  mutate(
    rmse = sqrt(mse),
    method=ifelse(method=="equal","equal","paired"),
    method = recode(method,
                    equal  = "Equal randomization",
                    paired = "Paired randomization"),
    log_bias = log(abs(bias) + 1e-6),  # per evitare log(0)
    model = recode(model,
                   glm     = "GLM",
                   rf      = "Random Forest",
                   xgboost = "XGBoost",
                   sl      = "Super Learner"),
    analysis = recode(analysis,
                      ttest    = "t-test",
                      glm_adj  = "GLM adjustment",
                      lmer     = "Linear mixed model (pair RE)")
  ) %>%
  distinct(method, caliper, analysis, model, .keep_all = TRUE) %>%
  mutate(
    model = factor(model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    analysis = factor(analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)")),
    method = factor(method, levels = c("Equal randomization", "Paired randomization"))
  )

# eventualmente filtrare solo le analisi che ti interessano
df_plot_good <- df_long_good %>% filter(analysis %in% c("t-test", "GLM adjustment"))
#
# df_long_good$model <- factor(df_long_good$model, levels = rev(c("glm", "rf", "xgboost", "sl")))
# df_long_good$analysis <- factor(df_long_good$analysis, levels = rev(c("glm_adj", "ttest", "lmer")))
# df_plot_good <- df_long_good %>% filter(analysis %in% c("ttest", "glm_adj"))


# --- 1. Plot SMD ---
p_smd_good <- ggplot(df_plot_good, aes(x = caliper, y = smd_matched_ps_mean,
                                       color = model, shape = method)) +
  geom_point(size = 3, stroke = 1) +   # stroke aumenta lo spessore del bordo
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  facet_wrap(~analysis) +
  scale_color_manual(values = c(
    "glm" = "#0072B2",    # blu
    "rf"  = "#009E73",    # verde
    "xgboost" = "#D55E00",    # viola
    "sl"  = "#CC7"     # nero
  ))+
  scale_shape_manual(values = c("equal" = 1,  # cerchio vuoto
                                "paired" = 2)) + # triangolo vuoto
  labs(x = "Caliper", y = "Mean matched SMD",
       title = "Balance (SMD) - Well specified scenario") +
  theme_minimal()

p_smd_good


# --- 2. Bias vs RMSE ---

p_bias_SD_good <- ggplot(df_long_good,
                         aes(x = log_bias, y = sd,
                             color = model, linetype = analysis, shape = method)) +
  geom_linerange(aes(ymin = 0.060, ymax = sd), alpha = 0.7) + # linea verticale
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40")+
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "glm" = "#0072B2",    # blu
    "rf"  = "#009E73",    # verde
    "xgboost" = "#D55E00",    # viola
    "sl"  = "#CC7"     # nero
  ))+
  ylim(0.060,0.085)+
  facet_wrap(~caliper) +
  scale_shape_manual(values = c("equal" = 16, "paired" = 17)) +
  labs(x = "log(|Bias|)", y = "Treatment Standard Error",
       title = "Bias vs SE tradeoff - Well specified scenario") +
  theme_minimal()



#--- Preparazione dati --- bad
df_long_bad <-  result_all_bad %>%
  select(caliper, method, model,
         bias_ttest, mse_ttest,
         bias_glm_adj, mse_glm_adj,
         bias_lmer, mse_lmer,
         smd_matched_ps_mean, sd_ttest, sd_glm_adj, sd_lmer) %>%
  pivot_longer(
    cols = starts_with(c("bias", "mse", "sd")),
    names_to = c(".value", "analysis"),
    names_pattern = "(bias|mse|sd)_(.*)"
  ) %>%
  mutate(
    rmse = sqrt(mse),
    method=ifelse(method=="equal","equal","paired"),
    method = recode(method,
                    equal  = "Equal randomization",
                    paired = "Paired randomization"),
    log_bias = log(abs(bias) + 1e-6),  # per evitare log(0)
    model = recode(model,
                   glm_bad     = "GLM",
                   rf_bad      = "Random Forest",
                   xgboost_bad = "XGBoost",
                   sl_bad      = "Super Learner"),
    analysis = recode(analysis,
                      ttest    = "t-test",
                      glm_adj  = "GLM adjustment",
                      lmer     = "Linear mixed model (pair RE)")
  ) %>%
  distinct(method, caliper, analysis, model, .keep_all = TRUE) %>%
  mutate(
    model = factor(model, levels = c("GLM", "Random Forest", "XGBoost", "Super Learner")),
    analysis = factor(analysis, levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)")),
    method = factor(method, levels = c("Equal randomization", "Paired randomization")))


df_plot_bad<- df_long_bad %>% filter(analysis %in% c("t-test", "GLM adjustment"))

# --- 1. Plot SMD ---
p_smd_bad <- ggplot(df_plot_bad, aes(x = caliper, y = smd_matched_ps_mean,
                                     color = model, shape = method)) +
  geom_point(size = 3, stroke = 1) +   # stroke aumenta lo spessore del bordo
  geom_hline(yintercept = 0.05, linetype = "dashed",
             color = "red", linewidth = 0.8) +
  facet_wrap(~analysis) +
  scale_color_manual(values = c(
    "glm" = "#0072B2",    # blu
    "rf"  = "#009E73",    # verde
    "xgboost" = "#D55E00",    # viola
    "sl"  = "#CC7"     # nero
  ))+
  scale_shape_manual(values = c("equal" = 1,  # cerchio vuoto
                                "paired" = 2)) + # triangolo vuoto
  labs(x = "Caliper", y = "Mean matched SMD",
       title = "Balance (SMD) - Bad specified scenario") +
  theme_minimal()

p_smd_bad
ggpubr::ggarrange(p_smd_good, p_smd_bad, nrow = 2)

# --- 2. Bias vs RMSE ---

p_bias_SD_bad <- ggplot(df_long_bad,
                        aes(x = log_bias, y = sd,
                            color = model, linetype = analysis, shape = method)) +
  geom_linerange(aes(ymin = 0.060, ymax = sd), alpha = 0.7) + # linea verticale
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey40")+
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "glm" = "#0072B2",    # blu
    "rf"  = "#009E73",    # verde
    "xgboost" = "#D55E00",    # viola
    "sl"  = "#CC7"     # nero
  ))+
  ylim(0.06,0.085)+
  facet_wrap(~caliper) +
  scale_shape_manual(values = c("equal" = 16, "paired" = 17)) +
  labs(x = "log(|Bias|)", y = "Treatment Standard Error",
       title = "Bias vs SE tradeoff - Bad specified scenario") +
  theme_minimal()
p_bias_SD_bad


ggpubr::ggarrange(p_bias_SD_good, p_bias_SD_bad, nrow = 2)




# standard error
library(ggplot2)
library(dplyr)
library(tidyr)

## good

p_se_good<-ggplot(df_plot_good, aes(x = sd, y = model, shape=method)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(caliper, model, analysis)),
            position = position_dodge(width = 0.5), linetype = "dashed") +
  facet_grid(analysis ~ caliper, scales = "free_x") +
  labs(
    x = "Standard error (se)",
    y = "Model for Score estimation",
    title = "Standard Error reduction - Well specified"
  ) +
  theme_minimal(base_size = 14)+
  xlim(0.061,0.085) +
  scale_shape_manual(values = c("Equal randomization" = 16, "Paired randomization" = 2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        legend.position="bottom")# 1 = empty circle
p_se_good

## bad

p_se_bad<-ggplot(df_plot_bad, aes(x = sd, y = model, shape=method)) +
  geom_point(size = 3) +
  geom_line(aes(group = interaction(caliper, model, analysis)),
            position = position_dodge(width = 0.5), linetype = "dashed") +
  facet_grid(analysis ~ caliper, scales = "free_x") +
  labs(
    x = "Standard error (se)",
    y = "Model for Score estimation",
    title = "Standard Error reduction - Bad Specified"
  ) +
  theme_minimal(base_size = 14)+
  xlim(0.061,0.085) +
  scale_shape_manual(values = c("Equal randomization" = 16, "Paired randomization" = 2)) +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        legend.position="bottom")# 1 = empty circle
p_se_bad


ggpubr::ggarrange(p_se_good, p_se_bad, nrow = 2)

