load("output/sim_seq_xgboost_n16.rda")
load("output/sim_seq_glm_n16.rda")

load("output/sim_seq_xgboost_n20.rda")
load("output/sim_seq_glm_n20.rda")

load("output/sim_seq_xgboost_n26.rda")
load("output/sim_seq_glm_n26.rda")

load("output/sim_seq_xgboost_n34.rda")
load("output/sim_seq_glm_n34.rda")

load("output/sim_seq_xgboost_n49.rda")
load("output/sim_seq_glm_n49.rda")



source("2_functions/continuous_function_sequential.R")

sim_seq_glm_n16$final_output<-sim_seq_glm_n16$final_output |> mutate(model="glm")
sim_seq_glm_n20$final_output<-sim_seq_glm_n20$final_output |> mutate(model="glm")
sim_seq_glm_n26$final_output<-sim_seq_glm_n26$final_output |> mutate(model="glm")
sim_seq_glm_n34$final_output<-sim_seq_glm_n34$final_output |> mutate(model="glm")
sim_seq_glm_n49$final_output<-sim_seq_glm_n49$final_output |> mutate(model="glm")

sim_seq_xgboost_n16$final_output<-sim_seq_xgboost_n16$final_output |> mutate(model="xgboost")
sim_seq_xgboost_n20$final_output<-sim_seq_xgboost_n20$final_output |> mutate(model="xgboost")
sim_seq_xgboost_n26$final_output<-sim_seq_xgboost_n26$final_output |> mutate(model="xgboost")
sim_seq_xgboost_n34$final_output<-sim_seq_xgboost_n34$final_output |> mutate(model="xgboost")
sim_seq_xgboost_n49$final_output<-sim_seq_xgboost_n49$final_output |> mutate(model="xgboost")

all_res<-list(sim_seq_glm_n16,
              sim_seq_glm_n20,
              sim_seq_glm_n26,
              sim_seq_glm_n34,
              sim_seq_glm_n49,

              sim_seq_xgboost_n16,
              sim_seq_xgboost_n20,
              sim_seq_xgboost_n26,
              sim_seq_xgboost_n34,
              sim_seq_xgboost_n49
)

#save(all_res, file="output/all_output_sequential_v1.rda")

all_res |> View()

library(purrr)
res_summary<-map(all_res, ~ summarize_results_sequential(result_obj = .x)) |> bind_rows(.id="scenario")


res_summary |> View()

results<-res_summary|>
  rename(
    power_glmadj = power_glm_adj,
    sd_glmadj = sd_glm_adj
  ) |>
  mutate(
    model = paste(model, "_model", sep = ""),
    paired = ifelse(method %in% model, "paired", "equal")
  ) |>
  dplyr::select(paired, caliper, method, delta,n_arrived_mean,n_unmatched_mean,n_excluded,n_matched_pairs_mean,prop_unmatched_mean,
                power_ttest, sd_ttest, power_glmadj, sd_glmadj, power_lmer, sd_lmer,
                mcse_power_ttest,mcse_power_glm_adj, mcse_power_lmer,
                power_perm, mcse_power_perm,
                model)




library(tidyr)
library(dplyr)
library(tidyr)

library(dplyr)
library(tidyr)


tbl_caliper_waste<-function(caliper_val, results) {
  results %>%
  filter(caliper %in% caliper_val) %>%
  group_by(method, caliper) |>
  pivot_longer(
    cols = c(power_ttest, power_glmadj, power_lmer,
             sd_ttest, sd_glmadj, sd_lmer),
    names_to = c(".value", "analysis"),
    names_pattern = "^(power|sd)_(.*)$"
  ) %>%
  mutate(
    delta= if_else(delta == 0, 0L, 1L),
    paired = as.character(paired)
  ) %>%
  distinct(
    n_matched_pairs_mean,caliper, model, analysis, paired, delta,
    .keep_all = TRUE
  ) %>%
  select(
    n_matched_pairs_mean,
    caliper, model, analysis, paired, delta,
    method,
    n_arrived_mean,
    n_unmatched_mean,
    n_excluded,
    prop_unmatched_mean,
    power,
    sd
  ) %>%
  pivot_wider(
    id_cols = c(
      caliper, model, analysis, paired,
      method,
      n_arrived_mean,
      n_unmatched_mean,
      n_excluded,
      n_matched_pairs_mean,
      prop_unmatched_mean
    ),
    names_from = delta,
    values_from = c(power, sd),
    names_glue = "{.value}_{delta}",
    values_fill = NA
  ) %>%
  pivot_wider(
    id_cols = c(caliper, model, analysis, n_matched_pairs_mean),
    names_from = paired,
    values_from = c(
      method,
      n_arrived_mean,
      n_unmatched_mean,
      prop_unmatched_mean,
      n_excluded,
      power_0,
      power_1,
      sd_0,
      sd_1
    ),
    names_glue = "{paired}_{.value}",
    values_fill = NA
  ) |> select(-equal_n_unmatched_mean,-equal_n_arrived_mean,-equal_n_excluded,-equal_method,
              -equal_sd_1,-paired_sd_1,-equal_sd_0,-paired_sd_0, -paired_n_unmatched_mean , -equal_prop_unmatched_mean, -paired_method)

}

tbl_caliper_waste(caliper_val = c(0.01,0.05,0.10), results=results) |> head()



library(dplyr); library(tidyr); library(ggplot2); library(flextable); library(scales)

base <- res_summary |>
  mutate(
    sample_size = round(target_n),
    arm         = ifelse(method == "equal", "equal", "paired"),
    # scarto CORRETTO, ricavato da n_matched e n_unmatched (bypassa il bug di n_arrived)
    n_screened     = 2 * n_matched_pairs_mean + n_unmatched_mean,
    prop_wasted    = n_unmatched_mean / n_screened,
    matching_yield = 2 * n_matched_pairs_mean / n_screened
  )

waste_tbl <- base |>
  filter(arm == "paired") |>
  distinct(model, caliper, sample_size,
           n_screened, n_unmatched_mean, prop_wasted, matching_yield) |>
  transmute(
    Model        = toupper(model),
    `Pairs (n)`  = sample_size,
    Caliper      = caliper,
    `Screened`   = round(n_screened, 1),
    `Wasted (n)` = round(n_unmatched_mean, 1),
    `Wasted (%)` = round(100 * prop_wasted, 1),
    `Yield (%)`  = round(100 * matching_yield, 1)
  ) |>
  arrange(Model, `Pairs (n)`, Caliper)

library(dplyr); library(tidyr); library(ggplot2); library(flextable); library(scales)

# per il TESTO: versione ridotta agli n piccoli, dove lo scarto morde di più
waste_tbl |>
  filter(`Pairs (n)` %in% c(16, 26, 49)) |>
  flextable() |> autofit() |>
  set_caption("Matching yield and wasted (screened-but-unmatched) subjects under sequential enrolment.")  |>
  flextable::save_as_docx( path = "results/table_waste.docx")


## supplementary table

oc_long <- base |>
  mutate(power_ttest = paste0(power_ttest, " (", round(mcse_power_ttest,4), ")"),
         power_perm =  paste0(power_perm, " (", round(mcse_power_perm,4), ")"),
         power_glm_adj=  paste0(power_glm_adj, " (", round(mcse_power_glm_adj,4), ")"),
         power_lmer =  paste0(power_lmer, " (", round(mcse_power_lmer,4), ")")) |>
  pivot_longer(c(power_ttest, power_perm, power_glm_adj, power_lmer),
               names_to = "analysis", names_prefix = "power_",
               values_to = "rate") |>
  mutate(metric = ifelse(delta == 0, "Type I error", "Power"))


oc_tbl <- oc_long |>
  select(model, sample_size, caliper, analysis, arm, metric, rate) |>
  pivot_wider(names_from = c(arm, metric), values_from = rate) |>
  arrange(caliper, sample_size, model)

oc_tbl |> flextable() |> autofit() |>
  set_caption("Operating characteristics under sequential enrolment with estimated SD(Y0).") |>
  flextable::save_as_docx( path = "results/table_SupplS10_Sequential.docx")

#### figures

plot_base <- results |>
  mutate(
    sample_size = round(n_matched_pairs_mean),             # number of pairs (target)
    n_screened  = 2 * n_matched_pairs_mean + n_unmatched_mean,
    prop_wasted = n_unmatched_mean / n_screened,           # CORRECTED waste fraction
    model_lab   = recode(model,
                         glm_model     = "GLM",
                         xgboost_model = "XGBoost"),
    method_lab  = recode(paired,
                         equal  = "Equal randomization",
                         paired = "Paired randomization"),
    caliper_lab = factor(paste0("c = ", formatC(caliper, format = "f", digits = 2)),
                         levels = paste0("c = ", formatC(sort(unique(caliper)),
                                                         format = "f", digits = 2)))
  ) |>
  mutate(
    model_lab  = factor(model_lab,  levels = c("GLM", "XGBoost")),
    method_lab = factor(method_lab, levels = c("Equal randomization", "Paired randomization"))
  )

# Okabe-Ito palette (same vocabulary as the strata figure) --------------------
analysis_colors <- c(
  "t-test"               = "#0072B2",   # blue
  "GLM adjustment"       = "#E69F00",   # orange
  "Linear mixed model (pair RE)" = "#CC79A7"    # reddish-purple
 , "Permutation Test"        = "#009E73"   # <- uncomment when power_perm is added
)

method_shapes <- c("Equal randomization" = 16,   # filled circle
                   "Paired randomization"           = 17)   # filled triangle (takes colour cleanly)

# =============================================================================
# FIGURE A — WASTED SUBJECTS (answers Reviewer 2; paired arm only)
# =============================================================================
waste_df <- plot_base |>
  filter(paired == "paired") |>
  distinct(model_lab, caliper, sample_size, prop_wasted, n_screened, n_unmatched_mean) |>
  mutate(caliper_f = factor(formatC(caliper, format = "f", digits = 2)))

waste_colors <- c("0.01" = "#0072B2", "0.05" = "#E69F00", "0.10" = "#D55E00")

fig_waste <- ggplot(waste_df, aes(
  x = sample_size, y = prop_wasted,
  color = caliper_f, group = caliper_f, shape = caliper_f
)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2) +
  facet_wrap(~ model_lab, nrow = 1, labeller = label_both) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_color_manual(values = waste_colors) +
  labs(
    title = "Screened-but-unmatched subjects under sequential enrolment",
    x = "Number of pairs", y = "Wasted subjects (%)",
    color = "Caliper", shape = "Caliper"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

fig_waste

ggsave("figures/figure_waste_seq.pdf", fig_waste, device = cairo_pdf,
       width = 8, height = 4, dpi = 600)
ggsave("figures/figure_waste_seq.png", fig_waste, device = "png",
       width = 8, height = 4, dpi = 600)

# =============================================================================
# FIGURE B — OPERATING CHARACTERISTICS (power + type I error)
# Same aesthetic as the strata figure: colour = analysis, shape/linetype = method.
# Caliper added as facet ROWS -> facet_grid(caliper ~ model).
# =============================================================================
oc_long <- plot_base |>
  select(caliper_lab, model_lab, method_lab, delta, sample_size,
         power_ttest, power_glmadj, power_lmer, power_perm) |>
  pivot_longer(starts_with("power_"),
               names_to = "analysis", names_prefix = "power_",
               values_to = "rate") |>
  mutate(
    analysis = recode(analysis,
                      ttest  = "t-test",
                      glmadj = "GLM adjustment",
                      lmer   = "Linear mixed model (pair RE)",
                      perm = "Permutation Test"),
    analysis = factor(analysis,
                      levels = c("t-test", "GLM adjustment", "Linear mixed model (pair RE)",
                                 "Permutation Test")),
    metric   = ifelse(delta == 0, "Type I error", "Power")
  ) |>
  distinct(caliper_lab, model_lab, method_lab, analysis, metric, sample_size, .keep_all = TRUE)

# helper to keep both panels visually identical -------------------------------
oc_panel <- function(df, yname, href) {
  ggplot(df, aes(
    x = sample_size, y = rate,
    color = analysis, linetype = method_lab, shape = method_lab,
    group = interaction(analysis, method_lab)
  )) +
    geom_point(size = 2, position = position_dodge(width = 0.01)) +
    geom_line(linewidth = 0.5, position = position_dodge(width = 0.01)) +
    facet_grid( ~ model_lab + caliper_lab) +
    geom_hline(yintercept = href, linetype = "dashed",
               color = "red", linewidth = 0.8) +
    scale_color_manual(values = analysis_colors) +
    scale_shape_manual(values = method_shapes) +
    labs(x = "Number of pairs", y = yname,
         color = "Analysis", shape = "Randomization method",
         linetype = "Randomization method") +
    theme_minimal(base_size = 10) +
    scale_x_continuous(breaks = c(16,20,26,34,49))+
    theme(
      strip.text  = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    ) +
    guides(
      color    = guide_legend(nrow = 3, byrow = TRUE),
      linetype = guide_legend(nrow = 2, byrow = TRUE),
      shape    = guide_legend(nrow = 2, byrow = TRUE)
    )
}

pw_seq    <- oc_panel(filter(oc_long, metric == "Power"),
                      "Power", 0.80) + ggtitle("Power")  +
  ylim( ymin = 0.6, ymax = 1)

typeI_seq <- oc_panel(filter(oc_long, metric == "Type I error"),
                      "Type I error", 0.05) + ggtitle("Type I error") +
  ylim( ymin = 0.025, ymax = 0.07)

pw_seq
typeI_seq

# shared legend (bottom) ------------------------------------------------------
leg <- cowplot::get_legend(
  typeI_seq +
    theme(legend.position = "bottom") +
    guides(
      color    = guide_legend(order = 1, nrow = 1, byrow = TRUE),
      linetype = guide_legend(order = 2, nrow = 1, byrow = TRUE),
      shape    = guide_legend(order = 2, nrow = 1, byrow = TRUE)
    )
)

figure_oc <- cowplot::plot_grid(
  cowplot::plot_grid(pw_seq, typeI_seq, ncol = 1, labels = c("A", "B")),
  ggpubr::as_ggplot(leg),
  ncol = 1,
  rel_heights = c(1, 0.12)
)

figure_oc

ggsave("figures/figure_oc_seq.pdf", figure_oc, device = cairo_pdf,
       width = 11, height = 7, dpi = 600)
ggsave("figures/figure_oc_seq.svg", figure_oc, device = "svg",
       width = 11, height = 7, dpi = 600)
ggsave("figures/figure_oc_seq.png", figure_oc, device = "png",
       width = 11, height = 7, dpi = 600)
