# My function Continuous -- VALIDATION variant ---------------- (based on continuous_function_paired_lmer.R)
#
# Same structure as the original paired_lmer function. Additions are marked # >>> NEW.
#
# Oracle quantities for validating eqs. (4)-(13), computed once per (sim, method, caliper)
# on the sampled paired set:
#   - rho_wp_pearson      : within-pair correlation on Y0  -> eq.(8) view
#   - rho_wp_vardecomp    : same rho via sigma_b^2/(sigma_b^2+sigma_w^2) -> eq.(5) view
#   - var_y0, sigma2_b, sigma2_w
#   - mean_wp_ps_dist     : matching tightness on the estimated score (ps)
#   - var_s, r2_trial, rho_s, cor_s_pshat : S-dependent
#   - r2_hist             : passthrough scalar (registry-side XGBoost CV R^2)
#
# >>> NEW in this version: the TRUE score S is computed INSIDE the function via
# compute_true_score() from the full covariate set V1..V17 (so the trial pool must
# keep the unmeasured V11..V15). Priority: explicit stored column (true_score_col)
# > a column named "S" already present > computed on the fly. If V11..V15 are absent
# the S-block stays NA (we never fabricate S).
#
# Validation logic:
#   eq.(8):  rho = Cov(Yi0,Yj0)/Var(Y0)
#   eq.(9):  R2_trial = Var(S)/Var(Y0)
#   eq.(11): rho ~ lambda * R2_hist            => lambda = rho_wp / r2_hist
#   decomposition: rho_wp = rho_s * R2_trial   => lambda = rho_s * (R2_trial / R2_hist)

library(MatchIt)
library(lme4)
library(afex)
library(randomizr)
library(dplyr)   # >>> NEW (filter/arrange/select used explicitly here)

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


# >>> NEW: true prognostic score S = E[Y0 | X], deterministic part of the trial DGP
# (03_RCT_dataset_generation.R, lines 44-71) WITHOUT the rnorm noise term.
# Indices z[, k] map to Vk; pass z = data[, paste0("V", 1:17)] so positional == nominal.
compute_true_score <- function(z, b, b2) {
  z <- as.data.frame(z)
  S <- b[1]  * z[, 1] +
    b[2]  * (z[, 2]^2) +
    b[3]  * sqrt(abs(z[, 3])) +
    b[4]  * z[, 4] +
    b[5]  * (z[, 5] * log(z[, 8])) +
    b[6]  * log(z[, 6]) +
    b[7]  * (z[, 7]^2) +
    b[8]  * z[, 8] +
    # b[9]  * (sin(z[, 9]) / (cos(1.5 * z[, 13]) + 1) * z[, 11]) +   # inactive in DGP
    b[10] * z[, 10] +
    b[11] * z[, 11] +
    b[12] * z[, 12] +
    b[13] * z[, 13] +
    b[14] * (z[, 14]^2) +
    b[15] * z[, 15] +
    b2[1] * (z[, 16]) +
    b2[2] * (z[, 17]) +
    b[4]  * (z[, 17] * z[, 7]) +
    # b2[1] * sin(2 * pi * z[, 1] / 110) +                          # inactive in DGP
    b2[2] * (z[, 6] / (z[, 9] + 1))
  as.numeric(S)
}


my_simulation_continuous_function <- function(delta_effect, N, iter, n, method,
                                              caliper_assigned = caliper_assigned,
                                              score_col = "score",   # >>> NEW: explicit stored-score column (override)
                                              r2_hist = NA_real_,      # >>> NEW: registry XGBoost CV R^2 (scalar passthrough)
                                              b  = c(0.38, 0.1, 0.2, 0.06, 0.4, -0.15, 0.09, 0.15, 1.6, 0.9,   # >>> NEW: trial DGP coefficients
                                                     -0.38, 0.8, -0.24, 0.5, 0.18),
                                              b2 = c(3.6, 2.5)) {                                                # >>> NEW
  # N: is the size of generated patient
  # n is the number of paires selected
  final_output <- data.frame(
    sim = integer(),
    method = character(),
    initial_size = integer(),
    reduced_size = integer(),

    # ttest
    p_sign_ttest = logical(),
    coef_ttest = numeric(),
    sd_ttest = numeric(),

    # glm adj
    p_sign_glm_adj = logical(),
    coef_glm_adj = numeric(),
    sd_glm_adj = numeric(),

    # lmer subclass
    p_sign_lmer = logical(),
    coef_lmer = numeric(),
    sd_lmer = numeric(),

    # >>> NEW: oracle validation quantities
    var_y0 = numeric(),
    rho_wp_pearson = numeric(),
    rho_wp_vardecomp = numeric(),
    sigma2_b = numeric(),
    sigma2_w = numeric(),
    mean_wp_ps_dist = numeric(),
    var_s = numeric(),
    r2_trial = numeric(),
    rho_s = numeric(),
    cor_s_pshat = numeric(),
    r2_hist = numeric(),
    stringsAsFactors = FALSE
  )

  final_output_1 <- NULL
  out <- output <- final_output_1 <- final_output <- NULL

  compute_smd <- function(var, treated, control) {
    mean_treated <- mean(var[treated == 1], na.rm = TRUE)
    mean_control <- mean(var[treated == 0], na.rm = TRUE)
    sd_pooled <- sqrt((sd(var[treated == 1], na.rm = TRUE)^2 + sd(var[treated == 0], na.rm = TRUE)^2) / 2)
    return(abs(mean_treated - mean_control) / sd_pooled)
  }

  # Apply function to all covariates
  covariates <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V16", "V17")

  for (sim in seq_along(1:iter)) {
    data_RCT1 <- data_continuous2[[sim]]

    list_mod <- list_mod_continuous_outcome

    methods <- method
    data_RCT1[, 2:11] <- lapply(data_RCT1[, 2:11], as.vector)



    for (method in methods) {
      set.seed(123 * sim)
      if (method == "sl_model") {
        x_test <- data_RCT1 |> dplyr::select(V1:V17)
        temp <- predict(list_mod[[method]], type = "raw", x_test, onlySL = TRUE)
        data_RCT1$ps <- temp$pred |> as.vector()
      } else {
        data_RCT1$ps <- predict(list_mod[[method]], type = "raw", newdata = data_RCT1)
      }

      final_output_1 <- NULL
      for (caliper_value in caliper_assigned) {
        obj_m <- matchit(T_equal ~ ps, data = data_RCT1, method = "nearest", standardize = TRUE, distance = data_RCT1$ps, caliper = caliper_value)
        caliper_standardized <- obj_m$caliper
        smd_matched_ps <- as.data.frame((summary(obj_m)$sum.matched))$`Std. Mean Diff.`[1]

        m_dataRCT <- match.data(obj_m, data = data_RCT1)
        m_dataRCT <- m_dataRCT |> filter(subclass %in% sample(1:length(unique(m_dataRCT$subclass)), size = n, replace = F))
        m_dataRCT$T_matched <- m_dataRCT$T_equal
        db_merged <- right_join(data_RCT1, m_dataRCT[, c("id", "subclass", "T_matched")], by = "id")
        smd_matched_cov <- mean(sapply(db_merged[covariates], compute_smd, treated = db_merged$T_matched, control = db_merged$T_matched), na.rm = T)

        # ====================================================================
        # >>> NEW: ORACLE QUANTITIES FOR FORMULA VALIDATION
        # Computed once per (sim, method, caliper) on the SAMPLED paired set.
        # Delta-invariant: use y_pre_T_c (Y0) and ps, neither depends on delta.
        # ====================================================================
        g0_or <- db_merged |> filter(T_matched == 0) |> arrange(subclass)
        g1_or <- db_merged |> filter(T_matched == 1) |> arrange(subclass)

        # Y0 = control potential outcome for BOTH pair members (oracle: only in simulation)
        y0_i <- g0_or$y_pre_T_c
        y0_j <- g1_or$y_pre_T_c

        # (A) eq.(8) view: within-pair correlation on Y0 (Pearson across pair members)
        rho_wp_pearson <- cor(y0_i, y0_j)

        # sigma^2 = outcome variance on the paired set (raw scale; rho and R2 are scale-free)
        var_y0 <- var(db_merged$y_pre_T_c)

        # (B) eq.(5) view: rho = sigma_b^2 / (sigma_b^2 + sigma_w^2)
        # ANOVA variance-component estimators for pairs (group size 2), effect-free (Y0 only).
        diff_ij  <- y0_i - y0_j
        mean_ij  <- (y0_i + y0_j) / 2
        msw      <- mean(diff_ij^2) / 2          # within-pair mean square  -> sigma_w^2
        msb      <- 2 * var(mean_ij)             # between-pair mean square -> E = sigma_w^2 + 2 sigma_b^2
        sigma2_w <- msw
        sigma2_b <- (msb - msw) / 2
        rho_wp_vardecomp <- sigma2_b / (sigma2_b + sigma2_w)

        # (C) matching tightness on the estimated score (mean within-pair |delta ps|)
        mean_wp_ps_dist <- mean(abs(g0_or$ps - g1_or$ps))

        # (D) S-dependent quantities. Resolve the score column: explicit override,
        # else the computed/stored "S"; otherwise NA (we never fabricate S).
         if (!is.null(score_col)) {
          s_i         <- g0_or[[score_col]]
          s_j         <- g1_or[[score_col]]
          var_s       <- var(db_merged[[score_col]])
          r2_trial    <- var_s / var_y0                          # eq.(9)
          rho_s       <- cor(s_i, s_j)                           # within-pair corr of the TRUE score
          cor_s_pshat <- cor(db_merged[[score_col]], db_merged$ps)  # Siegfried estimation quality (NOT rho_wp)
        } else {
          var_s <- r2_trial <- rho_s <- cor_s_pshat <- NA_real_
        }
        # ====================================================================

        output <- final_output_ml <- NULL
        for (delta in (c(0, delta_effect))) {
          db_merged$y_pre_T_scaled <- db_merged$y_pre_T_c |>
            scale() |>
            as.numeric()
          db_merged$response_T_ml <- db_merged$y_pre_T_scaled + db_merged$T_matched * delta

          g1 <- db_merged |>
            filter(T_matched == 0) |>
            arrange(subclass)
          g2 <- db_merged |>
            filter(T_matched == 1) |>
            arrange(subclass)

          # paired t-test  (analysis used for the formula validation)
          test <- t.test(g2$response_T_ml, g1$response_T_ml, data = db_merged, paired = TRUE)
          coef_ml <- test$estimate
          sd_ml <- test$stderr
          p_ml_sign <- test$p.value < 0.05

          # glm adj
          mod_ml_glm <- glm(response_T_ml ~ T_matched + ps, data = db_merged)
          coef_T_ml_glm <- coef(mod_ml_glm)[2]
          sd_T_ml_glm <- summary(mod_ml_glm)$coefficients["T_matched", "Std. Error"]
          p_ml_sign_glm <- summary(mod_ml_glm)$coefficients["T_matched", 4] < 0.05

          # lmer (pair as random intercept)
          mod_ml_lmer <- lmer(response_T_ml ~ T_matched + (1 | subclass), data = db_merged)
          coef_T_ml_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Estimate"]
          sd_T_ml_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Std. Error"]
          p_ml_sign_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Pr(>|t|)"] < 0.05

          size <- nrow(na.omit(db_merged))

          out <- data.frame(
            delta = delta,
            sim = sim,
            caliper = caliper_value,
            caliper_distance = caliper_standardized,
            smd_matched_ps = smd_matched_ps,
            smd_matched_cov = smd_matched_cov,
            method = method,
            initial_size = n,
            reduced_size = size,

            # ttest
            p_sign_ttest = p_ml_sign,
            coef_ttest = coef_ml,
            sd_ttest = sd_ml,

            # glm adj
            p_sign_glm_adj = p_ml_sign_glm,
            coef_glm_adj = coef_T_ml_glm,
            sd_glm_adj = sd_T_ml_glm,

            # lmer subclass
            p_sign_lmer = p_ml_sign_lmer,
            coef_lmer = coef_T_ml_lmer,
            sd_lmer = sd_T_ml_lmer,

            # >>> NEW: oracle quantities (identical across the two delta rows)
            var_y0 = var_y0,
            rho_wp_pearson = rho_wp_pearson,
            rho_wp_vardecomp = rho_wp_vardecomp,
            sigma2_b = sigma2_b,
            sigma2_w = sigma2_w,
            mean_wp_ps_dist = mean_wp_ps_dist,
            var_s = var_s,
            r2_trial = r2_trial,
            rho_s = rho_s,
            cor_s_pshat = cor_s_pshat,
            r2_hist = r2_hist,
            stringsAsFactors = FALSE
          )

          output <- rbind(out, output)
        }

        final_output_1 <- rbind(final_output_1, output)

        output_equal <- out_equal <- NULL
        # randomization WITHOUT pairing, on the same matched patients (unpaired reference -> unpaired SE for rho_eff)
        db_merged$T_equal_random <- complete_ra(N = nrow(db_merged))
        table(db_merged$T_equal_random, db_merged$T_matched)
        smd_matched_cov <- mean(sapply(db_merged[covariates], compute_smd, treated = db_merged$T_equal_random, control = db_merged$T_equal_random), na.rm = T)
        smd_matched_ps <- mean(sapply(db_merged["ps"], compute_smd, treated = db_merged$T_equal_random, control = db_merged$T_equal_random), na.rm = T)

        for (delta in c(0, delta_effect)) {
          db_merged$y_pre_T_c_scaled <- db_merged$y_pre_T_c |>
            scale() |>
            as.numeric()
          db_merged$response_T_equal <- db_merged$y_pre_T_c_scaled + db_merged$T_equal_random * delta

          # equal ttest
          mod_equal <- glm(response_T_equal ~ T_equal_random, data = db_merged, family = "gaussian")
          coef_T_equal <- coef(mod_equal)[2]
          sd_T_equal <- summary(mod_equal)$coefficients["T_equal_random", "Std. Error"]
          p_equal_sign <- summary(mod_equal)$coefficients["T_equal_random", 4] < 0.05

          # equal adj
          mod_equal_adj <- glm(response_T_equal ~ T_equal_random + ps, data = db_merged, family = "gaussian")
          coef_T_equal_adj <- coef(mod_equal_adj)[2]
          sd_T_equal_adj <- summary(mod_equal_adj)$coefficients["T_equal_random", "Std. Error"]
          p_equal_sign_adj <- summary(mod_equal_adj)$coefficients["T_equal_random", 4] < 0.05

          mod_equal_lmer <- glm(response_T_equal ~ T_equal_random+V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V16+V17 , data = db_merged)
          coef_T_equal_lmer <-  coef(mod_equal_lmer)[2]
          sd_T_equal_lmer <- summary(mod_equal_lmer)$coefficients["T_equal_random", "Std. Error"]
          p_equal_sign_lmer <- summary(mod_equal_lmer)$coefficients["T_equal_random", 4] < 0.05

          size <- nrow(data_RCT1)
          out_equal <- data.frame(
            delta = delta,
            sim = sim,
            caliper = caliper_value,
            caliper_distance = caliper_standardized,
            smd_matched_ps = smd_matched_ps,
            smd_matched_cov = smd_matched_cov,
            method = "equal",
            initial_size = n,
            reduced_size = size,

            # ttest
            p_sign_ttest = p_equal_sign,
            coef_ttest = coef_T_equal,
            sd_ttest = sd_T_equal,

            # glm adj
            p_sign_glm_adj = p_equal_sign_adj,
            coef_glm_adj = coef_T_equal_adj,
            sd_glm_adj = sd_T_equal_adj,

            # lmer subclass
            p_sign_lmer = p_equal_sign_lmer,
            coef_lmer = coef_T_equal_lmer,
            sd_lmer = sd_T_equal_lmer,

            # >>> NEW: oracle columns kept for schema consistency.
            # Pair-structure quantities are NA for the unpaired "equal" rows;
            # cohort-level quantities (var_y0, var_s, r2_trial, cor_s_pshat, r2_hist) carried as-is.
            var_y0 = var_y0,
            rho_wp_pearson = NA_real_,
            rho_wp_vardecomp = NA_real_,
            sigma2_b = NA_real_,
            sigma2_w = NA_real_,
            mean_wp_ps_dist = NA_real_,
            var_s = var_s,
            r2_trial = r2_trial,
            rho_s = NA_real_,
            cor_s_pshat = cor_s_pshat,
            r2_hist = r2_hist,
            stringsAsFactors = FALSE
          )
          output_equal <- rbind(out_equal, output_equal)
        }

        final_output_1 <- rbind(final_output_1, output_equal)
      }

      final_output <- rbind(final_output, final_output_1)
    }
  }

  return(list(final_output = final_output, dataset = db_merged))
}


# funzione da usare in 05 per i summary dei vari modelli
summarize_results <- function(power_levels, sample_size_to_try, iteractions = 1000) {

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

        # >>> NEW: oracle validation quantities (delta-invariant; mean over iterations + MC SE on rho)
        var_y0_mean            = mean(var_y0, na.rm = TRUE),
        rho_wp_pearson_mean    = mean(rho_wp_pearson, na.rm = TRUE),
        rho_wp_pearson_se      = sd(rho_wp_pearson, na.rm = TRUE) / sqrt(iteractions),
        rho_wp_vardecomp_mean  = mean(rho_wp_vardecomp, na.rm = TRUE),
        sigma2_b_mean          = mean(sigma2_b, na.rm = TRUE),
        sigma2_w_mean          = mean(sigma2_w, na.rm = TRUE),
        mean_wp_ps_dist_mean   = mean(mean_wp_ps_dist, na.rm = TRUE),
        var_s_mean             = mean(var_s, na.rm = TRUE),
        r2_trial_mean          = mean(r2_trial, na.rm = TRUE),
        rho_s_mean             = mean(rho_s, na.rm = TRUE),
        cor_s_pshat_mean       = mean(cor_s_pshat, na.rm = TRUE),
        r2_hist                = mean(r2_hist, na.rm = TRUE),
        .groups = "drop"
      ) |>
      mutate(sample_size = sample_size_to_try[i])
  }

  bind_rows(power_levels_temp)
}


# >>> NEW: dedicated validation summary -- two legs anchored on the paired t-test
#   leg 1 (calibration): predicted power (eq.13) vs empirical power
#   leg 2 (attenuation): lambda = rho_wp / R2_hist, and decomposition rho_s * (R2_trial / R2_hist)
summarize_validation <- function(summary_df, alpha = 0.05) {
  z_a <- qnorm(1 - alpha / 2)
  summary_df |>
    dplyr::filter(method != "equal", delta != 0) |>      # paired set, effect scenario
    dplyr::transmute(
      sample_size, caliper, method, delta,
      npairs              = initial_size_mean,
      r2_hist,
      r2_trial            = r2_trial_mean,
      rho_wp              = rho_wp_pearson_mean,          # eq.(8) oracle
      rho_wp_se           = rho_wp_pearson_se,
      rho_wp_vardecomp    = rho_wp_vardecomp_mean,        # eq.(5) cross-check (should match rho_wp)
      rho_s               = rho_s_mean,                   # within-pair corr of TRUE score
      lambda_emp          = rho_wp_pearson_mean / r2_hist,
      lambda_decomp       = rho_s_mean * (r2_trial_mean / r2_hist),
      power_pred          = 1 - pnorm(z_a - delta * sqrt(initial_size_mean / (2 * (1 - rho_wp_pearson_mean)))),
      power_emp           = power_ttest,
      power_gap           = power_ttest - (1 - pnorm(z_a - delta * sqrt(initial_size_mean / (2 * (1 - rho_wp_pearson_mean)))))
    )
}


# funzione per dividere i risultati (UNCHANGED)
split_results_tables <- function(results_df) {

  results_operating <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  power_ttest, power_glm_adj, power_lmer,
                  sd_ttest, sd_glm_adj, sd_lmer)

  results_balance <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  caliper_dist, smd_matched_ps_mean, smd_matched_cov_mean) %>%
    dplyr::filter(delta == 0.2)

  results_bias <- results_df %>%
    dplyr::select(sample_size, caliper, method, delta,
                  coef_ttest, bias_ttest, mse_ttest, se_bias_ttest,
                  coef_glm_adj, bias_glm_adj, mse_glm_adj, se_bias_glm_adj,
                  coef_lmer, bias_lmer, mse_lmer, se_bias_lmer) %>%
    dplyr::filter(delta == 0.2)

  list(
    operating = results_operating,
    balance   = results_balance,
    bias      = results_bias
  )
}


# function for final tables (UNCHANGED except the regex fix)
library(dplyr)
library(tidyr)

make_tbl_caliper <- function(data, caliper_value) {
  data %>%
    filter(caliper == caliper_value) %>%
    pivot_longer(
      cols = c(power_ttest, power_glm_adj, power_lmer,
               sd_ttest, sd_glm_adj, sd_lmer),
      names_to = c(".value", "analysis"),
      names_pattern = "(power|sd)_(.*)"          # >>> NEW: non-greedy fix (was "(.*)_(.*)")
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
