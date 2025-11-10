# ----------------------------------------------------------------------------------------------
# Monte Carlo Simulation for Prognostic Score–Based Paired Randomization in Continuous Outcomes
# ----------------------------------------------------------------------------------------------

# This function performs repeated simulations of clinical trial randomization using prognostic scores estimated from machine learning models. 
# It evaluates the operating characteristics (power, type I error, bias, and variance) of paired randomization compared to simple randomization across different calipers and model types.Each iteration generates outcomes for treated and control pairs based on a pre-specified effect size, applies matching on the prognostic score, and fits statistical models (paired t-test, GLM with adjustment, LMM with random intercept for pairs). The function returns individual simulation results and the matched dataset for further analysis.
#-------------------------------------------------------------------------------------------------------------------------


packages_required<- c("MatchIt", "lme4", "afex", "randomizr", "dplyr")

for (pkg in packages_required) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}


my_simulation_continuous_function <- function(delta_effect, N, iter, n, method, caliper_assigned = caliper_assigned) {
  # N: is the size of generated patient
  # n is the number of paires selected
  final_output <- data.frame(
    sim = integer(),
    method = character(),
    initial_size = integer(),
    reduced_size = integer(),

    # ttest
    p_sign_ttest = logical(), # t test
    coef_ttest = numeric(),
    sd_ttest = numeric(),

    # glm adj
    p_sign_glm_adj = logical(), # glm adj
    coef_glm_adj = numeric(),
    sd_glm_adj = numeric(),

    # lmer subclass
    p_sign_lmer = logical(), # glmer
    coef_lmer = numeric(),
    sd_lmer = numeric(),
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



  # sim=1;N=8000;delta=.2,
  for (sim in seq_along(1:iter)) {
    # data from out
    data_RCT1 <- data_continuous[[sim]]

    list_mod <- list_mod_continuous_outcome
    # method="xgboost_model"

    # input of the function to not loop over all methods.
    methods <- method
    data_RCT1[, 2:11] <- lapply(data_RCT1[, 2:11], as.vector)
    # method <- c("xgboost_model")

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

          # paired t-test
          test <- t.test(g2$response_T_ml, g1$response_T_ml, data = db_merged, paired = TRUE)
          coef_ml <- test$estimate
          sd_ml <- test$stderr
          p_ml_sign <- test$p.value < 0.05

          # glm
          mod_ml_glm <- glm(response_T_ml ~ T_matched + ps, data = db_merged)
          coef_T_ml_glm <- coef(mod_ml_glm)[2]
          sd_T_ml_glm <- summary(mod_ml_glm)$coefficients["T_matched", "Std. Error"]
          p_ml_sign_glm <- summary(mod_ml_glm)$coefficients["T_matched", 4] < 0.05

          # glmer

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
            p_sign_ttest = p_ml_sign, # paired t test
            coef_ttest = coef_ml,
            sd_ttest = sd_ml,

            # glm adj
            p_sign_glm_adj = p_ml_sign_glm, # glm adj
            coef_glm_adj = coef_T_ml_glm,
            sd_glm_adj = sd_T_ml_glm,

            # lmer subclass
            p_sign_lmer = p_ml_sign_lmer, # glmer
            coef_lmer = coef_T_ml_lmer,
            sd_lmer = sd_T_ml_lmer,
            stringsAsFactors = FALSE
          )

          output <- rbind(out, output)
        }



        final_output_1 <- rbind(final_output_1, output)




        output_equal <- out_equal <- NULL
        # randomization without considering pairing on the same paired patients
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

          # equal
          mod_equal_adj <- glm(response_T_equal ~ T_equal_random + ps, data = db_merged, family = "gaussian")
          coef_T_equal_adj <- coef(mod_equal_adj)[2]
          sd_T_equal_adj <- summary(mod_equal_adj)$coefficients["T_equal_random", "Std. Error"]
          p_equal_sign_adj <- summary(mod_equal_adj)$coefficients["T_equal_random", 4] < 0.05

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
            p_sign_ttest = p_equal_sign, # t test
            coef_ttest = coef_T_equal,
            sd_ttest = sd_T_equal,

            # glm adj
            p_sign_glm_adj = p_equal_sign_adj, # glm adj
            coef_glm_adj = coef_T_equal_adj,
            sd_glm_adj = sd_T_equal_adj,

            # lmer subclass
            p_sign_lmer = NA, # glmer
            coef_lmer = NA,
            sd_lmer = NA,
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



#