# Sequential enrollment simulation function
# Mirrors my_simulation_continuous_function but simulates sequential arrival
# of patients from a pool of 100 subjects
#
# Key difference vs. pool-based function:
#   - All subjects arrive UNTREATED
#   - Greedy nearest-neighbour matching with caliper on PS
#   - Randomisation (T=0 / T=1) happens AFTER pairing
#   - Tracks n_matched, n_unmatched, prop_unmatched
#   - Runs same 3 analyses on formed pairs
#   - Also runs "equal" comparator on same 100-subject pool

library(MatchIt)
library(lme4)
library(afex)
library(randomizr)
library(dplyr)




# ── within-pair sign-flip permutation test ────────────────────────────────────
# d : differenze intra-coppia (trattato - controllo), una per coppia.
# Test esatto, design-based, della nulla forte sotto randomizzazione within-pair.
# Isola il proprio RNG: lo stream principale della simulazione resta intatto
# (i risultati parametrici esistenti restano identici al re-run).
perm_p_signflip <- function(d, B = 999, seed = NULL) {
  d <- d[is.finite(d)]
  n <- length(d)
  if (n < 2 || sd(d) == 0) return(NA_real_)

  tstat <- function(x) mean(x) / (sd(x) / sqrt(length(x)))
  obs   <- tstat(d)

  if (exists(".Random.seed", envir = .GlobalEnv)) {       # salva/ripristina seed
    old <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
  }
  if (!is.null(seed)) set.seed(seed)

  S  <- matrix(sample(c(-1, 1), n * B, replace = TRUE), nrow = n)  # n x B?: s is the sign
  D  <- S * d                     # randomly changes the sign of the difference
  m  <- colMeans(D)
  v  <- colSums((D - rep(m, each = n))^2) / (n - 1)
  tp <- m / sqrt(v / n)

  (1 + sum(abs(tp) >= abs(obs))) / (B + 1)   # p-value valido per ogni B
}



# Sequential enrollment simulation function
#
# Process:
#   1. Subjects arrive sequentially (random order) from a pool of N_pool
#   2. Each subject joins a single waiting queue
#   3. When a pair is formed (new arrival matches someone in queue within caliper)
#      -> both are randomised (T=0/T=1) and exit the queue
#   4. Enrolment STOPS as soon as n_target pairs are formed
#   5. Subjects still in waiting queue at stop = unmatched
#   6. Subjects not yet arrived from pool = simply not enrolled (ignored)
#
# sd_ps_hist: optional external SD of PS (e.g. from historical registry).
#             If NULL, SD is computed from the current pool.

library(MatchIt)
library(lme4)
library(afex)
library(randomizr)
library(dplyr)




my_simulation_sequential_function <- function(
    delta_effect,
    iter,
    n_target,                  # target number of pairs to form
    method,
    caliper_assigned = 0.05,
    N_pool           = 100,    # total subjects available in the pool
    sd_ps_hist       = NULL,    # optional historical SD of PS for caliper standardisation,
    B_perm=999 # number of permuations.
) {

  # ── helpers ────────────────────────────────────────────────────────────────

  compute_smd <- function(var, group) {
    mean_t  <- mean(var[group == 1], na.rm = TRUE)
    mean_c  <- mean(var[group == 0], na.rm = TRUE)
    sd_pool <- sqrt((sd(var[group == 1], na.rm = TRUE)^2 +
                       sd(var[group == 0], na.rm = TRUE)^2) / 2)
    abs(mean_t - mean_c) / sd_pool
  }

  covariates <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10","V16","V17")

  # ── greedy sequential matching ─────────────────────────────────────────────
  # Returns list:
  #   $matched    : data.frame of paired subjects (subclass, T_matched)
  #   $n_matched  : number of pairs formed (= n_target if pool large enough)
  #   $n_unmatched: subjects in waiting queue when enrolment stopped
  #   $n_arrived  : total subjects who had arrived before enrolment stopped

  sequential_match <- function(df, caliper_value, n_target, sd_ps_hist = NULL) {

    # caliper standardisation: use historical SD if provided, else pool SD
    sd_ps       <- if (!is.null(sd_ps_hist)) sd_ps_hist else sd(df$ps, na.rm = TRUE)
    caliper_raw <- caliper_value * sd_ps

    # random arrival order
    df <- df[sample(nrow(df)), ]

    waiting <- integer(0)
    pairs   <- list()
    pair_id <- 0L
    i_last  <- nrow(df)   # will track last subject processed

    for (i in seq_len(nrow(df))) {
      subj_ps <- df$ps[i]

      if (length(waiting) > 0) {
        dists    <- abs(df$ps[waiting] - subj_ps)
        best_pos <- which.min(dists)

        if (dists[best_pos] <= caliper_raw) {
          matched_row <- waiting[best_pos]
          pair_id <- pair_id + 1L
          pairs[[pair_id]] <- c(i, matched_row)
          waiting <- waiting[-best_pos]

          if (pair_id == n_target) {
            i_last <- i
            break
          }
        } else {
          waiting <- c(waiting, i)
        }
      } else {
        waiting <- c(waiting, i)
      }
    }

    if (pair_id == 0L) {
      return(list(
        matched     = NULL,
        n_matched   = 0L,
        n_unmatched = length(waiting),
        n_arrived   = nrow(df)   # exhausted whole pool
      ))
    }

    # build matched dataset with within-pair randomisation
    out_list <- vector("list", pair_id)
    for (p in seq_len(pair_id)) {
      r1 <- df[pairs[[p]][1], ]
      r2 <- df[pairs[[p]][2], ]
      r1$subclass  <- p
      r2$subclass  <- p
      assignment   <- sample(c(0L, 1L))
      r1$T_matched <- assignment[1]
      r2$T_matched <- assignment[2]
      out_list[[p]] <- rbind(r1, r2)
    }

    list(
      matched     = do.call(rbind, out_list),
      n_matched   = pair_id,
      n_unmatched = length(waiting),        # in queue when enrolment stopped
      n_arrived   = i_last  # paired + waiting (arrived but not yet matched)
    )
  }

  # ── main loop ──────────────────────────────────────────────────────────────

  final_output <- NULL

  for (sim in seq_len(iter)) {

    data_RCT1 <- data_continuous[[sim]]
    data_RCT1[, 2:11] <- lapply(data_RCT1[, 2:11], as.vector)

    list_mod <- list_mod_continuous_outcome
    methods  <- method

    for (meth in methods) {

      set.seed(123 * sim)

      # predict PS on full dataset
      if (meth == "sl_model") {
        x_test       <- data_RCT1 |> dplyr::select(V1:V17)
        temp         <- predict(list_mod[[meth]], type = "raw", x_test, onlySL = TRUE)
        data_RCT1$ps <- temp$pred |> as.vector()
      } else {
        data_RCT1$ps <- predict(list_mod[[meth]], type = "raw", newdata = data_RCT1)
      }

      # draw pool of N_pool subjects (all untreated at this stage)
      pool_idx <- sample(seq_len(nrow(data_RCT1)), size = N_pool, replace = FALSE)
      pool_df  <- data_RCT1[pool_idx, ]

      # ── loop over calipers ─────────────────────────────────────────────────
      final_output_1 <- NULL

      for (caliper_value in caliper_assigned) {

        res <- sequential_match(
          df           = pool_df,
          caliper_value = caliper_value,
          n_target     = n_target,
          sd_ps_hist   = sd_ps_hist
        )

        matched_df     <- res$matched
        n_matched      <- res$n_matched
        n_unmatched    <- res$n_unmatched
        n_arrived      <- res$n_arrived
        # proportion of arrived subjects who were left unmatched
        prop_unmatched <- if (n_arrived > 0) n_unmatched / n_arrived else NA_real_

        # SMD on matched sample
        if (!is.null(matched_df) && n_matched >= 2) {
          smd_matched_ps  <- compute_smd(matched_df$ps, matched_df$T_matched)
          smd_matched_cov <- mean(
            sapply(matched_df[covariates], compute_smd, group = matched_df$T_matched),
            na.rm = TRUE
          )
        } else {
          smd_matched_ps  <- NA_real_
          smd_matched_cov <- NA_real_
        }

        # ── analyses on matched pairs ────────────────────────────────────────
        output <- NULL

        for (delta in c(0, delta_effect)) {

          if (!is.null(matched_df) && n_matched >= 2) {

            db <- matched_df
            db$y_pre_T_scaled <- scale(db$y_pre_T_c) |> as.numeric()
            db$response_T_ml  <- db$y_pre_T_scaled + db$T_matched * delta

            g1 <- db |> filter(T_matched == 0) |> arrange(subclass)
            g2 <- db |> filter(T_matched == 1) |> arrange(subclass)

            # permuation
            d_pairs     <- g2$response_T_ml - g1$response_T_ml
            p_perm      <- perm_p_signflip(
              d_pairs, B = B_perm,
              seed = 123 * sim + match(caliper_value, caliper_assigned) * 101 + (delta > 0)
            )
            p_perm_sign <- p_perm < 0.05

            # paired t-test
            test      <- t.test(g2$response_T_ml, g1$response_T_ml, paired = TRUE)
            coef_ml   <- test$estimate
            sd_ml     <- test$stderr
            p_ml_sign <- test$p.value < 0.05

            # GLM + PS
            mod_glm    <- glm(response_T_ml ~ T_matched + ps, data = db)
            coef_glm   <- coef(mod_glm)[2]
            sd_glm     <- summary(mod_glm)$coefficients["T_matched", "Std. Error"]
            p_glm_sign <- summary(mod_glm)$coefficients["T_matched", 4] < 0.05

            # GLM + covariates
            mod_ml_lmer <- lmer(response_T_ml ~ T_matched + (1 | subclass), data = db)
            coef_T_ml_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Estimate"]
            sd_T_ml_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Std. Error"]
            p_ml_sign_lmer <- summary(mod_ml_lmer)$coefficients["T_matched", "Pr(>|t|)"] < 0.05


          } else {
            coef_ml <- sd_ml <- p_ml_sign <- NA
            coef_glm <- sd_glm <- p_glm_sign <- NA
            coef_lmer <- sd_lmer <- p_lmer_sign <- NA
            p_perm <- NA_real_; p_perm_sign <- NA
          }

          out <- data.frame(
            delta            = delta,
            sim              = sim,
            caliper          = caliper_value,
            smd_matched_ps   = smd_matched_ps,
            smd_matched_cov  = smd_matched_cov,
            method           = meth,
            N_pool           = N_pool,
            n_matched_pairs  = n_matched,
            n_unmatched      = n_unmatched,
            n_arrived        = n_arrived,
            prop_unmatched   = prop_unmatched,
            target_n         = n_target,
            p_sign_ttest     = p_ml_sign,
            coef_ttest       = coef_ml,
            sd_ttest         = sd_ml,
            p_sign_glm_adj   = p_glm_sign,
            coef_glm_adj     = coef_glm,
            sd_glm_adj       = sd_glm,
            p_sign_lmer      = p_ml_sign_lmer,
            coef_lmer        = coef_T_ml_lmer,
            sd_lmer          = sd_T_ml_lmer,
            p_sign_perm      = p_perm_sign,
            p_perm           = p_perm,
            sd_y0 = sd( db$ps ,na.rm=TRUE),
            model=method,
            stringsAsFactors = FALSE
          )
          output <- rbind(out, output)
        }

        final_output_1 <- rbind(final_output_1, output)

        # ── "equal" comparator: simple randomisation on the first n_target *2 subjects ──────
        # Use only subjects who had actually until the target n, even if not matched, not full pool,
        # and not only those that are matched.
        arrived_df             <- pool_df[seq_len(min(n_arrived, nrow(pool_df))), ] |> slice_head(n=n_target*2)
        arrived_df$T_equal_random <- complete_ra(N = nrow(arrived_df))
        output_equal <- NULL

        for (delta in c(0, delta_effect)) {

          db_eq <- arrived_df[]
          db_eq$y_pre_T_c_scaled <- scale(db_eq$y_pre_T_c) |> as.numeric()
          db_eq$response_T_equal <- db_eq$y_pre_T_c_scaled + db_eq$T_equal_random * delta

          mod_eq     <- glm(response_T_equal ~ T_equal_random, data = db_eq)
          coef_eq    <- coef(mod_eq)[2]
          sd_eq      <- summary(mod_eq)$coefficients["T_equal_random", "Std. Error"]
          p_eq_sign  <- summary(mod_eq)$coefficients["T_equal_random", 4] < 0.05

          mod_eq_adj    <- glm(response_T_equal ~ T_equal_random + ps, data = db_eq)
          coef_eq_adj   <- coef(mod_eq_adj)[2]
          sd_eq_adj     <- summary(mod_eq_adj)$coefficients["T_equal_random", "Std. Error"]
          p_eq_sign_adj <- summary(mod_eq_adj)$coefficients["T_equal_random", 4] < 0.05



          out_eq <- data.frame(
            delta            = delta,
            sim              = sim,
            caliper          = caliper_value,
            smd_matched_ps   = NA_real_,
            smd_matched_cov  = NA_real_,
            method           = "equal",
            N_pool           = N_pool,
            n_matched_pairs  = nrow(db_eq)/2,
            n_unmatched      = NA_integer_,
            n_arrived        = n_arrived,
            prop_unmatched   = NA_real_,
            target_n         = n_target,
            p_sign_ttest     = p_eq_sign,
            coef_ttest       = coef_eq,
            sd_ttest         = sd_eq,
            p_sign_glm_adj   = p_eq_sign_adj,
            coef_glm_adj     = coef_eq_adj,
            sd_glm_adj       = sd_eq_adj,
            p_sign_lmer = NA_real_, # glmer
            coef_lmer = NA_real_,
            sd_lmer = NA_real_,
            p_sign_perm      = NA,
            p_perm           = NA_real_,
            sd_y0 = sd( db_eq$ps ,na.rm=TRUE),
            model=method,
            stringsAsFactors = FALSE
          )
          output_equal <- rbind(out_eq, output_equal)
        }

        final_output_1 <- rbind(final_output_1, output_equal)
      }

      final_output <- rbind(final_output, final_output_1)
    }
  }

  return(list(final_output = final_output))
}

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


# ── Summary function ──────────────────────────────────────────────────────────

summarize_results_sequential <- function(result_obj, iteractions = 10000) {

  result_obj$final_output |>
    as.data.frame() |>
    group_by(caliper, method, model, delta) |>
    summarize(
      N_pool               = mean(N_pool, na.rm = TRUE),
      target_n             = mean(target_n, na.rm = TRUE),
      sd_y0 = mean(sd_y0, na.rm=TRUE),

      # --- sample size metrics ---
      # subjects actually randomised in the trial (2 per pair)
      sample_size_trial    = mean(2 * n_matched_pairs, na.rm = TRUE),
      # subjects who had arrived before enrolment stopped (trial + waiting list)
      sample_size_total    = mean(2 * n_matched_pairs + n_unmatched, na.rm = TRUE),
      # subjects left in waiting list when enrolment stopped (= excluded)
      n_excluded           = mean(n_unmatched, na.rm = TRUE),

      n_matched_pairs_mean = mean(n_matched_pairs, na.rm = TRUE),
      n_unmatched_mean     = mean(n_unmatched, na.rm = TRUE),
      n_arrived_mean       = mean(n_arrived, na.rm = TRUE),
      prop_unmatched_mean  = mean(prop_unmatched, na.rm = TRUE),

      smd_matched_ps_mean  = mean(smd_matched_ps, na.rm = TRUE),
      smd_matched_cov_mean = mean(smd_matched_cov, na.rm = TRUE),

      power_ttest          = mean(p_sign_ttest, na.rm = TRUE),
      mcse_power_ttest = mcse_prop(p_sign_ttest),
      power_perm = mean(p_sign_perm, na.rm = TRUE),
      mcse_power_perm = mcse_prop(p_sign_perm),
      # mcse_power_perm=NA_real_,
      # power_perm = NA,
      sd_ttest             = mean(sd_ttest, na.rm = TRUE),


      power_glm_adj        = mean(p_sign_glm_adj, na.rm = TRUE),
      mcse_power_glm_adj = mcse_prop(p_sign_glm_adj),
      sd_glm_adj           = mean(sd_glm_adj, na.rm = TRUE),

      power_lmer           = mean(p_sign_lmer, na.rm = TRUE),
      mcse_power_lmer = mcse_prop(p_sign_lmer),
      sd_lmer              = mean(sd_lmer, na.rm = TRUE),

      model=unique(model),
      n_sim=n(),

      .groups = "drop"
    )
}
