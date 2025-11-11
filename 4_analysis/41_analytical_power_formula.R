# =============================================================
# Title: Power Calculations for Paired Randomization
# Description: Functions and examples to compute power for paired designs accounting for correlation (rho) and prognostic score R².
# =============================================================

# Load required package
if (!requireNamespace("pwr", quietly = TRUE)) install.packages("pwr")
library(pwr)

# -----------------------------
# Basic power calculation
# -----------------------------
# Two-sample unpaired t-test
power.t.test(
  n = 300,        # sample size per group
  delta = 0.2,    # expected mean difference
  sd = 1,         # standard deviation
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

# Compute required sample size for 80% power (unpaired)
power.t.test(
  power = 0.8,
  delta = 0.2,
  sd = 1,
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)

# Paired t-test
power.t.test(
  n = 300,
  delta = 0.2,
  sd = 1,
  sig.level = 0.05,
  type = "paired",
  alternative = "two.sided"
)

# -----------------------------
# Paired t-test adjustment for correlation rho
# -----------------------------
pwr.t.paired.rho <- function(n = NULL, delta, sigma = 1, rho = 0,
                             sig.level = 0.05, power = NULL,
                             alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  if (is.null(delta)) stop("Specify expected mean difference 'delta'.")
  
  # Standard deviation of the difference
  sd_diff <- sqrt(2 * sigma^2 * (1 - rho))
  
  # Standardized effect size
  d_paired <- delta / sd_diff
  
  res <- pwr::pwr.t.test(
    n = n,
    d = d_paired,
    sig.level = sig.level,
    power = power,
    type = "paired",
    alternative = alternative
  )
  
  # Annotate output
  res$rho <- rho
  res$sd_diff <- sd_diff
  res$delta <- delta
  res$d_paired <- d_paired
  res$note <- paste0("n = number of pairs; correlation rho = ", rho)
  res$method <- "Paired t-test adjusted for correlation rho"
  return(res)
}

# Example usage
pwr.t.paired.rho(delta = 0.2, sigma = 1, rho = 0.3, sig.level = 0.05, n = 300)

# -----------------------------
# Paired t-test adjustment for correlation and prognostic R²
# -----------------------------
pwr.t.paired.rhoR2 <- function(n = NULL, delta, sigma = 1, rho = 0, R2 = 0,
                               sig.level = 0.05, power = NULL,
                               alternative = c("two.sided", "less", "greater")) {
  
  alternative <- match.arg(alternative)
  if (is.null(delta)) stop("Specify expected mean difference 'delta'.")
  
  # Residual SD accounting for correlation and prognostic R²
  sd_diff <- sqrt(2 * sigma^2 * (1 - R2) * (1 - rho))
  
  # Standardized effect size
  d_adj <- delta / sd_diff
  
  res <- pwr::pwr.t.test(
    n = n,
    d = d_adj,
    sig.level = sig.level,
    power = power,
    type = "paired",
    alternative = alternative
  )
  
  # Annotate output
  res$rho <- rho
  res$R2 <- R2
  res$sd_diff <- sd_diff
  res$delta <- delta
  res$d_adj <- d_adj
  res$note <- paste0("n = number of pairs; rho = ", rho, ", R2 = ", R2)
  res$method <- "Paired t-test adjusted for correlation and prognostic R²"
  return(res)
}

# Example usage
pwr.t.paired.rhoR2(delta = 0.2, sigma = 1, rho = 0.2, R2 = 0.3, n = 300)

# -----------------------------
# Custom analytical power function
# -----------------------------
power_paired <- function(n_pair, d, sigma = 1, alpha = 0.05, R2 = 0, k = 0.3) {
  z_alpha <- qnorm(1 - alpha/2)
  var_diff <- 2 * sigma^2 * (1 - k * R2)
  z_effect <- d * sqrt(n_pair) / sqrt(var_diff)
  power <- 1 - pnorm(z_alpha - z_effect)
  return(power)
}

# Example usage
power_paired(n_pair = 49, d = 0.5, sigma = 1, alpha = 0.05, R2 = 0.851, k = 0.28)
