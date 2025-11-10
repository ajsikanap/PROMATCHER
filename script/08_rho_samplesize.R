load("data/data_registry.rda")

s1<-dt$response_cont |> sample(200)
s2<-dt$response_cont |> sample(200)

cor(s1,s2)

temp<-power_levels[[1]]$dataset
tt<-temp |> arrange(subclass) |> filter(T_matched==1)
tc<-temp |> arrange(subclass) |> filter(T_matched==0)

cor(tt$y_pre_T_c, tc$y_pre_T_c)
#0.43

#with correlation
((1.96+0.84)/0.2)^2*((1-0.4))*2

#without correlation
((1.96+0.84)/0.2)^2*2


#install.packages("pwr"
library(pwr)
power.t.test(
  n = 300,
  delta = 0.2,       # differenza attesa tra le medie
  sd = 1,          # deviazione standard
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)
#pw 68% con 300 pairs

power.t.test(
  #n = 300,
  power =0.8,
  delta = 0.2,       # differenza attesa tra le medie
  sd = 1,          # deviazione standard
  sig.level = 0.05,
  type = "two.sample",
  alternative = "two.sided"
)
# 393 pairs senza accoppiamento per pw 80%

# con accoppiamento
power.t.test(
  n = 300,
  #power =0.8,
  delta = 0.2,       # differenza attesa tra le medie
  sd = 1,          # deviazione standard
  sig.level = 0.05,
  type = "paired",
  alternative = "two.sided"
)

?power.t.test()
#N=393
#
# Effetto corretto per correlazione
rho=0.6
effect_size=0.2
alpha=0.05
#rho=0
d_paired <- effect_size /sqrt(2 * (1 - rho))

pwr.t.test(
  d = d_paired,
  sig.level = alpha,
  type = "two.sample",
  alternative = "two.sided",
  n=300
  #power=0.8
)

#------------------------------------
pwr.t.paired.rho <- function(n = NULL, delta = NULL, sigma = 1, rho = 0,
sig.level = 0.05, power = NULL,
alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  if (is.null(delta))
    stop("Please specify the expected mean difference 'delta'.")

  # sd della differenza (var(Y1 - Y2) = 2 * sigma^2 * (1 - rho))
  sd_diff <- sqrt(2 * sigma^2 * (1 - rho))

  # effect size standardizzato per paired t-test
  d_paired <- delta / sd_diff

  # usa pwr.t.test nativo
  res <- pwr::pwr.t.test(
    n = n,
    d = d_paired,
    sig.level = sig.level,
    power = power,
    type = "paired",
    alternative = alternative
  )

  # aggiungi annotazioni per chiarezza
  res$rho <- rho
  res$sd_diff <- sd_diff
  res$delta <- delta
  res$d_paired <- d_paired
  res$note <- paste0("n = number of pairs; correlation rho = ", rho)
  res$method <- "Paired t-test (adjusted for correlation rho)"
  return(res)
}

library(pwr)

pwr.t.paired.rho(
  delta = 0.2,   # differenza media attesa
  sigma = 1,     # deviazione standard individuale
  rho = 0,     # correlazione tra coppie
  sig.level = 0.05,
  power = 0.8
)

pwr.t.paired.rho(
  delta = 0.2,   # differenza media attesa
  sigma = 1,     # deviazione standard individuale
  rho = 0.3,     # correlazione tra coppie
  sig.level = 0.05,
  #power = 0.8,
  n=300
)



pwr.t.paired.rhoR2 <- function(n = NULL, delta = NULL, sigma = 1, rho = 0, R2 = 0,
                               sig.level = 0.05, power = NULL,
                               alternative = c("two.sided", "less", "greater")) {
  alternative <- match.arg(alternative)

  if (is.null(delta))
    stop("Please specify the expected mean difference 'delta'.")

  # sd residua della differenza
  sd_diff <- sqrt(2 * sigma^2 * (1 - R2) * (1 - rho))

  # effect size standardizzato
  d_adj <- delta / sd_diff

  res <- pwr::pwr.t.test(
    n = n,
    d = d_adj,
    sig.level = sig.level,
    power = power,
    type = "paired",
    alternative = alternative
  )

  res$rho <- rho
  res$R2 <- R2
  res$sd_diff <- sd_diff
  res$delta <- delta
  res$d_adj <- d_adj
  res$note <- paste0("n = number of pairs; rho = ", rho, ", R2 = ", R2)
  res$method <- "Paired t-test adjusted for correlation (rho) and prognostic R²"
  return(res)
}

pwr.t.paired.rhoR2(
  delta = 0.2,
  sigma = 1,
  rho = 0.2,
  R2 = 0.3,
  sig.level = 0.05,
 # power = 0.8,
 n=300
)


# Calcolo della potenza per il disegno paired con correlazione ~ k*R2
power_paired <- function(n_pair, d, sigma = 1, alpha = 0.05, R2 = 0, k = 0.3) {

  # Quantile per alpha
  z_alpha <- qnorm(1 - alpha/2)

  # Varianza della differenza tra due soggetti di una coppia
  var_diff <- 2 * sigma^2 * (1 - k * R2)

  # Noncentrality parameter (standardized effect in units of SE)
  z_effect <- d * sqrt(n_pair) / sqrt(var_diff)

  # Calcolo della potenza
  power <- 1 - pnorm(z_alpha - z_effect)
  return(power)
}

# Esempio di utilizzo
power_paired(
  n_pair = 49,
  d = 0.5,
  sigma = 1,
  alpha = 0.05,
  R2 = 0.851,
  k = 0.28
)
se_paired=0.1753
se_unpaire=0.2020
rho_emp=1-(se_paired^2)/(se_unpaire^2)
rho_emp/0.851


# bad specified model
se_paired=0.079
se_unpaire=0.0816
rho_emp=1-(se_paired^2)/(se_unpaire^2)
rho_emp/0.51

power_paired(
  n_pair = 300,
  d = 0.2,
  sigma = 1,
  alpha = 0.05,
  R2 = 0.576,
  k = 0.122
)
