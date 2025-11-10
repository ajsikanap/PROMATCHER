# Generation of the registry ------------------

options(scipen = 999)
library(tidyverse)

# simulate data of a nutritional data
# log(OR) = 1.2, 1.6
if (!require("clusterGeneration")) install.packages("clusterGeneration")
library("clusterGeneration")
if (!require("mvtnorm")) install.packages("mvtnorm")
library("mvtnorm")



## 1. Generate covariance matrix of z = # ----

## a.  Multivariate correlated data with Gamma
# install.packages("simstudy")
library(simstudy)
# ?genCorGen
# generazione di una Multivariate random gamma
# simulo il registro
set.seed(123)

N <- 2000
k <- 16
mean <- c(
  50.3, 23.14, 84.6, 190.1, 130, 47.75, 20.3, 14, 14.57, 0.83, 106.8 # ,#LAST is LDL
  , 10, 145, 0.5, 2, 13
) # 5 aggiuntive
beta <- c(
  25, 3.5, 4.5, 6, 2, 4, 3, 1.5, 3.5, 1.2, 4.5 # , #LAST is LDL
  , 3, 12, 3, 4, 1.3
)
scale <- 1 / beta

set.seed(123)
z_gamma <- genCorGen(
  n = N,
  nvars = k,
  params1 = mean, # vector specifying the mean of the distribution
  params2 = scale, # dispersion
  dist = "gamma",
  rho = 0.1,
  corstr = "cs",
  corMatrix = NULL,
  wide = TRUE,
  cnames = NULL,
  method = "copula",
  idname = "id"
)[, -1] # remove id

# https://search.r-project.org/CRAN/refmans/simstudy/html/genCorGen.html
# https://link.springer.com/article/10.1007/s00477-010-0434-8
z_gamma |> summary()
# it does not woek propertly

# V11 è l'LDH
glm(V11 ~ ., family = "gaussian", data = z_gamma) |> summary()

# Variable    // BETA
# Age, y      // 0.024  !ref
# BMI, kg/m2  // -0.152 !ref
# WC, cm      // 0.2    !ref
# TC, mg/dL   // 0.05
# TG, mg/dL   // 0.05
# HDL-C, mg/dL//  -0.9
# LDL-C, mg/dL// ------------outcome
# AST, IU/L   // 0.5
# ALT, IU/L   // 0.0012
# BUN, mg/dL  // 0.002
# Creatinine, mg/dL //–1.094
# Taking dyslipidemia medication, n (%)
# Diabetes mellitus, n (%)


# taglia le code:

z_gamma <- z_gamma |>
  mutate(
    V1 = ifelse(V1 < 0, 0, ifelse(V1 > 110, 110, V1)), # age
    V2 = ifelse(V2 < 12, 12, ifelse(V2 > 60, 60, V2)), # BMI, kg/m2
    V3 = ifelse(V3 < 30, 30, V3), # WC, cm
    V4 = ifelse(V4 < 10, 10, ifelse(V4 > 500, 500, V4)), # TC, mg/dL
    V5 = ifelse(V5 < 10, 10, ifelse(V5 > 500, 500, V5)), # TG, mg/dL
    V6 = ifelse(V6 < 20, 20, ifelse(V6 > 100, 100, V6)), # HDL-C, mg/dL
    V7 = ifelse(V7 < 5, 5, ifelse(V7 > 200, 200, V7)), # AST, IU/L
    V8 = ifelse(V8 < 5, 5, ifelse(V8 > 200, 200, V8)), # ALT, IU/L
    V9 = ifelse(V9 < 1, 1, ifelse(V9 > 100, 100, V9)), # BUN, mg/dL
    V10 = ifelse(V10 < 0.2, 0.2, ifelse(V10 > 10, 10, V10)) # Creatinine, mg/dL
  )
z_gamma |> summary()
glm(V11 ~ ., family = "gaussian", data = z_gamma) |> summary()
# WC, cm

# Other laboratory value
# AST, IU/L
# ALT, IU/L
# BUN, mg/dL
# Blood urea nitrogen
# Creatinine, mg/dL


library(tidyverse)
set.seed(123)
z_gamma$V11_01 <- rbinom(N, 1, p = 0.5) # sex
z_gamma$V12_01 <- rbinom(N, 1, p = 0.1) # diabete 0.1

# glm(V11 ~ ., family="gaussian",data=z_gamma) |> summary()
# rimuovo LDH
z_gamma <- z_gamma[, -c("V11")] |> as.data.frame()



# 2. Continuous outcomes -----------
###  a. Define Continuous  OUTCOME ---------

# beta
b <- c(
  0.38, 0.1, 0.2, 0.06, 0.4, -0.15, 0.09, 0.15, 1.6, 0.9,
  -0.38, 0.8, -0.24, 0.5, 0.18
)
b2 <- c(3.6, 2.5)


z <- z_gamma


y1 <- as.vector(
  b[1] * z[, 1] +
    b[2] * (z[, 2]^2) + # Quadratic term
    b[3] * sqrt(abs(z[, 3])) +
    b[4] * z[, 4] +
    b[5] * (z[, 5] * log(z[, 8])) +
    b[6] * log(z[, 6]) +
    b[7] * z[, 7]^2 + # Quadratic term
    b[8] * z[, 8] +
    # b[9] * (sin(z[,9]) / (cos(1.5 * z[,13]) + 1) * z[,11]) +  # Sine transformation
    b[10] * z[, 10] + # Exponential transformation


    # Unmeasured Covariates used for y1 but not in model prediction
    b[11] * z[, 11] +
    b[12] * z[, 12] +
    b[13] * z[, 13] +
    b[14] * z[, 14]^2 + # Quadratic term
    b[15] * z[, 15] +


    # Additional terms
    b2[1] * (z[, 16]) + # Binary interaction
    b2[2] * (z[, 17]) +
    b[4] * (z[, 17] * z[, 7]) + #  Additional interaction
    # b2[1] * sin(2 * pi * z[,1] / 110) +  # Additional non-linearity
    b2[2] * (z[, 6] / (z[, 9] + 1)) + # Additional interaction
    rnorm(N, mean = 0, sd = 50) # Noise term
) |>
  unlist() |>
  as.numeric() |>
  as.vector()



y1 <- y1 #|> scale()
y1 |> summary() # -8058.59  -143.37    64.26   -63.43   168.34  1210.43
y1 |> sd() # 524
data <- cbind(y1, z) |> as.data.frame()
# mutate_at(vars(1:16), ~scale(.))|> as.data.frame()
colnames(data) <- c("response_cont", c(paste0("V", 1:17)))
data |> summary()

# # scale the response
data_continuous_outcome <- data |>
  dplyr::select(!V11:V15) |>
  #   mutate_at(vars(1), ~scale(.))
  # mutate_at(vars(1:11), ~scale(.))|>
  as.data.frame()
#

# try
summary(data_continuous_outcome$response_cont)
hist(data_continuous_outcome$response_cont)

# model with only known covariates
mod <- glm(response_cont ~ ., data = data_continuous_outcome, family = "gaussian")
summary(mod) # ok
varImp(mod)

deviance <- summary(mod)$deviance
null_deviance <- summary(mod)$null.deviance
rsquared <- 1 - (deviance / null_deviance)
rsquared

# model with all covariates
mod <- glm(response_cont ~ ., data = data, family = "gaussian")
summary(mod)
deviance <- summary(mod)$deviance
null_deviance <- summary(mod)$null.deviance
rsquared <- 1 - (deviance / null_deviance)
rsquared

### b. Fit ML for continuous outcome ----
dt <- data_continuous_outcome |> as.data.frame()

save(dt,file="data/data_registry.rda")
