
# dataset generation and storage -----
if (!require("clusterGeneration")) install.packages("clusterGeneration")
library("clusterGeneration")
if (!require("mvtnorm")) install.packages("mvtnorm")
library(mvtnorm)
library(dplyr)
library(tidyverse)

# 1.CONTINUOUS ----

# data for PAIRED MATCHING ------------
data_continuous<-list()
k <- 15 # Number of covariates
N= 3000
delta=.2
iter=10000

for (sim in seq_along(1:iter)) {
  print(sim)
  set.seed(123 * sim)
  sigma <- genPositiveDefMat(k, "unifcorrmat")$Sigma
  sigma <- cov2cor(sigma)

  z_fix_prescale <- rmvnorm(N,
                            sigma = sigma,
                            mean = c(
                              50.3, 23.14, 84.6, 190.1, 130,
                              47.75, 20.3, 14, 14.57, 0.83, 10, 145, 0.5, 2, 13
                            )
  )
  z <- as.data.frame(z_fix_prescale)
  z$V16 <- rbinom(N, 1, 0.5)
  z$V17 <- rbinom(N, 1, 0.1)
  # beta

  # beta
  b <- c(
    0.38, 0.1, 0.2, 0.06, 0.4, -0.15, 0.09, 0.15, 1.6, 0.9,
    -0.38, 0.8, -0.24, 0.5, 0.18
  )
  b2 <- c(3.6, 2.5)

  y1 <- as.vector(
    b[1] * z[, 1] +
      b[2] * (z[, 2]^2) + # Quadratic term
      b[3] * sqrt(abs(z[, 3])) +
      b[4] * z[, 4] + # modificato da sin 21082025
      b[5] * (z[, 5] * log(z[, 8])) +
      b[6] * log(z[, 6]) +
      b[7] * z[, 7]^2 + # Quadratic term
      b[8] * z[, 8] +
      # b[9] * (sin(z[,9]) / (cos(1.5 * z[,13]) + 1) * z[,11]) +  # Sine transformation
      b[10] * z[, 10] + # Exponential transformation


      # Unmeasured Covariates used for y1 but not in model prediction
      b[11] * z[, 11] +
      b[12] * z[, 12] +
      b[13] * z[, 13] +##
      b[14] * z[, 14]^2 + # Quadratic term
      b[15] * z[, 15] +


      # Additional terms
      b2[1] * (z[, 16]) + # Binary interaction
      b2[2] * (z[, 17]) +
      b[4] * (z[, 17] * z[, 7]) + #  Additional interaction
      # b2[1] * sin(2 * pi * z[,1] / 110) +  # Additional non-linearity
      b2[2] * (z[, 6] / (z[, 9] + 1)) + # Additional interaction
      rnorm(N, mean = 0, sd = 8) # Noise term
  ) |>
    unlist() |>
    as.numeric() |>
    as.vector()


  y_pre_T_c <- y1
  data <- cbind(y_pre_T_c, z) |>
    #  mutate_at(vars(1:11), ~scale(.)) |>
    as.data.frame()
  data <- data[, c(1:11, 17:18)]
  colnames(data) <- c("y_pre_T_c", paste0("V", c(1:10, 16:17)))
  data$id <- seq(1, nrow(data))

  data_RCT1 <- data
  library(randomizr)
  data_RCT1$T_equal <- complete_ra(N = nrow(data_RCT1))


  data_continuous[[sim]]<-data_RCT1

}

save(data_continuous, file="data/data_continuous_10000_rcts.rda")

