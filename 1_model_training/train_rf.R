
# train model RF -------------

load("data/data_registry.rda")

# 2. model rf

set.seed(123)
library(caret)
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5
) 

rf_grid <- expand.grid(mtry = 1:12)
rf_fit <- train(response_cont ~ .,
                data = dt,
                method = "rf",
                family = "gaussian",
                trControl = fitControl,
                preProcess = "scale",
                tuneGrid=rf_grid
                
)

summary(rf_fit)
rf_fit$results
rf_model <- rf_fit


library(tidyverse)
rf_grid <- expand.grid(mtry = 1:9)

rf_fit_bad <- train(response_cont ~ .,
                data = dt |> dplyr::select(-V9,-V8, -V2),
                method = "rf",
                family = "gaussian",
                trControl = fitControl,
                preProcess = "scale",
                tuneGrid=rf_grid
)

summary(rf_fit_bad)
rf_fit_bad$results
rf_model <- rf_fit




rf_model <- rf_fit
rf_model_bad <- rf_fit_bad


save(rf_model, file="models/rf_model.rda")
save(rf_model_bad, file="models/rf_model_bad.rda")
