
# train model glm -------------

load("data/data_registry.rda")

# 1. model glm
set.seed(123)
library(caret)

fitControl <- trainControl( 
  method = "repeatedcv",
  number = 5,
  repeats = 5
) 

glm_fit <- train(response_cont ~ .,
                 data = dt,
                 method = "glm",
                 family = "gaussian",
                 trControl = fitControl,
                 preProcess = "scale"
)
print(glm_fit)
summary(glm_fit)
varImp(glm_fit)



library(tidyverse)
glm_fit_bad <- train(response_cont ~ .,
                 data = dt |> dplyr::select(-V9,-V8, -V2),
                 method = "glm",
                 family = "gaussian",
                 trControl = fitControl,
                 preProcess = "scale"
)
print(glm_fit_bad)
summary(glm_fit_bad)


glm_model <- glm_fit
glm_model_bad <- glm_fit_bad


save(glm_model, file="models/glm_model.rda")
save(glm_model_bad, file="models/glm_model_bad.rda")
