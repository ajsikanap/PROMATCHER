# train model XGBOOST -------------

load("data/data_registry.rda")

# 3. model xgboost

set.seed(123)
library(caret)

fitControl <- trainControl( 
  method = "repeatedcv",
  number = 5,
  repeats = 5
) 

library(xgboost) # for xgboost

grid <- expand.grid(
  nrounds = c(80, 100, 120),        
  max_depth = c(2, 3, 4),            
  eta = c(0.1, 0.2, 0.3),             
  gamma = c(0, 0.5, 1),               
  colsample_bytree = c(0.6, 0.8, 1), 
  min_child_weight = c(1, 2, 5),      
  subsample = c(0.6, 0.75, 1)         
)


xgboost_fit <- train(response_cont ~ .,
                     data = dt,
                     method = "xgbTree",
                     trControl = fitControl,
                     preProcess = "scale",
                     tuneGrid=grid
                     
)

xgboost_model <- xgboost_fit
summary(xgboost_fit)
xgboost_fit


library(tidyverse)
library(xgboost) # for xgboost
grid <- expand.grid(
  nrounds = c(80, 100, 120),       
  max_depth = c(2, 3),            
  eta = c(0.1, 0.2, 0.3),           
  gamma = 0,                        
  colsample_bytree = 0.6,           
  min_child_weight = c(1, 2),       
  subsample = 0.75                  
)

xgboost_fit_bad <- train(response_cont ~ .,
                     data = dt |> dplyr::select(-V9,-V8, -V2),
                     method = "xgbTree",
                     trControl = fitControl,
                     preProcess = "scale",
                     tuneGrid=grid
                     #
)

xgboost_model_bad <- xgboost_fit_bad
summary(xgboost_fit)
xgboost_fit

xgboost_model <- xgboost_fit
xgboost_model_bad <- xgboost_fit_bad

save(xgboost_model, file="models/xgboost_model.rda")
save(xgboost_model_bad, file="models/xgboost_model_bad.rda")
