# train model superlearner -------------

load("data/data_registry.rda")

# 1. model SL

set.seed(123)
library(SuperLearner)

y_train <- dt$response_cont |>
  as.vector() |>
  as.numeric()

x_train <- dt |> dplyr::select(-response_cont)

sl <- SuperLearner(
  Y = y_train, X = x_train, family = gaussian(),
  SL.library = c(
    "SL.mean",
    "SL.glmnet",
    "SL.xgboost",
    "SL.caret",
    "SL.svm",
    "SL.randomForest",
    "SL.cforest",
    "SL.bayesglm"
  )
)
summary(sl)

sl_model <- sl


sl_cv <- CV.SuperLearner(
  Y = y_train, X = x_train, family = gaussian(), V = 5,
  SL.library = c(
    "SL.mean",
    "SL.glmnet",
    "SL.xgboost",
    "SL.caret",
    "SL.svm",
    "SL.randomForest",
    "SL.cforest",
    "SL.bayesglm"
  )
)
summary(sl_cv)

sl_list <- list(sl, sl_cv)

y_train <- dt$response_cont |>
  as.vector() |>
  as.numeric()

x_train <- dt |> dplyr::select(-response_cont, -V9,-V8, -V2)

sl_bad <- SuperLearner(
  Y = y_train, X = x_train, family = gaussian(),
  SL.library = c(
    "SL.mean",
    "SL.glmnet",
    "SL.xgboost",
    "SL.caret",
    "SL.svm",
    "SL.randomForest",
    "SL.cforest",
    "SL.bayesglm"
  )
)
summary(sl_bad)

sl_model_bad <- sl_bad


sl_cv_bad <- CV.SuperLearner(
  Y = y_train, X = x_train, family = gaussian(), V = 5,
  SL.library = c(
    "SL.mean",
    "SL.glmnet",
    "SL.xgboost",
    "SL.caret",
    "SL.svm",
    "SL.randomForest",
    "SL.cforest",
    "SL.bayesglm"
  )
)
summary(sl_cv_bad)

sl_list_bad <- list(sl_bad, sl_cv_bad)


sl_model <- sl
sl_model_bad<-sl_bad


# save model to be used then
save(sl_model, file="models/sl_model.rda")
save(sl_model_bad, file="models/sl_model_bad.rda")

#save list for outputs 1) output 2) perfomance
save(sl_list,file="output/sl_list.rda")
save(sl_list_bad,file="output/sl_list_bad.rda")

