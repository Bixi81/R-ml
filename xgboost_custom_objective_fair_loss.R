library(ISLR)
library(xgboost)
library(tidyverse)
library(Metrics)

# Data
df = ISLR::Hitters %>% select(Salary,AtBat,Hits,HmRun,Runs,RBI,Walks,Years,CAtBat,CHits,CHmRun,CRuns,CRBI,CWalks,PutOuts,Assists,Errors)
df = df[complete.cases(df),]
train = df[1:150,]
test = df[151:nrow(df),]

# XGBoost Matrix
dtrain <- xgb.DMatrix(data=as.matrix(train[,-1]),label=as.matrix(train[,1]))
dtest <- xgb.DMatrix(data=as.matrix(test[,-1]),label=as.matrix(test[,1]))
watchlist <- list(eval = dtest)

# Custom objective function ("Fair loss")
myobjective <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  x = preds - labels
  c = 10   # "fairness score" to be tuned
  den = abs(x) + c
  grad = c*x / den
  hess = c*c / den^2
  return(list(grad = grad, hess = hess))
}

# Custom Metric
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  u = (preds-labels)^2
  err <- (sum(u) / length(u))^(1/2)
  return(list(metric = "MyError", value = err))
}

# Model Parameter
param <- list(booster = 'gbtree'
               , learning_rate = 0.1
               , objective = myobjective 
               , eval_metric = evalerror
               , set.seed = 2020)

# Train Model
xgb <- xgb.train(params = param
                  , data = dtrain
                  , nrounds = 500
                  , watchlist
                  , maximize = FALSE
                  , early_stopping_rounds = 5
                  ,verbose=1)

# Predict
pred = predict(xgb, dtest)
mae = mae(test$Salary, pred)
print(mae)
