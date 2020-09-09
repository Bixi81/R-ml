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

root = 2

# Custom objective function (squared error)
myobjective <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  grad <- root*(preds-labels)
  hess <- rep(root, length(labels))
  return(list(grad = grad, hess = hess))
}

# Custom Metric
evalerror <- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  u = (preds-labels)^root
  err <- (sum(u) / length(u))^(1/root)
  return(list(metric = "MyError", value = err))
}

# Model Parameter
param1 <- list(booster = 'gbtree'
              , learning_rate = 0.1
              , objective = myobjective 
              , eval_metric = evalerror
              , set.seed = 2020)

# Train Model
xgb1 <- xgb.train(params = param1
                 , data = dtrain
                 , nrounds = 500
                 , watchlist
                 , maximize = FALSE
                 , early_stopping_rounds = 5)

# Predict
pred1 = predict(xgb1, dtest)
mae1 = mae(test$Salary, pred1)

plot(test$Salary, pred1, ylim=c(0,1000), xlim=c(0,1000), xlab="True", ylab="Predicted", main=paste0("Root = ",root))
abline(0,1, col="red")
