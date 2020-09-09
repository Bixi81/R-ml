# Simple example for XGBoost regression on GPU

# Data
x = matrix(c(130,165,150,150,140,198,307,350,318,304,302,429),nrow=6,ncol=2,byrow = TRUE) 
y = c(18,15,18,16,17,15)

# OLS benchmark model
olsdata = data.frame(cbind(y,x))
ols = lm(y~.,data=olsdata)
olspreds = predict(ols,newdata=olsdata)
olsssr = sum((y-olspreds)^2)

# XGBoost
require(xgboost)
dtrain <- xgb.DMatrix(data=as.matrix(x),label=y)

# Note that this is a linear booster, for tree-based use "gbtree" or "dart"
# See: https://xgboost.readthedocs.io/en/latest/parameter.html#parameters-for-linear-booster-booster-gblinear
param <- list(booster = "gblinear"
            , tree_method = "gpu_exact"
            , objective = "reg:linear"
            , lambda = 2.854
            , eval_metric = 'mae'
            , seed = 4757)

xgb <- xgb.train(params = param
               , data = dtrain
               , nrounds = 668)

xgbpreds <- predict(xgb,dtrain)
xgbssr = sum((y-xgbpreds)^2)

print(paste0("OLS SSE: ",olsssr))
print(paste0("XGB SSE: ",xgbssr))
