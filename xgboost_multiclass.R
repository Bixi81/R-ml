library(ISLR)
library(xgboost)

# Data
mydat=iris

# Labels must be one-hot encoded
mydat$Species = as.integer(iris$Species)-1

# Define xgb matrix
xgb_train = xgb.DMatrix(data=as.matrix(mydat[,1:4]),label=as.matrix(mydat[,5]))

# Set some parameter
params = list(booster="gbtree",eta=0.01,
              objective="multi:softprob",
              eval_metric="mlogloss",
              num_class=3)

# Define model
xgb_fit <-xgb.train(params=params,
                    data=xgb_train,
                    nrounds=500,
                    nthreads=4,
                    verbose=1
)

# Make prediction (one column per target class containing the probability that some observation belongs to a class)
xgb_pred = predict(xgb_fit,xgb_train,reshape = T)
