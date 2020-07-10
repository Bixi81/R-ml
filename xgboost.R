dtrain <- xgb.DMatrix(data = xtrain, label = ytrain)
dtest  <- xgb.DMatrix(data = xtest, label = ytest)

# rounds=40; depth=13, lr=0,4 scheint gut geeignet -> 86,15
bst <- xgboost(data = dtrain, max.depth = 13, eta = 0.4, nthread = 2, nrounds = 40, objective = "binary:logistic", verbose = 1)
# Predict (probability of belonging to class X)
p_xg <- predict(bst, dtest)
# Encode (0,1)
xg_class <- as.numeric(p_xg > 0.5)
#confusionMatrix(data = as.factor(xg_class), as.factor(ytest))
cmxg = confusionMatrix(data = as.factor(xg_class), as.factor(ytest))
cmxg

# Variable importance plot
vip(bst, num_features = 10) 
