### Factors to dummies

# Data frame with two factor columns (1,2) and one numeric (3)
mydf = data.frame(cbind(c(1,1,2,2), c(1,1,3,3), c(7,8,9,7)))
mydf$X1 <- as.factor(mydf$X1)
mydf$X2 <- as.factor(mydf$X2)
mydf

# Dummies for X1 only (no intercept)
mymat = model.matrix(~ X1 -1 , data=mydf)
mymat

# Only factors to dummies -> third column (which is no factor) excluded
mymat2 = model.matrix(~ X1 + X2 - 1, data=mydf, 
            contrasts.arg=list(
              X1=diag(nlevels(mydf$X1)), 
              X2=diag(nlevels(mydf$X2))))

# Only factors to dummies -> third column included
mymat3 = model.matrix(~ . -1, data=mydf, contrasts.arg = lapply(mydf[,sapply(mydf, is.factor)], contrasts, contrasts=FALSE))
