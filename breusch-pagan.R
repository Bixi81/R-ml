library(MASS)
data(Boston)
# Logic of Breusch-Pagan test

# Regressions
reg1 = lm(medv~indus+lstat, data=Boston)
reg2 = lm(medv~indus+crim, data=Boston)

# Breusch-Pagan test
library(lmtest)
bptest(reg1)
bptest(reg2)
# Null hypothesis of BP test = homoscedasticity (= variance does not depend on auxiliary regressors)
# If the p-value is "small" (<0.05), the null hypothesis is rejected
# reg1 -> heteroscedasticity 
# reg2 -> no heteroscedasticity 

# Auxiliary regressions (resid~X)
# 1) Get residuals
Boston$u1 = residuals(reg1)^2
Boston$u2 = residuals(reg2)^2
# 2) Regression
aux1 = lm(u1~indus+lstat, data=Boston)
summary(aux1)
aux2 = lm(u2~indus+crim, data=Boston)
summary(aux2)
