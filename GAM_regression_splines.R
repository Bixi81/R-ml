library(SemiPar)
library(gam)
data(mtcars)

# Set windows for plotting
par(mfrow=c(1,2))

####################################
# Semiparametric regression
fit <- spm(mtcars$mpg ~  f(mtcars$hp) + as.factor(mtcars$am))
summary(fit)
plot(fit)

####################################
# Generalised additive model (regression splines)
gam <- gam(mpg ~ s(hp,4) + as.factor(am), data=mtcars, na.action = na.exclude)
summary(gam)
plot(gam,se=T,col="red")  
