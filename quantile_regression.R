# Simulate data (normal distribution)
n = 10000
df1 = data.frame(y=rnorm(n)+1, i=rep.int(1, n))
df2 = data.frame(y=rnorm(n), i=rep.int(0, n))
df = rbind(df1,df2)

plot(density(df1$y), main="PDF")
lines(density(df2$y))

# OLS
ols = lm(y~i,data=df)
summary(ols)
confint(ols)

# Quantile Regression
#install.packages("quantreg")
require(quantreg)
fit <- rq(y~i,tau = 0.99, data=df)
summary(fit)
quantile(df2$y, 0.99)
quantile(df1$y, 0.99)

#summary(fit, se = "boot", bsmethod= "xy")

# Bootstrapped Confience Interval
# https://stackoverflow.com/questions/38106310/calculating-95-confidence-intervals-in-quantile-regression-in-r-using-rq-functi
x <- cbind(1,df$i)
QR.b <- boot.rq(x,df$y,tau=0.99, R=300)
t(apply(QR.b$B, 2, quantile, c(0.025,0.975)))

quantile(df1$y, 0.995)
