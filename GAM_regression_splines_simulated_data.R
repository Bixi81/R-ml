# Generate data
x <- -50:100
y <- 0.001*x^3
plot(x,y)
df = data.frame(y,x)

# Linear regression
reg_ols=lm(y~.,data=df)
pred_ols = predict(reg_ols, newdata=df)

# GAM with regression splined (df=3)
library(gam)
reg_gam = gam(y~s(x,3), data=df)
pred_gam = predict(reg_gam, newdata=df)

# Plot prediction and actual data
require(ggplot2)
df2 = data.frame(x,y,pred_ols, pred_gam)
ggplot(df2, aes(x)) +                    
  geom_line(aes(y=y),size=1, colour="red") +  
  geom_line(aes(y=pred_ols),size=1, colour="blue") +
  geom_line(aes(y=pred_gam),size=1, colour="black", linetype = "dashed")  
