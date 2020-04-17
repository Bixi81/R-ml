# See: https://stackoverflow.com/a/61275219/9524424

# DF for person 1
# Data should give intercept = 0 and slope = 1
df1 = data.frame(c(0.9,1.1,1.9,2.1,2.9,3.1),c(1,1,2,2,3,3),c(0,0,0,0,0,0))
colnames(df1)<-c("y","x","i")
summary(lm(y~x,data=df1))

# New DF for person 2
df2 = df1
# y always + 1 for this guy
df2$y = df1$y + 1
# He/she is indicated by i = 1
df2$i = 1

# Combine DFs
df=rbind(df1,df2)
df

# Regression
mymod = lm(y~x+i,data=df)
summary(mymod)

# Prediction and plot
pred = predict(mymod, newdata=df)
plot(df$x[df$i==0], pred[df$i==0],type="l",ylim=c(0,5), xlim=c(1,3), xlab = "x", ylab="y")
lines(df$x[df$i==1], pred[df$i==1], type="l", col="red")

# How does "i" look like
is.numeric(df$i)

# We can treat this as a factor
df$i = as.factor(df$i)
summary(lm(y~x+i,data=df))

# We can also chenge the base level
summary(lm(y~x+relevel(i,ref="0"),data=df)) # just as before
summary(lm(y~x+relevel(i,ref="1"),data=df)) # just reversed since 1 is now the reference category
# note that intercept AND indicator change (not the marginal effect of x on y)
