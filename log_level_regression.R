# Pop China rounded (mio.)
# https://en.wikipedia.org/wiki/Demographics_of_China
df = data.frame(c(547,609,667,715,818,916,981,1045),c(1950,1955,1960,1965,1970,1975,1980,1985))
colnames(df)<-c("pop","year")
df

# Normal linear regression "level-level"
reg1 = lm(pop~year,data=df)
summary(reg1)

# Predict / plot result
pred1 = predict(reg1, newdata=df)
plot(df$year, pred1, type="b")
lines(df$year, df$pop, type = "o", col = "blue")

# Linear regression log-level
reg2 = lm(log(pop)~year,data=df)
summary(reg2)
reg2$coefficients[2]

# The average growth rate
exp(reg2$coefficients[2])-1

# Predict / plot result
pred2 = exp(predict(reg2, newdata=df))
plot(df$year, pred2, type="b")
lines(df$year, df$pop, type = "o", col = "blue")

# "Out of sample" prediction
preddf = data.frame(c(1950,1955,1960,1965,1970,1975,1980,1985,1990,1995,2000,2005,2010,2015))
colnames(preddf)<-c("year")
pred3 = exp(predict(reg2, newdata=preddf))

# Actual figures
actual = c(547,609,667,715,818,916,981,1045,1135,1204,1263,1304,1338,1375)
result = data.frame(cbind(preddf,round(pred3),actual))
colnames(result)<-c("year","pred","act")
result

# Plot results
plot(result$year, result$pred, type="b")
lines(result$year, result$act, type = "o", col = "blue")
