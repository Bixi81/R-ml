# Data
df = data.frame(c(12,32,23,13,45,31), c(657,456,265,263,475,354), c(8,5,9,4,6,3), c(1,1,0,0,1,0), c(0,1,1,0,0,1))
colnames(df) = c("f1", "f2", "f3", "t1", "t2")
df

# Logit
mylogit1 <- glm(t2 ~ f1+f2+f3, data = df, family = "binomial")
summary(mylogit1)

# Predict outcome
preds = predict(mylogit1, newdata = df, type = "response")

# Look at AUC
library(Metrics)
auc(df$t2, preds)

# Look at the confusion matrix
library(caret)
preddf = data.frame(as.factor(round(preds)),as.factor(df$t2))
colnames(preddf)=c("pred", "truth")
confusionMatrix(preddf$pred, preddf$truth)
