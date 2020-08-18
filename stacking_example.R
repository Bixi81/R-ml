library(dismo)
library(Metrics)

# Set up data frame
df = data.frame(c(1:10), c(2,1,3,5,6,4,8,9,10,8))
colnames(df)<-c("a","b")

# Standard regression on all data / get score
reg_all = lm(a~.,data=df)
mae(df$a,predict(reg_all,newdata=df))

# Split data into two folds
fold=kfold(df,2)

# Define train/test for fold 1
dftr1=df[fold==1,]
dfte1=df[fold!=1,]

# Define train/test for fold 2
dftr2=df[fold==2,]
dfte2=df[fold!=2,]

# Regression on fold 1
m1 = lm(a~b,data=dftr1)
# Predict on test fold 1
dfte1$pred = predict(m1,newdata=dfte1)

# Regression on fold 2
m2 = lm(a~b,data=dftr2)
# Predict on test fold 2
dfte2$pred = predict(m2,newdata=dfte2)

# Data after prediction
dfte1
dfte2

# Combine both folds to one df
meta = rbind(dfte1,dfte2)
meta

# Now do a regression on all data and include predictions as variabel
meta_reg = lm(a~.,data=meta)

# Obtain new score
mae(meta$a,predict(meta_reg,newdata=meta))
