library("e1071")
head(iris,5)

attach(iris)
# Divide Iris data to x (containt the all features) and y only the classes
x <- subset(iris, select=-Species)
y <- Species

# SVM Model
svm_model <- svm(Species ~ ., data=iris)
summary(svm_model)
# or alternatively...
svm_model <- svm(x,y)
summary(svm_model1)

# Predict
pred <- predict(svm_model,x)
table(pred,y)

# Tune SVM to find the best cost and gamma
svm_tune <- tune(svm, train.x=x, train.y=y, kernel="radial", ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)))
print(svm_tune)

# New SVM model with tuned parameters
svm_model2 <- svm(Species ~ ., data=iris, kernel="radial", cost=1, gamma=0.5)

# Predict
pred2 <- predict(svm_model2,x)
table(pred2,y)
