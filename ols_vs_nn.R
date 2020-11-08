df = data.frame(y=c(100,110,90,120,80,200,210,190,220,180),x1=c(0,0,0,0,0,1,1,1,1,1),x2=c(1,1,1,1,1,0,0,0,0,0))

ols=lm(y~x1+x2,data=df)
summary(ols)
# one level is dropped due to multicolinearity
library(Metrics)
print(mae(df$y, predict(ols, newdata=df)))


##################################
library(keras)
library(tidyverse)
# Keras

train_data = as.matrix(df$x1, df$x2)
train_labels = as.matrix(df$y)

lrate=0.8
epochs = 5000
build_model <- function() {
  model <- keras_model_sequential() %>%
    # OLS like
    layer_dense(units = 1, input_shape = dim(train_data)[2])
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(lr=lrate),
    metrics = list("mean_absolute_error")
  )
  model
}

model <- build_model()

history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.3,
  verbose = 0
)


c(loss, kmae) %<-% (model %>% evaluate(train_data, train_labels, verbose = 0))
print(kmae)
