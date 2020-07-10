# Keras
# wir brauchen einen y-Vektor und eine X-Matrix
train = data.frame(train)
ytrain = as.matrix(train$Gender)
xtrain = as.matrix(train[,-1])

test = data.frame(test)
ytest = as.matrix(test$Gender)
xtest = as.matrix(test[,-1])

# Scaling (train and test but ONLY based on mean/sd from train data)
# mean and sd per col (from train)
mean <- apply(xtrain, 2, mean)
std  <- apply(xtrain, 2, sd)
# allply mean and sd in scale to both (test and train)
xtrain <- scale(xtrain, center = mean, scale = std)
xtest  <- scale(xtest, center = mean, scale = std)

# KERAS-Model

regul = 0.01 
initi = "uniform"

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

# 16,8,8
model_keras %>% 
  layer_dense(units = 64, kernel_initializer = initi, activation = "relu", input_shape  = ncol(xtrain), kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  
  layer_dense(units = 32, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 32, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  
  layer_dense(units = 16, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 16, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  
  layer_dense(units = 8, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  layer_dense(units = 8, kernel_initializer = initi, activation = "relu", kernel_regularizer = regularizer_l2(l = regul)) %>% 
  layer_batch_normalization() %>%
  
  layer_dense(units = 1, kernel_initializer = initi, activation = "sigmoid") %>% 
  
  # Compile NN
  compile(
    #optimizer = 'adam',
    optimizer = optimizer_rmsprop(lr = 0.0001), #standard LR
    loss      = 'binary_crossentropy',
    metrics   = c('accuracy'))

model_keras

# Fit the keras model to the training data
history <- fit(
  object           = model_keras, 
  x                = xtrain, 
  y                = ytrain,
  batch_size       = 1000, 
  epochs           = 600, #30
  validation_split = 0.3
)

# Plot history
plot(history)

# PREDICTIONS
# Predicted Class
class_keras <- predict_classes(object = model_keras, x = xtest) %>%
  as.vector()
# Predicted Class Probability
p_keras  <- predict_proba(object = model_keras, x = xtest) %>%
  as.vector()

# Confusion matrix
cmkeras = confusionMatrix(data = as.factor(class_keras), as.factor(ytest))
cmkeras
