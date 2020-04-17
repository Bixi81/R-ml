x0 <- c(1,1,1,1,1) 
x1 <- c(1,2,3,4,5)
x2 <- c(8,4,3,1,8)
x <- as.matrix(cbind(x0,x1,x2))
y <- as.matrix(c(3,7,5,11,14))

x
y

# (X'X)^-1 X'y
beta1 = solve(t(x)%*%x) %*% t(x)%*%y 

# R's regression command
beta2 = summary(lm(y ~ x[, 2:3]))

# Gradient decent
m <- nrow(y)
grad <- function(x, y, theta) {
  gradient <- (1/m)* (t(x) %*% ((x %*% t(theta)) - y))
  return(t(gradient))
}

# define gradient descent update algorithm
grad.descent <- function(x, maxit){
  theta <- matrix(c(0, 0, 0), nrow=1) # Initialize the parameters
  
  alpha = .05 # set learning rate
  for (i in 1:maxit) {
    theta <- theta - alpha  * grad(x, y, theta)   
  }
  return(theta)
}

# results without feature scaling
print(grad.descent(x,2000))
beta1
beta2
