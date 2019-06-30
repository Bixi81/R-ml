
library(glmnet)

# Simple boosting example from:
# P. Bühlmann, T. Hothorn (2007), "Boosting Algorithms: Regularization, Prediction and Model Fitting", Statistical Science 22(4), p. 477-505.
# L2-Boosting from Section 3.3 (p. 483)

#######################
# Data 
x = matrix(c(130,165,150,150,140,198,307,350,318,304,302,429),nrow=6,ncol=2,byrow = TRUE) 
y = c(18,15,18,16,17,15)

# Parameter
# This works like a learning rate
nu = 0.1
# Early stopping (SSR value to stop)
es = 7.58765583743327
# Max. iterations (of boosting)
maxiter = 10000

#######################
# OLS benchmark model
olsdata = data.frame(cbind(y,x))
ols = lm(y~.,data=olsdata)
olspreds = predict(ols,newdata=olsdata)
olsssr = sum((y-olspreds)^2)
print(paste0("OLS SSE: ",olsssr))

#######################
# Boosting 

# Initialize f0
f0 = mean(y)
f0ssr = sum((y-f0)^2)

# Lists to store results per boosting iteration
ssrlist = list()
bstep = list()

# Boosting (p. 483, Sec. 3.3, L2-Boosting)
for (n in seq(1:maxiter)){
  # I) Get residual
  if(n==1){f=f0} # initialize in first step  
  u=y-f          # get residual from estimation 
  # II) Fit residual / predict, I use ridge (alpha=0)
  reg = glmnet(x,u,alpha=0, lambda=2)       
  g = predict(reg, newx=x, type="link") 
  # III) Update f
  f=f+nu*g
  # Print feedback
  cat(paste0("Step: ", n," SSR: ", sum(u^2), "\n" ))
  # Save SSR/iter to list
  ssrlist[[n]] <- sum(u^2)
  bstep[[n]] <- n
  # Early stopping rule
  if(sum(u^2)<es){break}
}

#######################
# End statement
cat("\n ~~~ RESULTS ~~~ \n")
cat(paste0("OLS SSR:           ", olsssr, "\n"))
cat(paste0("Initialised SSR:   ", f0ssr, "\n"))
cat(paste0("Last SSR boosting: ", ssrlist[length(ssrlist)], " [after ",bstep[length(bstep)], " iterations] \n"))

# Plots
cbind(y,olspreds,f)

# Plot SSR by iteration
par(mfrow=c(1,2))
plot(bstep,ssrlist,xlab="Boosting Step", ylab="SSR", ylim=range(ssrlist), type="l")   
abline(h=olsssr,col="red")

# Plot y and y_hat
plot(y,olspreds)
points(y,f,col=2)
lines(y,y,type="l")
