data(mtcars)
head(mtcars)

# Loop over variables is done with "lapply"

# Do something 
##############
# Operation: 0.5 * x 
# x is the value of the variable
# function(x) is the prefix to call the command
lapply(mtcars, function(x) 0.5*x)

# Do something for selected columns
###################################
# First two columns
lapply(mtcars[c(1:2)], function(x) 0.5*x)

# Do something by variable name
###############################
varlist = c("mpg", "wt")
lapply(mtcars[varlist], function(x) 0.5*x)

# Replace existing values in data
#################################
varlist = c("mpg", "wt")
mtcars[varlist] = lapply(mtcars[varlist], function(x) 0.5*x)

# Generate new variables
########################
varlist = c("mpg", "wt")
newvarlist = c("mpg2", "wt2")
mtcars[newvarlist] = lapply(mtcars[varlist], function(x) 0.5*x)
head(mtcars)

# Generate new variables by extending (old) variable names
##########################################################
varlist = c("mpg", "wt")
mtcars[paste("new", varlist, sep="")] = lapply(mtcars[varlist], function(x) 0.5*x)
head(mtcars)
