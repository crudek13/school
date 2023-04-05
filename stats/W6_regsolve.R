#predict mpg from wt and hp using lm
basemodel <- lm(mpg ~ wt + hp, data = mtcars)
basemodel$coefficients


#Using Matrices
y <- mtcars$mpg
X <- cbind(rep(1, length(mtcars$mpg)), mtcars$wt, mtcars$hp)
beta <- solve((t(X) %*% X)) %*% (t(X) %*% y)
beta

#Minimize SSE
SSE <- function(beta, X, y){
  return(sum((y - X %*% beta) ^ 2))
}

minSSE <- optim(c(0,0,0), SSE, X = X, y = y)
minSSE 

# Using maximum likelihood (!)
# Recall, we assumed the errors (y-X*Beta) were normally distributed with mean 0 
# and variance sigma^2 (matrix notation)

# We have 4 parameters! 
# param = c(beta0, beta1, beta2, sigma)
normloglik <- function(param, X, y){
  return(-sum(log(dnorm(y - X %*% param[1:3], 0, param[4]))))
}

MLE <- optim(par = c(0,0,0,1), fn = normloglik, X = X, y = y, control = list(maxit = 10000))
MLE

basemodel$coefficients
beta
minSSE
MLE$par

