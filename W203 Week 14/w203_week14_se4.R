# getwd()
# setwd("W203 Week 14")
library(MASS)
N = 100
#creating independent variables
#first, the covariance matrix for our multi-variate normal
# this determines the correlation of our independent variables.
sig = matrix(c(2,.5,.25,.5,1,0,.25,0,1) , nrow=3)
# now the variables:
M = mvrnorm(n = N, mu = rep(1,3), Sigma = sig )
y.cont = 1 + 2* M[,1] - 5 * M[,2] + M[,3] + rnorm(N)
y.bin = as.numeric ( y.cont > 0 )
# include the intercept in your independent variables
X = cbind (1, M )
y = y.cont
# or y = y.bin , depending on the exercise

# Part 1. OLS using a recursive function
reg <- function(y, x) {
  if (as.integer(length(x)) == 1) {
    return(sum(x*y) / sum(x^2))
  } else {
      x = ifelse(length(x) > 1, x[, -length(x)], x[, length(x)])
      return(reg(y, x))
  }
}

pwr = function(x,i){
  # if i==0, return 1
  # if i>0, return x^(i-1) * x
  # if i<0, return x^(i+1) / x
  if (as.integer(i) == 0 ) return( 1)
  else return(
    ifelse( i>0, pwr(x,i-1)* x , pwr(x,i+1) /x ))
}


pwr(10, -3)

# Part 2. OLS using numeric optimization
# Generate heteroskedastic data

x1 <- runif(100)
x2 <- x1*rnorm(100)
y <- 1 + 2*x1 + rnorm(100)*x1*x2

d <- data.frame(y, x1, x2)

plot(y)
scatterplot(x1, y)

# linear model without bootstrap
mod <- lm(y ~ x1 + x2, data = d)
summary(mod)
confint(mod)

# use bootstrap to estimate the model
library(boot)

bootReg <- function (formula, data, i)
{
  d <- data [i,]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

boot.r <- boot(statistic = bootReg, formula = y ~ x1 + x2, data = d, R = 2000)
boot.r
summary(boot.r)
boot.ci(boot.r, type = "bca", index = 1)
boot.ci(boot.r, type = "bca", index = 2)
boot.ci(boot.r, type = "bca", index = 3)

library(sandwich)
library(lmtest)
?sandwich
?coeftest

coeftest(mod)
coeftest(mod, vcov = sandwich)
coeftest(mod, vcov = vcovHC)

# Part 3. Logit

likelihood <- function(X, b) {
  return(1 / (1 + exp(-(X%*%b))))
}

logit <- function(y, X, b) {
  return(
    sum( # sum the logs of likelihood
      log( # take a log of likelihood
        ifelse(y == 1, likelihood(y, X, b), 1 - likelihood(y, X, b)))))
}

loglikelihood <- function(y, X, b) {
  y*log(likelihood(X, b) + (1 - y)*log(1 - likelihood(X, b)))
}
