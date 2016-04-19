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

# # Part 1. OLS using a recursive function
# reg <- function(y, x) {
#   if (as.integer(length(x)) == 1) {
#     return(sum(x*y) / sum(x^2))
#   } else {
#       x = ifelse(length(x) > 1, x[, -length(x)], x[, length(x)])
#       return(reg(y, x))
#   }
# }
# 
# pwr = function(x,i){
#   # if i==0, return 1
#   # if i>0, return x^(i-1) * x
#   # if i<0, return x^(i+1) / x
#   if (as.integer(i) == 0 ) return( 1)
#   else return(
#     ifelse( i>0, pwr(x,i-1)* x , pwr(x,i+1) /x ))
# }
# 
# 
# pwr(10, -3)

# Part 2. OLS using numeric optimization

# Generate heteroskedastic data
x1 <- runif(100, -1, 1)# rep(c(-1, 1), 50)
x2 <- rnorm(100)
err <- rnorm(100, sd = 1:10)
y1 <- 1 + x1 + x2 + err # not sure if this is a better method

library(lmtest)
bptest(y1 ~ x1) # reject homoskedasticity (p > 0.05)

library(car)
scatterplot(x1, y) 

# create a data.frame for analysis
d <- data.frame(y1, x1, x2)

# use bootstrap to estimate the model
library(boot)

# copy bootReg from DSUR book
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

# linear model without bootstrap
mod <- lm(y ~ x1 + x2, data = d)
summary(mod)
confint(mod)

library(sandwich)
library(lmtest)
?sandwich
?coeftest

coefs.sand <- coeftest(mod, vcov = sandwich)

# results of lm and boot.r look similar
cbind("boot" = boot.r$t0, "lm" = coefs.sand[, 1])

# Part 3. Logit

# create the logit function based on equation 3 from the link below
# http://faculty.smu.edu/tfomby/eco6352/Notes/Logit%20and%20Probit%20Notes.pdf
logit.mine <- function(y, X, b) {
  return(
    -sum(y*log(1 / (1 + exp(-X %*% b))) 
         + (1 - y)*log(1 - 1 / (1 + exp(-X %*% b))))
    )
}

# initialize the values of our coefficients (b)
b.new <- c(0, 0, 0, 0) # one for each independent variable in X
model.logit <- optim(b.new, logit.mine, X = X, y = y.bin, 
                     method = "BFGS", hessian = TRUE)

X.new <- cbind(y.bin, X)
model.glm <- glm(X.new[, 1] ~ X.new[, 3] + X.new[, 4] + X.new[, 5], 
                 data = data.frame(X.new), family = binomial, x = TRUE)

# compare the results
cbind("logit.coef" = model.logit$par, "glm.coef" = model.glm$coefficients)
