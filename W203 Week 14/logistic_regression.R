#http://stats.stackexchange.com/questions/81000/calculate-coefficients-in-a-logistic-regression-with-r
#================================================
# compute the logistic regression parameters as 
#   an optimal value
#================================================
# define the logistic transformation

urlSheatherData = "http://www.stat.tamu.edu/~sheather/book/docs/datasets/MichelinNY.csv"
dfSheather = as.data.frame(read.csv(urlSheatherData, header = TRUE))

# create the design matrices
y_1 = as.matrix(dfSheather['InMichelin'])
M = as.matrix(dfSheather[c('Service','Decor', 'Food', 'Price')])

# add an intercept to the predictor variables
X_1 = cbind(1, M)

logit = function(X, b) {
  return( 
    exp(X %*% b) / (1 + exp(X %*% b)))
}

# stable parametrisation of the log-likelihood function
# Note: The negative of the log-likelihood is being returned, since we will be
# /minimising/ the function.
logLikelihoodLogitStable = function(b, X, y) {
  return(
    -sum(y*(X %*% b - log(1 + exp(X %*% b))) + (1 - y)*(-log(1 + exp(X %*% b)))) 
  )
}

# initial set of parameters
b_0 = c(10, -0.1, -0.3, 0.001, 0.01) # arbitrary starting parameters

# minimise the (negative) log-likelihood to get the logit fit
optimLogit = optim(b_0, logLikelihoodLogitStable,
                   X = X_1, y = y_1, method = 'BFGS', 
                   hessian=TRUE)

#================================================
# test against the implementation in R
# NOTE glm uses IRWLS: 
# http://en.wikipedia.org/wiki/Iteratively_reweighted_least_squares
# rather than the BFGS algorithm that we have reported
#================================================
logitSheather = glm(InMichelin ~ Service + Decor + Food + Price,
                    data = dfSheather, 
                    family = binomial, x = TRUE)