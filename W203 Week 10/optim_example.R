getwd()
setwd("W203 Week 10")

intexp <- function(base, exp) {
  res <- base
  while (exp > 1) {
    res <- res * base
    exp <- exp - 1
  }
  return(res)
}

objg <- function(r, number, k) {
  k*r^(k-1)
}

objf <- function(r, number, k) {
  # we will suppress warnings because 
  # optim might try to calculate
  # log of a negative number
  suppressWarnings(abs(sum(number - exp(k*log(r)))))
}

rootk <- function(number, k, start = NULL) {
  # we will only deal with positive bases
  if (!all(number > 0)) {
    stop("Negative number provided as input.")
  }

  # make a guess for r if not given as input
  if (is.null(start)) { 
    start <- as.integer(number/k)
  }
  
  start <- abs(start)
  
  # call optim
  root <- suppressWarnings(sapply(seq_along(number), function(i) { 
    optim(start[[i]], objf, number = number[[i]], k = k, method = "BFGS", 
          control = list(reltol = sqrt(.Machine$double.eps)))$par
    }))
  return(root)
}


nthroot <- function(x, n, err = sqrt(.Machine$double.eps)) {
  
  x0 <- x
  x1 <- x/n
  while (abs(x0 - x1) > err) {
    x1 <- x0
    x0 <- (1/n)*(((n-1)*x1) + (x/(x1^(n-1))))
  }
  return(x0)
}
