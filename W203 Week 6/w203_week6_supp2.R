# Write a function in R that does the following:
# Takes sample data (e.g., as a vector), mean of the null hypothesis,  
# population standard deviation,  a Boolean variable indicating whether we want a one-tailed or two-tailed test, 
# and another Boolean variable indicating which tail (left or right) should be taken in case of a one-tailed test.
# Returns p-values for the required test (Null is “population mean is the one that the function is fed”), 
# as well as a Boolean value showing whether the test passes a conventional 5% level or not.

myfun <- function(data, mu, pop.sd, two.tailed=TRUE, left.tail=NULL) {
  # we only want to deal with a numeric vector
  if(!is.numeric(data)) stop("'data' must be a numeric vector \n")
  
  # we expect 'left.tail' to be TRUE or FALSE for one-tailed test
  if(!two.tailed & is.null(left.tail)) stop("'left.tail' must be TRUE or FALSE for one tailed test \n")
  
  # calculate z-score
  z.raw <- (mean(data) - mu) / (pop.sd/(sqrt(length(data))))
  
  # calculate a p-value assuming a right-tailed test
  p.val <- pnorm(z.raw, lower.tail = FALSE)
  
  # convert p-value to two-tailed if applicable
  if(two.tailed) {
    p.val <- 2*pnorm(abs(z.raw), lower.tail = FALSE)
  } else {
    if(left.tail == TRUE) { 
      p.val <- pnorm(z.raw, lower.tail = TRUE)
    }
  }
  
  # create a boolean value for test pass or fail
  rejectNull <- ifelse(p.val < 0.05, TRUE, FALSE)
  
  return(list(p.val, rejectNull))
}


# Choose a sample size and then use rnorm(n, mean, sd) to generate a random sample 
# and test your function.
test_mean <- 12.345
test_sd <- 2.32
test_data <- rnorm(100, mean = test_mean, sd = test_sd)

# run a two-tailed test
myfun(test_data, mu = test_mean, pop.sd = test_sd, two.tailed = TRUE, left.tail = NULL)

# run a left-tailed test
myfun(test_data, mu = test_mean, pop.sd = test_sd, two.tailed = FALSE, left.tail = TRUE)

# run a right tailed test
myfun(test_data, mu = test_mean, pop.sd = test_sd, two.tailed = FALSE, left.tail = FALSE)
