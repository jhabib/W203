# 1. Write a function in R that does the following:
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
  
  # convert p-value to two-tailed if applicable
  if(two.tailed) {
    p.val <- 2*pnorm(-abs(z.raw), lower.tail = TRUE)
    rejectNull <- ifelse(p.val < 0.025, TRUE, FALSE)
  } else {
    if(left.tail == TRUE) { 
      p.val <- pnorm(-abs(z.raw), lower.tail = TRUE)
    } else { # no need to caclualte separate p.vals for left- and right- but we'll do it anyways
      p.val <- pnorm(abs(z.raw), lower.tail = FALSE)
    }
    rejectNull <- ifelse(p.val < 0.05, TRUE, FALSE)
  }
  
  (list("p.value" = p.val, "rejectNull" = rejectNull))
}

set.seed(5000)

# 2. Choose a sample size and then use rnorm(n, mean, sd) to generate a random sample 
# and test your function.
pop_mean <- 50
pop_sd <- 21
sample_size <- 36
sample_data <- rnorm(sample_size, mean = pop_mean, sd = pop_sd)

# run a two-tailed test
myfun(sample_data, mu = pop_mean, pop.sd = pop_sd, two.tailed = TRUE, left.tail = NULL)

# run a left-tailed test
myfun(sample_data, mu = pop_mean, pop.sd = pop_sd, two.tailed = FALSE, left.tail = TRUE)

# run a right tailed test
myfun(sample_data, mu = pop_mean, pop.sd = pop_sd, two.tailed = FALSE, left.tail = FALSE)

# 3. Use replicate or sapply (or replicate) to generate a sample 
# and do the test multiple times (say 1000). 
# Plot the histogram of p-values that you are getting when the Null is true. 
# Is your function calculating Type-I errors correctly?

num.trials <- 1000

result.vec.two.tailed <- replicate(num.trials, 
                                   myfun(rnorm(sample_size, mean = pop_mean, sd = pop_sd), 
                                         mu = pop_mean, 
                                         pop.sd = pop_sd, 
                                         two.tailed = TRUE, 
                                         left.tail = NULL)$p.value, 
                                   simplify = TRUE)
hist(result.vec.two.tailed)

result.vec.left.tailed <- replicate(num.trials, 
                                   myfun(rnorm(sample_size, mean = pop_mean, sd = pop_sd), 
                                         mu = pop_mean, 
                                         pop.sd = pop_sd, 
                                         two.tailed = FALSE, 
                                         left.tail = TRUE)$p.value, 
                                   simplify = TRUE)
hist(result.vec.left.tailed)


result.vec.right.tailed <- replicate(num.trials, 
                                    myfun(rnorm(sample_size, mean = pop_mean, sd = pop_sd), 
                                          mu = pop_mean, 
                                          pop.sd = pop_sd, 
                                          two.tailed = FALSE, 
                                          left.tail = FALSE)$p.value, 
                                    simplify = TRUE)
hist(result.vec.right.tailed)

# 4. Now assume your Null is false. Note: For type-II error calculation, 
# you need a specific assumption about the mean of the population from which the sample is taken. 
# You can assume that this mean is one tenth of one standard deviation above the Null mean.
# Calculate type-II errors both theoretically and by simulation as in step 3.

# test_data represents our test vector with population mean = pop_mean and population sd = pop_sd
# we assume that mean(test_data) is our null hypothesis in this case

# Any values above this rejection.mean will cause us to reject the null hypothesis
rejection.mean <- pop_mean + pop_sd/10
# For original population with mean pop_mean, rejection.mean represents a z-score of
z.pop <- (rejection.mean - pop_mean) / (pop_sd/sqrt(length(sample_data)))


# Assuming that Null (pop_mean) is False, we take mean(sample_data) as our Null hypothesis
# That is, we use a distribution with mean(sample_data)
z.type.one <- (rejection.mean - mean(sample_data))/(pop_sd/sqrt(length(sample_data)))
p.type.one <- pnorm(abs(z.type.one), lower.tail = FALSE)


# Type II error is the area to the left of rejection.mean on the original distribution
# with mean = pop.mean
p.type.two <- pnorm(z.type.one)
test.power <- 1 - p.type.two
# There is a probability = p.type.two that we will fail to reject the null hypothesis when we should

