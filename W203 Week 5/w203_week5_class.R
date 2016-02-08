pop <- 1:6
pop
pop.pr <- rep(1/6, 6)

mu <- mean(pop)
mu

mu1 <- sum(pop * pop.pr)
mu1

sigma <- sqrt(mean(pop^2) - mean(pop)^2)
sigma

sample.size <- 30

s1 <- sample(x = pop, size = sample.size, replace = TRUE)
s1.mean <- mean(s1)
s1.sd <- mean(s1)
s1.hist <- hist(s1)
s1.hist

n.rep <- 10000

n.sample.size <- 1000

n.sample.means <- replicate(n.rep, mean(sample(pop, size = n.sample.size, replace = TRUE)))
hist(n.sample.means, breaks = seq(3,4, by=0.015))
abline(v = mean(n.sample.means), col = "red")


MakeItStd <- function(x, mu, samplesize) {
  (x - mu) / sigma * samplesize
}


standard <- MakeItStd(n.sample.means, mu, sigma)
hist(standard)
