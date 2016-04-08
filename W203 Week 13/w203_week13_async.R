getwd()
setwd("W203 Week 13")

hap <- read.csv("Happiness.csv")
head(hap)
table(names(hap))
summary(hap)

# I will use the frequency of being outdoors as the independent variable
hap1 <- hap[, c(11, 15)]
head(hap1)
names(hap1) <- c("outdoors", "selflove")


# a: conduct a correlation or t-test
# ?cor.test
cor.test(x = hap1$outdoors, y = hap1$selflove, alternative = "two.sided")

# we see that outdoors has a weak positive relationship with self-love, r  = 0.31, p < 0.01

# ?pairwise.t.test()
pairwise.t.test(hap1$selflove, hap1$outdoors, p.adjust.method = "bonferroni")

# b: produce a scatterplot
library(car)
scatterplot(hap1$outdoors, hap1$selflove, jitter = list(x  = 1, y = 1))

# plot(hap1$outdoors, hap1$selflove)

# c. run a linear regression

hap1model <- lm(selflove ~ outdoors, data  = hap1, na.action = na.exclude)
summary(hap1model)
plot(hap1model)

