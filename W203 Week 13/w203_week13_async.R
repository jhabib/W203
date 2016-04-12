getwd()
setwd("W203 Week 13")

hap <- read.csv("Happiness.csv")
head(hap)
table(names(hap))
summary(hap)

# I will use the frequency of being outdoors as the independent variable
hap1 <- hap[, c(15, 4)]
head(hap1)
names(hap1) <- c("selflove", "age")


# a: conduct a correlation or t-test
# ?cor.test
hcor <- cor.test(x = hap1$age, y = hap1$selflove, alternative = "two.sided")

hcor$estimate^2

# we see that outdoors has a weak positive relationship with self-love, r  = 0.31, p < 0.01

# ?pairwise.t.test()
# pairwise.t.test(hap1$selflove, hap1$age, p.adjust.method = "bonferroni")

# b: produce a scatterplot
library(car)
scatterplot(hap1$age, hap1$selflove, jitter = list(x  = 1, y = 1))

# plot(hap1$outdoors, hap1$selflove)

# c. run a linear regression

hap1model <- lm(selflove ~ age, data  = hap1, na.action = na.exclude)
summary(hap1model)
plot(hap1model)

hist(hap1$selflove)
hist(hap1$age)
shapiro.test(log(hap1$age))

hist(log(hap1$age))

boxcox(hap1model)
