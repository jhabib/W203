getwd()
setwd("W203 Week 13")

# install.packages("faraway")

library(faraway)
data(eco)
plot(income ~ usborn, data = eco)
iub <- lm(income ~ usborn, data = eco)
summary(iub)
plot(income ~ usborn, data = eco, xaxs = "i")
abline(iub$coefficients)

# chicago data
data(chicago)
head(chicago)

ch <- data.frame(chicago[, c(1:4, 6)], income = chicago[ ,7]/1000)
head(ch)
summary(ch)

par(mfrow = c(2, 3))
for (i in 1:6) hist(ch[, i], main = names(ch)[i])
for (i in 1:6) boxplot(ch[, i], main = names(ch)[i])
pairs(ch)

?pairs

summary(lm(involact ~ race, data = ch))

g <- lm(involact ~ race + fire + theft + age + log(income), data = ch)
summary(g)

plot(g$fitted.values, g$residuals)
abline(h = 0)
qqnorm(g$residuals)

gi <- lm.influence(g)
for (i in 1:5) qqnorml(gi$coefficients[, i+1], main = names(ch)[-5][1])

qqnorml(rstudent(g), main = "Jacknife Residuals")
qt(0.05/(2*47), 47-6-1)

halfnorm(cooks.distance(g), main = "Cook's Distance")

ch[c(6, 24), ]


g2 <- lm(involact ~ race + fire + theft + age + log(income), data = ch, subset = (1:47)[-c(6, 24)])
summary(g2)

g3 <- lm(involact ~ race + poly(fire, 2) + poly(theft, 2) + poly(age, 2) + poly(log(income), 2), 
         data = ch, subset = (1:47)[-c(6, 24)])
summary(g3)

anova(g2, g3)
