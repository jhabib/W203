getwd()
setwd("W203 Week 12")

load("Countries3.Rdata")
head(Countries)
summary(Countries)

library(car)

scatterplot(Countries$gdp, Countries$internet_users_2011)

rownames(Countries) <- Countries$Country
rownames(Countries)

model <- lm(internet_users_2011 ~ gdp, data = Countries)
summary(model)

plot(model)

# Test for Large Residuals
outlierTest(model)

# Durbin-Watson Test
dwt(model)

library(lmtest)

# Breusch-Pagan Tesst
bptest(model)


install.packages("sandwich")
library(sandwich)

# Heteroskedasticity-robust Residuals
coeftest(model, vcov = vcovHC)

Countries$loggdp <- log(Countries$gdp)
rownames(Countries) <- Countries$Country

scatterplot(x = Countries$loggdp, y = Countries$internet_users_2011)
logmodel <- lm(internet_users_2011 ~ loggdp, data = Countries)
logmodel
summary(logmodel)

plot(logmodel)

outlierTest(logmodel)
outlierTest(model)
dwt(logmodel)
dwt(model)

bptest(logmodel)
bptest(model)

coeftest(logmodel, vcov = vcovHC)
coeftest(model, vcov = vcovHC)
