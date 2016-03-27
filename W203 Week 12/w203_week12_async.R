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

# Durbin-Watson Test for autocorrelations
dwt(model)

library(lmtest)

# Breusch-Pagan Test for Heteroscedasticity
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

install.packages("QuantPsyc")
library(QuantPsyc)
library(boot)

album1 <- read.delim("Album Sales 1.dat", header = TRUE)
head(album1)

albumSales.1 <- lm(sales ~ adverts, data = album1)
summary(albumSales.1)
plot(albumSales.1)
summaryAlbumSales.1 <- summary(albumSales.1)
sqrt(summaryAlbumSales.1$r.squared)

#scatter plot of album sales vs album adverts
scatterplot(x = album1$adverts, y = album1$sales)
