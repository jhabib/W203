getwd()
setwd("W203 Week 13")

album2 <- read.delim("Album Sales 2.dat", header = TRUE)

as1 <- lm(sales ~ adverts, data = album2)
summary(as1)

as2 <- lm(sales ~ adverts + airplay + attract, data = album2)
summary(as2)

plot(as2)

install.packages("QuantPsyc")
library(QuantPsyc)

# convert coefficients (b-values) from as2 to standardized Beta values 
# using lm.beta()
lm.beta(as2)
as2$coefficients[c("adverts", "airplay", "attract")]
# standardized coefficients show the number of standard deviations change 
# in outcome variable when predictor variable is changed by one 
# standard deviation

# calculate confidence interval of coefficients using confint()
confint(as2)

# comparing models using F-Ratio
# F = (N - k - 1)*R^2/k*(1 - R^2)
# R-square is the change in R-square from one model to next
fas1 <- (200 - 1 - 1)*0.334848/(1*(1 - 0.334648))
fas1

# k in the denominator is the change in number of predictors
fas2 <- (200 - 3 - 1)*(0.664668 - 0.334848)/(2*(1 - 0.664668))
fas2

# comparing two models
anova(as1, as2)

# explore outliers and influential cases
album2$residuals <- resid(as2)
album2$standardized.residuals < rstandard(as2)
album2$studentized.residuals <- rstudent(as2)
album2$cooks.distance <- cooks.distance(as2)
album2$dfbeta <- dfbeta(as2)
album2$dffit <- dffits(as2)
# leverage aka hat value
album2$leverage <- hatvalues(as2)
album2$covariance.ratio <- covratio(as2)

head(album2)

write.table(album2, "Album Sales With Diagnostics.dat", sep = "\t", row.names = FALSE)

rs <- rstandard(as2)
album2$standardized.residuals <- rs

# get standardized.residuals outside of +2, -2
album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2
sum(album2$large.residual)

album2[album2$large.residual, c("cooks.distance", "leverage", "covariance.ratio")]

# assess independent errors using Durbin Watson Test
# ??"durbinWatsonTest"
library(car)
durbinWatsonTest(as2)
# ??"dwt"
library(lmtest)
dwt(as2)
# D-W Statistic is close to 2 and p > 0.05
# so our errors are independent

# assessing multicollinearity of predictors
vif(as2)
max(vif(as2)) # max vif < 10

# tolerance
1/vif(as2) # tolerance > 2

#average vif
mean(vif(as2)) # average vif slightly > 1

# our model does not seem to be affected by multicollinearity of predictors

# checking assumption about residuals
plot(album2$standardized.residuals)
hist(album2$standardized.residuals)

plot(as2)

hist(as2$fitted.values)
hist(album2$studentized.residuals)

plot(x = as2$fitted.values, y = album2$standardized.residuals)

# bootstrapping robust regression
library(boot)
bootReg <- function(formula, data, i) {
  coef(lm(formula, data[i,]))
}

bootResults <- boot(statistic = bootReg, 
                    formula = sales ~ adverts + airplay + attract, 
                    data = album2, 
                    R = 2000)
bootResults
boot.ci(bootResults, type = "bca", index = 1)
boot.ci(bootResults, type = "bca", index = 2)        
boot.ci(bootResults, type = "bca", index = 3)
boot.ci(bootResults, type = "bca", index = 4)

# categorical predictors and multiple regression
gfr <- read.delim("GlastonburyFestivalRegression.dat", header = TRUE)
head(gfr)

#??"constrasts"
contrasts(gfr$music) <- contr.treatment(4, base = 4)
gfr$music

as3 <- lm(change ~ music, data = gfr)
summary(as3)
