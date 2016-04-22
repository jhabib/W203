getwd()
setwd("W203 Week 15")
install.packages("PASWR")
library(PASWR)

?titanic3

df <- titanic3
head(df)
summary(df)

?glm

table(survi)

m1 <- glm(survived ~ parch, data = df, family = binomial)
summary(m1)

m1.chi <- m1$null.deviance - m1$deviance
m1.chi
m1.df <- m1$df.null - m1$df.residual
m1.df
m1.prob <- 1 - pchisq(m1.chi, m1.df)
m1.prob

# chi square(1) = 2.11, p > 0.05

m1.rsq <- m1.chi/m1$null.deviance
m1.rsq
# practically and statistically not significant

table(df$survived, df$parch)

names(df)
table(df$pclass)
library(plyr)
df$pclassrecode <- revalue(df$pclass, c("1st" = 1, "2nd" = 2, "3rd" = 3))
df$pclassrecode
levels(df$pclass)

df$pclassrecode <- factor(df$pclassrecode, c("1st", "2nd", "3rd"))
levels(df$pclassrecode)

m2 <- glm(survived ~ parch + as.numeric(pclassrecode), data = df, na.action = na.omit)
summary(m2)

m2.chi <- m2$null.deviance - m2$deviance
m2.chi
m2.df <- m2$df.null - m2$df.residual
m2.df
m2.prob <- 1 - pchisq(m2.chi, m2.df)
m2.prob

# chi square(2) = 32.59, p < 0.01
m2.rsq <- m2.chi/m2$null.deviance
m2.rsq
# addition of class makes the model statistically significant
# 

m3 <- glm(survived ~ parch + I(parch^2), data = df, family = binomial)
summary(m3)

m3.chi <- m3$null.deviance - m3$deviance
m3.chi
m3.df <- m3$df.null - m3$df.residual
m3.df
m3.prob <- 1 - pchisq(m3.chi, m3.df)
m3.prob

# chi square(3) = 37.12, p < 0.01
m3.rsq <- m3.chi/m3$null.deviance
m3.rsq

cbind(m1$aic, m2$aic, m3$aic)
