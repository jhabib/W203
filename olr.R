getwd()
install.packages("MASS")
library(MASS)
?polr

options(contrasts = c("contr.treatment", "contr.poly"))
house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
house.plr1 <- polr(Sat ~ Infl + Type, weights = Freq, data = housing)
house.plr2 <- polr(Sat ~ Infl, weights = Freq, data = housing)

house.plr
house.plr1
house.plr2
s <- summary(house.plr, digits = 3)
s1 <- summary(house.plr1, digits = 3)
s2 <- summary(house.plr2, digits = 3)

s
s1
s2

## slightly worse fit from
summary(update(house.plr, method = "probit", Hess = TRUE), digits = 3)
## although it is not really appropriate, can fit
summary(update(house.plr, method = "loglog", Hess = TRUE), digits = 3)
summary(update(house.plr, method = "cloglog", Hess = TRUE), digits = 3)

predict(house.plr, housing, type = "p")
addterm(house.plr, ~.^2, test = "Chisq")
house.plr2 <- stepAIC(house.plr, ~.^2)
house.plr2$anova
anova(house.plr, house.plr2)

house.plr <- update(house.plr, Hess=TRUE)
pr <- profile(house.plr)
confint(pr)
plot(pr)
pairs(pr)
