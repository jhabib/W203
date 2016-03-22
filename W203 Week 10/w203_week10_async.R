getwd()
setwd("W203 Week 10")

# One way ANOVA using R

install.packages("car")
install.packages("compute.es")
install.packages("multcomp")
install.packages("WRS", repos = "http://R-Forge.R-project.org")

library(car)
library(compute.es)
library(ggplot2)
library(pastecs)
library(multcomp)
library(WRS)
library(boot)

libido <- c(3, 2, 1, 1, 4, 5, 2, 4, 2, 3, 7, 4, 5, 3, 6)
dose <- gl(3, 5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData <- data.frame(dose, libido)
head(viagraData)

v.means <- aggregate(libido ~ dose, data = viagraData, mean)



libido.plot <- ggplot(data = viagraData, aes(x = dose, y = libido)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = 1), colour = "royalblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 4, aes(group = 1), colour = "royalblue") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "royalblue") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")
  # geom_errorbar(aes(ymin = min(v.means$libido), ymax = max(v.means$libido)))
libido.plot

libido.sum <- by(viagraData$libido, viagraData$dose, stat.desc)
libido.sum

summary(viagraData)

leveneTest(viagraData$libido, viagraData$dose, center = median)

viagraModel <- lm(libido ~ dose, data = viagraData)
viagraModel

viagraAOV <- aov(libido ~ dose, data = viagraData, na.action = na.exclude)
viagraAOV
summary(viagraAOV)
plot(viagraAOV)

viagraWelchF <- oneway.test(libido ~ dose, data = viagraData, na.action = na.exclude)
viagraWelchF
summary(viagraWelchF)

viagraWide <- unstack(viagraData, libido ~ dose)
viagraWide

t1way(viagraWide, tr = 0.1, grp = c(1:3))

med1way(viagraWide)

t1waybt(viagraWide, tr = 0.1, nboot = 599)

viagraT1bt <- t1waybt(viagraWide, tr = 0.1, nboot = 599)
viagraT1bt
table(viagraT1bt)


summary.lm(viagraModel)
summary.lm(viagraAOV)

contrasts(viagraData$dose) <- contr.helmert(3)
viagraData$dose

contrast1 <- c(-2, 1, 1)
contrast2 <- c(0, -1, -1)
contrasts(viagraData$dose) <- cbind(contrast1, contrast2)
viagraData$dose
viagraPlanned <- aov(libido ~ dose, data = viagraData)
summary.lm(viagraPlanned)
contrasts(viagraData$dose) <- cbind(contrast1, contrast2)

summary(viagraPlanned)

contrasts(viagraData$dose) <- contr.poly(3)
viagraTrend <- aov(libido ~ dose, data = viagraData)
summary.lm(viagraTrend)


pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "bonferroni")

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "BH")

tukeyModel <- glht(viagraModel, linfct = mcp(dose = "Tukey"))
summary(tukeyModel)
confint(tukeyModel)


dunnettModel <- glht(viagraModel, linfct = mcp(dose = "Dunnett"), base = 1)
summary(dunnettModel)
confint(dunnettModel)


lincon(viagraWide)
mcppb(viagraWide)
mcppb20(viagraWide)
