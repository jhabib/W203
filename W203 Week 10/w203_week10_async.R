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
  stat_summary(fun.y = mean, geom = "line", size = 1, aes(group = 1)) + 
  stat_summary(fun.y = mean, geom = "point", size = 4, aes(group = 1)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")
  # geom_errorbar(aes(ymin = min(v.means$libido), ymax = max(v.means$libido)))
libido.plot
