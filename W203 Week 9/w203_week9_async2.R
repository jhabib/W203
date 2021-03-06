getwd()
setwd("W203 Week 9")

install.packages("akima")
install.packages("WRS", repos="http://R-Forge.R-project.org")
install.packages("pastecs")

library(ggplot2)
library(pastecs)
library(WRS)

spiderLong <- read.delim("SpiderLong.dat", header = TRUE)
head(spiderLong)

?geom_errorbar

spiderLong.summary <- aggregate(spiderLong$Anxiety ~ spiderLong$Group, FUN = mean)
spiderLong.summary

ggplot(spiderLong.summary, aes(x = spiderLong.summary$`spiderLong$Group`, 
                               y = spiderLong.summary$`spiderLong$Anxiety`)) + 
  geom_bar(position = position_dodge(), stat = "identity")

spider.summary <- data.frame(group = levels(spiderLong$Group), 
                             mean = tapply(spiderLong$Anxiety, spiderLong$Group, mean), 
                             n = tapply(spiderLong$Anxiety, spiderLong$Group, length), 
                             sd = tapply(spiderLong$Anxiety, spiderLong$Group, sd))

spider.summary

spider.summary$sem <- spider.summary$sd/sqrt(spider.summary$n)
spider.summary$me <- qt(1 - 0.05/2, df = spider.summary$n)*spider.summary$sem

ggplot(spider.summary, aes(x = spider.summary$group, y = spider.summary$mean)) + 
  geom_bar(position = position_dodge(), stat = "identity", fill = "coral") + 
  geom_errorbar(aes(ymin = mean-me, ymax = mean+me))


spiderWide <- read.delim("SpiderWide.dat", header = TRUE)
head(spiderWide)

spiderWide$participant <- c(1:length(spiderWide$picture))

row.names(spiderWide) <- NULL

spiderW.summary <- data.frame(pmean = mean(spiderWide$picture),
                              psd = sd(spiderWide$picture), 
                              pn = length(spiderWide$picture), 
                              psem = sd(spiderWide$picture)/sqrt(length(spiderWide$picture)), 
                              pme = qt(1 - 0.05/2, df = length(spiderWide$picture)) * sd(spiderWide$picture)/sqrt(length(spiderWide$picture)), 
                              rmean = mean(spiderWide$real), 
                              rn = length(spiderWide$real), 
                              rsem = sd(spiderWide$real)/sqrt(length(spiderWide$real)), 
                              rme = qt(1 - 0.05/2, df = length(spiderWide$real)) * sd(spiderWide$real)/sqrt(length(spiderWide$real)))

sw.summary <- as.data.frame(matrix(ncol = 6, nrow = 0))
sw.summary
colnames(sw.summary) <- c("group", "mean", "n", "sd", "sem", "me")

options(stringsAsFactors = FALSE)
sw.summary <- rbind(sw.summary, c("group" = "picture", 
                                  "mean" = mean(spiderWide$picture),
       "sd" = sd(spiderWide$picture), 
       "n" = length(spiderWide$picture), 
       "sem" = sd(spiderWide$picture)/sqrt(length(spiderWide$picture)), 
       "me" = qt(1 - 0.05/2, df = length(spiderWide$picture)) * sd(spiderWide$picture)/sqrt(length(spiderWide$picture))))

sw.summary <- rbind(sw.summary, c("real", 
                                  mean(spiderWide$real),
                                  sd(spiderWide$real), 
                                  length(spiderWide$real), 
                                  sd(spiderWide$real)/sqrt(length(spiderWide$real)), 
                                  qt(1 - 0.05/2, df = length(spiderWide$real)) * sd(spiderWide$real)/sqrt(length(spiderWide$real))))


sw.summary[2, 2:6] <- as.numeric(sw.summary[2, 2:6])

# ggplot(data = sw.summary, aes(x = sw.summary$group, y = as.numeric(sw.summary$mean))) + 
#   geom_bar(stat = "identity", position = position_dodge(), fill = "indianred") + 
#   geom_errorbar(aes(ymin = as.numeric(sw.summary$mean) - as.numeric(sw.summary$me), ymax = as.numeric(sw.summary$mean) + as.numeric(sw.summary$me)))


# spiderw.long <- reshape(spiderWide, varying = c("picture", "real"), idvar = "participant", direction = "long")

library(reshape2)
spiderW.long <- melt(spiderWide, id.vars = c("participant"))
spiderW.long


spiderW.summary <- data.frame(group = levels(spiderW.long$variable), 
                             mean = tapply(spiderW.long$value, spiderW.long$variable, mean), 
                             n = tapply(spiderW.long$value, spiderW.long$variable, length), 
                             sd = tapply(spiderW.long$value, spiderW.long$variable, sd))
spiderW.summary$sem <- spiderW.summary$sd/sqrt(spiderW.summary$n)
spiderW.summary$me <- qt(1 - 0.05/2, df = spiderW.summary$n)*spiderW.summary$sem
spiderW.summary

ggplot(data = spiderW.summary, aes(x = group, y = mean)) + 
  geom_bar(stat = "identity", position = position_dodge(), fill = "blue") + 
  geom_errorbar(aes(ymin = mean - me, ymax = mean + me))


spiderWide

spiderWide$pMean <- (spiderWide$picture + spiderWide$real) / 2
grandMean <- mean(c(spiderWide$picture, spiderWide$real))
grandMean

spiderWide$adj <-grandMean - spiderWide$pMean
spiderWide

spiderWide$picture_adj <- spiderWide$picture + spiderWide$adj
spiderWide$real_adj <- spiderWide$real + spiderWide$adj

spiderWide$pMean2 <- (spiderWide$picture_adj + spiderWide$real_adj) / 2

spiderWide

spiderWide.Long <- melt(spiderWide[, c(3, 6, 7)], id.vars = c("participant"))
spiderWide.Long


spiderWide.Long.Summary <- with(spiderWide.Long, 
                                data.frame(group = levels(variable), 
                                           mean = tapply(value, variable, mean), 
                                           n = tapply(value, variable, length), 
                                           sd = tapply(value, variable, sd)))
spiderWide.Long.Summary
spiderWide.Long.Summary$stderr <- spiderWide.Long.Summary$sd / sqrt(spiderWide.Long.Summary$n)
spiderWide.Long.Summary$me <- qt(1 - 0.05/2, df = spiderWide.Long.Summary$n) * spiderWide.Long.Summary$stderr


ggplot(spiderWide.Long.Summary, aes(x = group, y = mean)) + 
  geom_bar(stat = "identity", fill = "grey", position = position_dodge()) + 
  geom_errorbar(aes(ymin = mean - me, ymax = mean + me))

lm(data = spiderLong, formula = Anxiety ~ Group)

??"yuen"

yuen(spiderWide$real, spiderWide$picture)

yuenbt(spiderWide$real, spiderWide$picture, nboot = 2000, side = TRUE)

pb2gen(spiderWide$real, spiderWide$picture, alpha = 0.05, nboot = 2000, est = mom)

ind.t.test <- t.test(spiderWide$real, spiderWide$picture)
ind.t.test$statistic[[1]]
ind.t.test$parameter[[1]]

t <- ind.t.test$statistic[[1]]
df <- ind.t.test$parameter[[1]]

r <- sqrt(t^2 / (t^2 + df))
r
round(r, 3)

dep.t.test <- t.test(spiderWide$real, spiderWide$picture, paired = TRUE)
dep.t.test

ind.t.test

t <- dep.t.test$statistic[[1]]
t
df <- dep.t.test$parameter[[1]]
df
effect.size <- sqrt(t^2 / (t^2 + df))
effect.size
round(effect.size, 3)


board <- read.delim("Board&Fritzon(2005).dat")
View(board)

install.packages("gmodels")
install.packages("MASS")


library(gmodels)
library(MASS)

install.packages("clinfun")
install.packages("pgirmess")

library(clinfun)
library(ggplot2)
library(pastecs)
library(pgirmess)

