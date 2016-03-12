# library(car)
# 
# test.data <- rnorm(1000)
# test.data
# 
# y <- rt(200, df = 5)
# qqnorm(y); qqline(y, col = 2)
# qqplot(y, rt(300, df = 5))
# 
# qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
# 
# View(y)

library(pastecs)

getwd()
setwd("~/Exploring and Analyzing Data/W203 Async/W203 Week 7")

dlf <- read.delim("DownloadFestival.dat", header = TRUE)

head(dlf)

hist.day1 <- ggplot(dlf, aes(day1)) + 
  options(legend.postion = "none") + 
  geom_histogram(aes(y = ..density..), 
                 colour = "black", 
                 fill = "white") + 
  labs(x = "Hygiene score on day 1", 
       y = "Density")

hist.day1

hist.day1 + stat_function(fun = dnorm, 
                          args = list(mean = mean(dlf$day1, na.rm = TRUE), 
                                      sd = sd(dlf$day1, na.rm = TRUE)), 
                          colour = "black", size = 1)


qqplot.day1 <- qplot(sample = dlf$day1, stat = "qq")
qqplot.day1


# day2 <- na.omit(dlf$day2)
dlf.complete.day2 <- dlf[complete.cases(dlf$day2),]

head(dlf.complete.day2)

hist(dlf.complete$day2)

hist.day2 <- ggplot(dlf.complete.day2, aes(day2)) + 
#  options(legend.postion = "none") + 
  geom_histogram(aes(y = ..density..), 
                 colour = "black", 
                 fill = "white") + 
  labs(x = "Hygiene score on day 2", 
       y = "Density")
hist.day2

hist.day2 + stat_function(fun = dnorm, 
                          args = list(mean = mean(dlf.complete.day2$day2), 
                                      sd = sd(dlf.complete.day2$day2)), 
                          colour = "black", size = 1)


qqplot.day2 <- qplot(sample = dlf.complete.day2$day2, stat = "qq")
qqplot.day2


hist.day3 <- ggplot(dlf, aes(day3)) + 
  #  options(legend.postion = "none") + 
  geom_histogram(aes(y = ..density..), 
                 colour = "black", 
                 fill = "white") + 
  labs(x = "Hygiene score on day 3", 
       y = "Density")
hist.day3

hist.day3 + stat_function(fun = dnorm, 
                          args = list(mean = mean(dlf$day3, na.rm = TRUE), 
                                      sd = sd(dlf$day3, na.rm = TRUE)), 
                          colour = "black", size = 1)


qqplot.day3 <- qplot(sample = dlf$day2, stat = "qq")
qqplot.day3

describe(dlf$day1)



stat.desc(dlf$day1, basic = FALSE, norm = TRUE)

describe(cbind(dlf$day1, dlf$day2, dlf$day3))

stat.desc(cbind(dlf$day1, dlf$day2, dlf$day3), basic = FALSE, norm = TRUE)


describe(dlf[, c("day1", "day2", "day3")])
stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)

stat.des <- stat.desc(dlf[, c("day1", "day2", "day3")], basic = FALSE, norm = TRUE)
class(stat.des)


rexam <- read.delim("rexam.dat", header = TRUE)

rexam$uni <- factor(rexam$uni, levels = c(0:1), 
                    labels = c("Duncetown University", "Sussex University"))


describe(rexam[,c("exam", "computer", "lectures", "numeracy")])
stat.desc(rexam[,c("exam", "computer", "lectures", "numeracy")], basic = FALSE, norm = TRUE)

by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)
by(rexam$exam, rexam$uni, stat.desc)
by(rexam$exam, rexam$uni, stat.desc, basic = FALSE, norm = TRUE)
by(cbind(rexam$exam, rexam$numeracy), rexam$uni, stat.desc, basic = FALSE, norm = TRUE)
by(rexam[, c("exam", "numeracy")], rexam$uni, stat.desc, basic = FALSE, norm = TRUE)
by(rexam[, c("exam", "numeracy")], rexam$uni, describe)

dunce.data <- subset(rexam, rexam$uni=="Duncetown University")
sussex.data <- subset(rexam, rexam$uni=="Sussex University")

hist.numeracy.duncetown <- ggplot(dunce.data, aes(numeracy)) + 
  # opts(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black", binwidth = 1) + 
  labs(x = "Numeracy Score", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(dunce.data$numeracy, na.rm = TRUE), 
                                         sd = sd(dunce.data$numeracy, na.rm = TRUE)), 
                colour = "blue", size = 1)

hist.numeracy.duncetown

hist.exam.duncetown <- ggplot(dunce.data, aes(exam)) + 
  # opts(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black", binwidth = 1) + 
  labs(x = "Exam Score", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(dunce.data$exam, na.rm = TRUE), 
                                         sd = sd(dunce.data$exam, na.rm = TRUE)), 
                colour = "blue", size = 1)

hist.exam.duncetown


hist.numeracy.sussex <- ggplot(sussex.data, aes(numeracy)) + 
  # opts(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black", binwidth = 1) + 
  labs(x = "Numeracy Score", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(sussex.data$numeracy, na.rm = TRUE), 
                                         sd = sd(sussex.data$numeracy, na.rm = TRUE)), 
                colour = "blue", size = 1)

hist.numeracy.sussex

hist.exam.sussex <- ggplot(sussex.data, aes(exam)) + 
  # opts(legend.position = "none") + 
  geom_histogram(aes(y = ..density..), fill = "white", color = "black", binwidth = 1) + 
  labs(x = "Exam Score", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(sussex.data$exam, na.rm = TRUE), 
                                         sd = sd(sussex.data$exam, na.rm = TRUE)), 
                colour = "blue", size = 1)

hist.exam.sussex

shapiro.test(rexam$exam)
shapiro.test(rexam$numeracy)

by(rexam$exam, rexam$uni, shapiro.test)
by(rexam$numeracy, rexam$uni, shapiro.test)

qplot(sample = rexam$exam, stat = "qq")
qplot(sample = rexam$numeracy, stat = "qq")

leveneTest(rexam$exam, rexam$uni)
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)

