getwd()

setwd("~/Exploring and Analyzing Data/W203 Async/W203 Week 9")

adverts <- c(5,4,4,6,8)
packets <- c(8,9,10,13,15)
advertData <- data.frame(adverts, packets)
plot(adverts, packets)

library(boot)
library(ggm)
library(ggplot2)
library(Hmisc)
library(polycor)

examData <- read.delim("Exam Anxiety.dat", header = TRUE)
View(examData)

class(examData)

cor(examData, use = "complete.obs", method = "pearson")


cor(examData)
cor(examData[sapply(examData, is.numeric)])

cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "pearson")
cor(examData$Exam, examData$Anxiety, use = "complete.obs", method = "kendall")
cor(examData$Exam, examData$Anxiety, use = "pairwise.complete.obs", method = "kendall")

# rcorr(examData[1:4], type = "pearson")
rcorr(as.matrix(examData[, 1:4]), type = "pearson")

cor.test(examData$Exam, examData$Anxiety, method = "pearson")

cor.test(examData$Exam, examData$Anxiety, alternative = "less", method = "pearson")

cor.test(examData$Exam, examData$Anxiety, alternative = "less", method = "pearson", conf.level = 0.99)

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]
cor(examData2)

cor(examData[, c("Exam", "Anxiety", "Revise")])

examMatrix <- as.matrix(examData[, c("Exam", "Anxiety", "Revise")])

rcorr(examMatrix)

cor.test(examData$Anxiety, examData$Exam)

cor(examData2)^2

cor(examData2)

cor(examData2)^2*100


liarData <- read.delim("The Biggest Liar.dat", header = TRUE)

liarData = read.delim(file.choose(), header = TRUE)

cor(liarData$Position, liarData$Creativity, method = "spearman")

liarMatrix <- as.matrix(liarData[, c("Position", "Creativity")])

rcorr(liarMatrix)

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "spearman")

cor(liarData$Position, liarData$Creativity, method = "kendall")

cor.test(liarData$Position, liarData$Creativity, alternative = "less", method = "kendall")

library(boot)

bootTau <- function(liarData, i) {
  cor(liarData$Position[i], 
      liarData$Creativity[i], 
      use = "complete.obs", 
      method = "kendall")
}

boot_kendall <- boot(liarData, bootTau, 2000)

boot_kendall

boot.ci(boot_kendall)

names(examData2)

catData <- read.csv("pbcorr.csv", header = TRUE)

head(catData)

cor(catData$time, catData$gender, method = "pearson")

cor.test(catData$time, catData$gender)

cor(catData$time, catData$recode, method = "pearson")

cor.test(catData$time, catData$recode, method = "pearson")

# Calculate a table of male and female cat frequencies
catFrequencies <- table(catData$gender)
catFrequencies

# Create a proportion table of cat frequencies
prop.table(catFrequencies)

z <- qnorm(0.467, lower.tail = FALSE)
z

y <- dnorm(z)
y

r.biserial <- 0.378 * sqrt(0.533*0.467) / 0.397
r.biserial

polyserial(catData$time, catData$gender)

std.err.r.biserial <- sqrt(0.533*0.467) / (y * sqrt(60))
std.err.r.biserial

z.r.biserial <- (r.biserial - 0) / std.err.r.biserial

z.r.biserial

pnorm(z.r.biserial, lower.tail = FALSE)

examData <- read.delim("Exam Anxiety.dat", header = TRUE)

examData2 <- examData[, c("Exam", "Anxiety", "Revise")]

library(ggm)

pcor(c("Exam", "Anxiety", "Revise"), var(examData2))

pc <- pcor(c("Exam", "Anxiety", "Revise"), var(examData2))
pc

pc^2

pcor.test(pc, 1, 103)

library(gmodels)
library(MASS)

catDataNew <- read.delim("cats.dat", header = TRUE)
catDataNew

food <- c(10, 28)
affection <- c(114, 48)

catsTable <- cbind(food, affection)
catsTable

CrossTable(catDataNew$Training, catDataNew$Dance, fisher = TRUE, 
           chisq = TRUE, expected = TRUE, sresid = TRUE, 
           format = "SPSS")

CrossTable(catsTable, fisher = TRUE, chisq = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS")

###################################### week 8 class
library(foreign)

insurgency.stata <- read.dta("lyall2010.dta")


View(insurgency.stata)

unique(insurgency.stata$occ)
unique(insurgency.stata$wdl)

summary(insurgency.stata)


cor(insurgency.stata$occ, insurgency.stata$wdl)


aggregate(insurgency.stata$wdl ~ insurgency.stata$occ, data = insurgency.stata, FUN = length)


