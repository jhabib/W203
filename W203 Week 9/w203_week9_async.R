getwd()

setwd("W203 Week 9")

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

library(foreign)

insurgency.stata <- read.dta("lyall2010.dta")


View(insurgency.stata)

unique(insurgency.stata$occ)
unique(insurgency.stata$wdl)

summary(insurgency.stata)


cor(insurgency.stata$occ, insurgency.stata$wdl)


aggregate(insurgency.stata$wdl ~ insurgency.stata$occ, data = insurgency.stata, FUN = length)


