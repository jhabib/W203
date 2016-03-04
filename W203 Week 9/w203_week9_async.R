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

