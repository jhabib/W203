library(ggplot2)

load("Countries")

setwd("C:/Users/SP4/Google Drive/Exploring and Analyzing Data/W203 Async/W203 Week 5/")
getwd()

library(datasets)

library(car)

data()

data(UN, package="car")
UN
UN$Country <- row.names(UN)
row.names(UN) <- NULL

load("InternetUsers.RData")
InternetUsers

Countries <- merge(UN, InternetUsers, by = "Country", all = TRUE)

View(Countries)

Fertility <- read.table("Fertility.csv", header = TRUE, sep=",")
View(Fertility)

Countries <- merge(Countries, Fertility, by = "Country", all = TRUE)
View(Countries)
