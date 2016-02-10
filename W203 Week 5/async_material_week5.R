library(ggplot2)

setwd("C:/Users/SP4/Documents/Exploring and Analyzing Data/W203 Async/W203 Week 5/")
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

write.csv(Countries, "Countries.csv")

summary(Countries)

# demonstration of histogram
graph1 <- ggplot(Countries, aes(gdp))
graph1 + geom_histogram()

graph1 + geom_histogram() + scale_x_log10()

graph1 + geom_histogram(binwidth = .2) + scale_x_log10()


# investigation of corruption and growth of internet users
Corruption <- read.csv("2012_Corruption_Index.csv")
View(Corruption)
summary(Corruption)

Countries <- merge(Countries, Corruption, by = "Country", all = TRUE)
View(Countries)

Countries$internet_growth <- (Countries$internet_users_2011 - Countries$internet_users_2010)/(Countries$internet_users_2010)

graph2 <- ggplot(Countries, aes(internet_growth))

graph2 + geom_histogram()

graph3 <- ggplot(Countries, aes(cpi, internet_growth))
graph3 + geom_point(shape = 4)
graph3 + geom_point() + scale_y_log10()

# recode corruption index as a binary variable
Countries$high_cpi <- Countries$cpi > mean(Countries$cpi, na.rm = TRUE)
Countries$high_cpi <- ifelse(Countries$cpi > mean(Countries$cpi, na.rm = TRUE), "Trustworthy", "Corrupt")
Countries$high_cpi

graph4 <- ggplot(Countries[!is.na(Countries$high_cpi),], aes(high_cpi, internet_growth))
graph4 + geom_boxplot()

graph4 +  stat_summary(fun.y = mean, 
                       geom = "bar", 
                       fill = "White", 
                       colour = "Black") + stat_summary(fun.data = mean_cl_normal, 
                                                        geom = "errorbar", 
                                                        width = 0.2)

# investigate the effect of high and low gdp
# code a new high_gdp variable
Countries$high_gdp <- ifelse(Countries$gdp > mean(Countries$gdp, na.rm = TRUE), "High", "Low")
Countries$high_gdp
Countries$high_gdp <- factor(Countries$high_gdp)
Countries_lim <- Countries[!(is.na(Countries$high_gdp) | is.na(Countries$high_cpi)), ]

# create bar chart of internet growth against high corruption and high gdp
graph5 <- ggplot(Countries_lim, aes(high_gdp, internet_growth, fill = high_cpi))
graph5 + stat_summary(fun.y = mean, geom = "bar", colour = "black", position = "dodge") + 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))

# create scatter plot of internet growth by internet users and high cpi

graph6 <- ggplot(Countries_lim, aes(internet_users_2010, internet_growth, colour = high_cpi))
graph6 + geom_point() + geom_smooth()
