s <- summary(GSS$income91)
class(s)
hist(s, freq = FALSE, breaks = 10)
?hist

income.art <- GSS[GSS$visitart == "Yes", 14]
hist(summary(income.art), freq = FALSE, breaks = 10)


income.notart <- GSS[GSS$visitart == "No", 14]
hist(summary(income.notart), freq = FALSE, breaks = 10)

names(GSS)

summary(GSS$country)
summary(GSS$age)

cmusic <- data.frame(GSS$age, GSS$country)
names(cmusic) <- c("age", "country")
levels(cmusic$country)
hist(cmusic$age)

library(car)
leveneTest(cmusic$age ~ cmusic$country)
cmusic$age
?aov

amodel <- aov(age ~ country, data=cmusic)
amodel
summary(amodel)

GSS$relig
levels(GSS$sex)
GSS$sibs

unique(GSS$sibs)

hist(GSS$educ[GSS$educ < 98], breaks = 20)
hist(GSS$tvhours[GSS$tvhours < 99])
summary(GSS$tvhours)

plot(GSS$educ, GSS$tvhours)
qqplot(GSS$educ, GSS$tvhours)


levels(GSS$politics)
levels(GSS$news)
hist(summary(GSS$politics))
hist(summary(GSS$news))


unique(GSS$age)
levels(GSS$country)
cmusic.complete <- cmusic[cmusic$country != c("NA", "NAP"),]

library(stats)

cmodel <- kruskal.test(age ~ country, data = cmusic.complete, na.action = na.exclude)
summary(cmodel)

cmusic.complete$ranks <- rank(cmusic.complete$age)
by(cmusic.complete$ranks, cmusic.complete$country, mean)

boxplot(cmusic.complete$age ~ cmusic.complete$country, na.action = na.exclude)
?boxplot

unique(cmusic.complete$country)

unique(GSS$sibs)
d <- data.frame(GSS$relig, GSS$sex, GSS$sibs)
head(d)
d <- d[d$GSS.relig == "Catholic",]
head(d)
d <- d[!(d$GSS.sibs %in% c(98, 99)),]
unique(d$GSS.sibs)
hist(d$GSS.sibs[d$GSS.sex == "Male"])

hist(d$GSS.sibs[d$GSS.sex == "Female"])


shapiro.test(d$GSS.sibs[d$GSS.sex == "Male"])

shapiro.test(d$GSS.sibs[d$GSS.sex == "Female"])



unique(GSS$educ)
unique(GSS$tvhours)
