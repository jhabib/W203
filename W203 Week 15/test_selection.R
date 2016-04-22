getwd()
# setwd("W203 Week 15")

d <- read.csv("Dating.csv", header = TRUE)
head(d)
# summary(d)
# names(d)
View(d)

summary(d$marital_status)
summary(d$use_reddit)
chisq.test(d$marital_status, d$use_reddit)
table(d$marital_status, d$use_reddit)

levels(d$marital_status)
levels(d$use_reddit)

levels(d$region)
summary(d$region) # categorical

unique(d$life_quality) # interval, continuous
hist(as.numeric(d$life_quality))

table(d$life_quality, d$region) # ANOVA or Kruskal-Wallis

unique(d$flirted_online) # independent, categorical
unique(d$years_in_relationship) # dependent, numeric
hist(as.numeric(d$years_in_relationship))
summary(d$flirted_online)

cor.test(x = as.numeric(d$flirted_online), as.numeric(d$age))
a <- aov(age ~ flirted_online, data  = d)
summary(a)
plot(a)

t.test(d$flirted_online, d$age) # cannot recode

summary(d$lgbt)
summary(d$adults_in_household)

chisq.test(d$lgbt, d$adults_in_household)
table(d$lgbt, d$adults_in_household)

a2 <- aov()
