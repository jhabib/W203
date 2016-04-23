getwd()
# setwd("C:/Users/SP4/Documents/Exploring and Analyzing Data/W203 Async/W203 Week 15")

d <- read.csv("Dating.csv", header = TRUE, stringsAsFactors = FALSE)
head(d)
# summary(d)
# names(d)
View(d)

#Q9 use_reddit vs marital_status ###############
summary(d$marital_status)
summary(d$use_reddit)
chisq.test(d$marital_status, d$use_reddit)
table(d$marital_status, d$use_reddit)

??"contingency"

vcd::structable(use_reddit ~ marital_status, data = d)
?CrossTable
library(gmodels)
CrossTable(d$marital_status, d$use_reddit)

levels(d$marital_status)
levels(d$use_reddit)

#Q10 life_quality vs region #################
levels(d$region)
summary(d$region) # categorical

unique(d$life_quality) # ordinal
hist(as.numeric(d$life_quality))

table(d$life_quality, d$region) # ANOVA or Kruskal-Wallis

d1 <- d[!d$life_quality %in% c("Refused", "Don't know", "NA"), c("life_quality", "region")]
d1 <- d1[!is.na(d1$life_quality) & !is.na(d1$region), ]

alr <- aov(as.numeric(life_quality) ~ region, data = d1)
summary(alr)

# Q11 flirted_online vs years_in_relationship ###############
unique(d$flirted_online) # independent, categorical
unique(d$years_in_relationship) # dependent, numeric
hist(as.numeric(d$years_in_relationship))
summary(d$flirted_online)

dfish <- d[!d$flirted_online %in% c(" ", "Refused", "Don't know", "NA"), c("years_in_relationship", "flirted_online")]
dfish <- dfish[complete.cases(dfish) & !dfish$years_in_relationship %in% c("Refused", "Don't know"), ]

?fisher.test
# fisher.test(d$years_in_relationship, factor(dfish$flirted_online))

dfish$years_in_relationship <- as.numeric(dfish$years_in_relationship)
dfish$flirted_online <- factor(dfish$flirted_online)

summary(dfish)

cor.test(x = as.numeric(d$flirted_online), as.numeric(d$years_in_relationship))
a <- aov(years_in_relationship ~ flirted_online, data  = dfish)
summary(a)
plot(a)

t.test(d$flirted_online, d$age) # cannot recode

# Q12. lgbt ~ adults_in_household
summary(d$lgbt)
summary(d$adults_in_household)

chisq.test(d$lgbt, d$adults_in_household)
table(d$lgbt, d$adults_in_household)

dso <- d[!d$lgbt %in% c("Don't know", "Refused", " ", "NA"), c("lgbt", "adults_in_household")]
dso <- dso[!dso$adults_in_household %in% c("Don't know", "Refused", " ", "NA"), ]
dso <- dso[complete.cases(dso), ]
dso$adults_in_household <- as.numeric(dso$adults_in_household)
summary(dso$adults_in_household)
hist(dso$adults_in_household)
dso$lgbt <- factor(dso$lgbt)
summary(dso$lgbt)
chisq.test(dso$adults_in_household, dso$lgbt)

??"CrossTable"
library(gmodels)
CrossTable(dso$adults_in_household, dso$lgbt)
?wilcox.test
wilcox.test(dso$adults_in_household ~ dso$lgbt)
adso <- aov(dso$adults_in_household ~ dso$lgbt)
summary(adso)

# We have homogeneity of variance
leveneTest(dso$adults_in_household, dso$lgbt)
hist(log(dso$adults_in_household))

# Q13. number of children ~ age ############### 
dac <- d[, c("age", "children0_5", "children6_11", "children12_17")]
dac[is.na(dac)] <- 0
head(dac)
dac <- dac[complete.cases(dac), ]
dac <- dac[dac$children0_5 < 98 & dac$children6_11 < 98 & dac$children12_17 < 98, ]
dac$totalchildren <- with(dac, children0_5 + children6_11 + children12_17)
apply(dac, 2, max)
hist(dac$age)
hist(dac$totalchildren)
mage <- lm(totalchildren ~ age, dac)
summary(mage)

mage2 <- lm(age ~ totalchildren, dac)
summary(mage2)
plot(dac$age, dac$totalchildren)
scatterplot(dac$age, dac$totalchildren)
wilage <- wilcox.test(dac$age, dac$totalchildren)
wilcox.test(dac$totalchildren ~ dac$age)
summary(wilage)
?wilcox.test

# Q14. 31 year old men or women have more children ############### 
dac2 <- d[, c("age", "sex", "children0_5", "children6_11", "children12_17")]
dac2[is.na(dac2)] <- 0
head(dac2)
dac2 <- dac2[complete.cases(dac2), ]
dac2 <- dac2[dac2$children0_5 < 98 & dac2$children6_11 < 98 & dac2$children12_17 < 98, ]
dac2$totalchildren <- with(dac2, children0_5 + children6_11 + children12_17)

dac2a <- dac2[dac2$age == 31, c("sex", "totalchildren")]
head(dac2a)
hist(dac2a$totalchildren)

wilcox.test(dac2a$totalchildren ~ dac2a$sex)
