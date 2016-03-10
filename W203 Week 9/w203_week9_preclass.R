getwd()
setwd("W203 Week 9")

senate.data <- read.csv("united_states_senate_2014.csv", header = TRUE)
head(senate.data)
# View(senate.data)
summary(senate.data)

# Study the data and then for each of the questions below, form a hypothesis, 
# choose the appropriate test, run the test, and report your results. 
# (report two-tailed p-values)

# 1. Is there a difference between the amount of money a senator raises 
# and the amount spent?

# H0 : there is no correlation between the amount raised and spent

senate.expense <- senate.data[, c("Senator.Names", 
                                   "Campaign.Money.Raised..millions.of...", 
                                   "Campaign.Money.Spent..millions.of...")]
names(senate.expense) <- c("Senator", "Raised.Millions", "Spent.Millions")
head(senate.expense)

senate.expense$Expense.Delta <- senate.expense$Spent.Millions - senate.expense$Raised.Millions

# create a plot for Raised.Millions vs. Spent.Millions

# plot(x = senate.expense$Raised.Millions, y = senate.expense$Spent.Millions)

library(ggplot2)

ggplot(data = senate.expense, aes(x = Senator, y = Expense.Delta, 
                                  fill = ifelse(Expense.Delta > 0, "red", "blue"))) + 
  geom_bar(stat = "identity", position = position_dodge())

ggplot(data = senate.expense, aes(x = Expense.Delta)) + 
         geom_histogram(colour = "black", fill = "white", binwidth = 0.5)

ggplot(data = senate.expense, aes(x = Raised.Millions)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 2)

ggplot(data = senate.expense, aes(x = Spent.Millions)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 2)

expense.plot <- ggplot(data = senate.expense) + 
  geom_point(aes(x = Raised.Millions, y = Spent.Millions))
expense.plot

expense.lm <- lm(senate.expense$Spent.Millions ~ senate.expense$Raised.Millions)
expense.lm

# Let's use a repeated-measures design because the same Senator 
# Raised and Spent the money
expense.dependent.t.test <- t.test(senate.expense$Raised.Millions, 
                                   senate.expense$Spent.Millions, 
                                   paired = TRUE, 
                                   alternative = "two.sided")
expense.dependent.t.test
# we see from the above that t(99) = 5.99, p < 0.01.
# we can reject the null hypothesis that there is no difference between 
# mean values of Raised.Millions and Spent.Millions
# t > 0 tells us that senators raised more money than they spent


# 2. Do female Democratic senators raise more or less money than 
# female Republican senators?

# our data is in long form
# we will use independent t-test because democratic and republican  
# female senators are different populations 

senate.party <- senate.data[senate.data$"Gender" == "Female", 
                            c("Senator.Names", "Party", 
                              "Campaign.Money.Raised..millions.of...")]
names(senate.party) <- c("Senator", "Party", "Raised.Millions")
head(senate.party)

party.independent.t.test <- t.test(Raised.Millions ~ Party, 
                                   data = senate.party, 
                                   paired = FALSE,
                                   alternative = "two.sided")
party.independent.t.test
# we see from the above that t(17.10) = 3.25, p < 0.01.
# we can reject the null hypothesis that there is no difference between 
# the money raised by female democrat and female republican senators
# t > 0 tells us that female democrat senators raised more money than 
# female republican senators

# 3. Do protestant Senators spend more or less money than 
# non-protestant senators?
senate.religion <- senate.data[, c("Senator.Names", "Religion", 
                              "Campaign.Money.Spent..millions.of...")]
names(senate.religion) <- c("Senator", "Religion", "Spent.Millions")
senate.religion$Religion <- ifelse(senate.religion$Religion == "Protestant", 
                                   "Protestant", "Non-Protestant")
head(senate.religion)

religion.independent.t.test <- t.test(Spent.Millions ~ Religion, 
                                      data = senate.religion, 
                                      paired = FALSE, 
                                      alternative = "two.sided")
religion.independent.t.test
# we see from the above that t(97.17) = 1.29, p > 0.05
# we cannot reject the null hypothesis that there is no difference in means of 
# Spent.Millions between Protestant and Non-Protestant senators

