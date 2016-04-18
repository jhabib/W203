getwd()
setwd("W203 Week 13")

# 0. Load the data
load("GSS_daughters.rdata")
head(GSS)

# view description

desc <- function(var, df) {
  attr(df, "var.labels")[match(var, colnames(df))]
}
desc("partyid", GSS)
desc("kdsex1", GSS)
levels(GSS$kdsex1)

cbind(names = colnames(GSS), labels = attr(GSS, "var.labels"))

# 1. Recode sex of first child (kdsex1) into dummy variable (daugter1)
table(GSS$kdsex1)

GSS$daughter1 <- ifelse(GSS$kdsex1 == "female", 1, 0)
table(GSS$daughter1)
GSS$daughter1 <- factor(GSS$daughter1, labels = c("false", "true"))

# convert partyid into a numeric variable
table(GSS$partyid)

GSS$pid1 <- as.numeric(GSS$partyid)
table(GSS$pid1)
GSS <- GSS[GSS$pid1 != 8,]

# remove obs. with partyid %in% c("other party", "dk", "na") from GSS
GSS <- GSS[!(GSS$partyid %in% c("other party", "dk", "na")), ]

# remove NA from partyid
GSS <- GSS[!is.na(GSS$partyid), ]

# remove NA from relig, this will come in handy later
GSS <- GSS[!is.na(GSS$relig), ]

table(GSS$partyid)


GSS$partyid1 <- factor(GSS$partyid, labels = c("strong democrat", "not str democrat", 
                                     "ind near dem", "independent", 
                                     "ind near rep", "not str republican", 
                                     "strong republican"))

levels(GSS$partyid1)
unique(factor(GSS$partyid))

# convert our reduced GSS$partyid to numeric
GSS$partyid1 <- as.numeric(factor(GSS$partyid1))
table(GSS$partyid1)
unique(GSS$partyid1)
# 2. linear regression partyid ~ daughter1
pm1 <- lm(pid1 ~ daughter1, data = GSS)
summary(pm1)
plot(pm1)

# 3. linear regression with partyid ~ daughter1 + age
pm2 <- lm(partyid ~ daughter1 + age, data = GSS)
summary(pm2)

# 3. linear regression with partyid ~ daughter1 + relig
table(GSS$relig)

unique(GSS$relig)

GSS$relig <- factor(GSS$relig, 
                    c("catholic", "protestant", "other", "jewish", "none"))
# use "none" as the base group
contrasts(GSS$relig) <- contr.treatment(5, base = 5)

pm3 <- lm(pid1 ~ daughter1 + relig, data = GSS)
summary(pm3)

# 4. yes, we are using the same data 
# because we omitted NA from  relig and partyid

# 5. yes, the daughter1true coeff went down from 0.29970 to 0.28523 when relig was added
# 
anova(pm1, pm3)

levels(GSS$relig)

cat_vs_none <- c(1, 0, 0, 0)
