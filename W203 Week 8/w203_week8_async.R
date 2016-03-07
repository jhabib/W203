library(car)

getwd()
setwd("w203 week 8")

load("Countries2.Rdata")
summary(Countries)


Requests <- read.csv("Removal_Requests.csv")
head(Requests)

# View(Requests)

Requests$total.takedowns <- Requests$Court.Orders + Requests$Executive..Police..etc.

R2 <- aggregate(Requests[, c("Court.Orders", "Executive..Police..etc.", 
                             "total.takedowns")], list(Country = Requests$Country), sum)

head(R2)

Countries <- merge(Countries, R2, by = "Country", all = TRUE)

head(Countries)

scatterplot(Countries$cpi, Countries$total.takedowns)

cor.test(Countries$cpi, Countries$total.takedowns)

cor(Countries[, c("gdp", "cpi", "total.takedowns")], 
    use = "pairwise.complete.obs") ** 2

table(Countries$region, Countries$high_cpi)

cs <- chisq.test(Countries$region, Countries$high_cpi)

cs

cs$stdres

cs$expected

fisher.test(Countries$region, Countries$high_cpi)

cramers_v <- function(cs) { 
  cv <- sqrt(cs$statistic / (sum(cs$observed) * (min(dim(cs$observed))-1)))
  
  print.noquote("Cramer's V:")
  return(as.numeric(cv))
}

cramers_v(cs)

Corrupt_Source <- aggregate(Countries[, c("Court.Orders", 
                                          "Executive..Police..etc.")], 
                            list(high_cpi = Countries$high_cpi), 
                            sum, na.rm = TRUE)

Corrupt_Source

rownames(Corrupt_Source) <- Corrupt_Source$high_cpi

Corrupt_Source[, c(-1)]

cs <- chisq.test(Corrupt_Source[, c(-1)])
cs

cs$stdres

cs$expected

corrupt_odds <- Corrupt_Source["Corrupt", "Court.Orders"] / Corrupt_Source["Corrupt", "Executive..Police..etc."]
corrupt_odds

trustworthy_odds <- Corrupt_Source["Trustworthy", "Court.Orders"] / Corrupt_Source["Trustworthy", "Executive..Police..etc."]
trustworthy_odds

corrupt_odds / trustworthy_odds

Requests2 <- merge(Countries[, c("Country", "high_cpi")], Requests, by = "Country")
head(Requests2)

Requests2 <- Requests2[!is.na(Requests2$high_cpi),]
head(Requests2)

Corrupt_Product <- Requests2[, c("Product", "high_cpi")]
head(Corrupt_Product)

Corrupt_Product$Corrupt <- ifelse(Requests2$high_cpi == "Corrupt", 
                                  Requests2$total.takedowns, 0)
Corrupt_Product$Trustworthy <- ifelse(Requests2$high_cpi == "Trustworthy", 
                                  Requests2$total.takedowns, 0)
 

head(Corrupt_Product)

Corrupt_Product <- aggregate(Corrupt_Product[, c("Corrupt", "Trustworthy")], 
                             list(Product = Corrupt_Product$Product), sum)

Corrupt_Product

library(reshape)

Corrupt_Product <- cast(Requests2, Product ~ high_cpi, 
                        fun = sum, 
                        value = c("total.takedowns"))
Corrupt_Product

cs <- chisq.test(Corrupt_Product[, c(-1)])
cs

cs$stdres

cs$expected

fisher.test(Corrupt_Product[, c(-1)])

chisq.test(Corrupt_Product[, c(-1)], simulate.p.value = TRUE)

cramers_v(cs)
cs
