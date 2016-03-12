install.packages("clinfun"); 
install.packages("ggplot2"); 
install.packages ("pastecs"); 
install.packages("pgirmess");

library(clinfun); 
library(ggplot2); 
library(pastecs); 
library(pgirmess);

getwd()

# Wilcoxon Rank-Sum Test (aka Mann-Whitney Test)
# Use when: 
# Non-Parametric Data
# Two Different Participants in Two Conditions

drug <- read.delim("Drug.dat")
names(drug)

drug.ranked <- drug[with(drug, order(wedsBDI)), c(1, 3)]
drug.ranked

drug.ranked$potRank <- c(1:length(drug.ranked$wedsBDI))
drug.ranked$actRank <- as.double(drug.ranked$potRank)

drug.duplicates <- drug.ranked[duplicated(drug.ranked$wedsBDI) | duplicated(drug.ranked$wedsBDI, fromLast = TRUE),]
drug.dup.ranks <- with(drug.duplicates, aggregate(actRank ~ drug + wedsBDI, FUN = mean))

install.packages("data.table")
require(data.table)

drug.ranked <- setDT(drug.ranked)
drug.dup.ranks <- setDT(drug.dup.ranks)

setkey(drug.ranked, drug, wedsBDI)
setkey(drug.dup.ranks, drug, wedsBDI)
drug.ranked[drug.dup.ranks, actRank := i.actRank]
drug.ranked

wed.ranks <- with(drug.ranked, aggregate(actRank ~ drug, FUN = sum))
wed.ranks

wed.ranks$W <- wed.ranks$actRank - 55
wed.ranks

# drug.ranked <- drug[with(drug, order(wedsBDI)), c(1, 3)]
drug.sun <- drug[with(drug, order(sundayBDI)), c(1, 2)]
drug.sun

drug.sun$potRank <- c(1:length(drug.sun$sundayBDI))
drug.sun$actRank <- as.double(drug.sun$potRank)
drug.sun

drug.sun.dup <- drug.sun[duplicated(drug.sun$sundayBDI) | duplicated(drug.sun$sundayBDI, fromLast = TRUE),]
drug.sun.dup.ranks <- with(drug.sun.dup, aggregate(actRank ~ sundayBDI, FUN = mean))

drug.sun <- setDT(drug.sun)
drug.sun.dup.ranks <- setDT(drug.sun.dup.ranks)
setkey(drug.sun, sundayBDI)
setkey(drug.sun.dup.ranks, sundayBDI)
drug.sun[drug.sun.dup.ranks, actRank := as.double(i.actRank)]

is.data.table(drug.sun)
is.data.table(drug.sun.dup.ranks)

sun.ranks <- with(drug.sun, aggregate(actRank ~ drug, FUN = sum))
sun.ranks

sun.ranks$W <- sun.ranks$actRank - 55
sun.ranks
