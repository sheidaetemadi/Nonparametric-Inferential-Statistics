##location of a single population
micrometerdata <- c(1.72,2.5,2.16,2.13,2.06,2.24,2.31,2.03,1.09,1.40,2.57,2.64,1.26,2.05,2.19,2.13,1.27,1.51,2.41,1.95)
summary(micrometerdata)
binom.test(20,20,alternative = "two.sided")

##two populations,independent random samples
current <- c(190,212,211,211,190,213,211,212,164,209)
thinner <- c(216,217,216,167,219,216,179,153,215,217)
wilcox.test(current,thinner, alternative = "two.sided",paired = FALSE,conf.int = FALSE, conf.level = 0.9)

##two populations, matched pairs
human <- c(185.4,248.5,174.4,184.9,240,253.8,238.8,263.5)
robot <- c(180.4,146.3,185.5,216.4,269.3,249.6,282,315.9)
wilcox.test(human,robot,alternative = "two.sided",paired = TRUE,conf.int = FALSE,conf.level = 0.95,correct = FALSE)

##three populations,completely randomized design
plan1 <- c(102,120,105,104,126,108,117,104,125,106,121,106)
plan2 <- c(117,115,103,119,105,114,105,106,119,104,109,107)
plan3 <- c(129,103,125,128,111,107,105,125,132,124,136,120)
kruskal.test(list(plan1,plan2,plan3))

##Four populations, Randomized block design
stressrate <- matrix(c(5,3,4,6,1,9,2,10,15,12,8,13,7,11,14,6,5,3,7,1,9,4,13,15,8,2,10,10,12,14,4,7,2,8,1,10,5,12,15,6,3,13,9,14,11,7,5,3,8,4,12,6,14,15,2,1,10,13,11,9),nrow = 15,byrow = FALSE,dimnames = list(1:15,c("non-supervisors","level1","level2","level3")))
friedman.test(stressrate)

##Spearman's Rho and Kendall's Tau tests of association
library(pspearman)
annealingtime <- c(10,20,45,90,120)
potentialmv <- c(-408,-400,-392,-379,-385)
spearman.test(annealingtime,potentialmv,alternative = "two.sided",approximation = "exact")

library(Kendall)
annealingtime <- c(10,20,45,90,120)
potentialmv <- c(-408,-400,-392,-379,-385)
Kendall(annealingtime,potentialmv)

##Kendall's coefficient of concordance
library(irr)
stressratenew <- matrix(c(5,3,4,6,1,9,2,10,15,12,8,13,7,11,14,6,5,3,7,1,9,4,13,15,8,2,10,10,12,14,4,7,2,8,1,10,5,12,15,6,3,13,9,14,11,7,5,3,8,4,12,6,14,15,2,1,10,13,11,9),nrow = 15)
kendall(stressratenew,correct = FALSE)

##Partial Correlation
library(ppcor)
pcordata <- data.frame(CA=c(33.3,26.5,31.2,33.1,30.2,31.1,31.6,29.3,29.7),NT=c(11,42,29,27,10,12,15,22,10),DA=c(16,13.7,14.1,14.3,15.9,16.5,15.3,14.9,17.1))
pcor(pcordata, method = "kendall")
