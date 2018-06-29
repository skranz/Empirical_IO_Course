library(sktools)
library(restorepoint)
library(AER)
library(dplyr)
library(ggplot2)

#### Exercise 3) 

#b) 
setwd("Path")
broiler <- read.csv("broiler.csv")

summary(broiler)

#c)

ols.easy <- lm(log(Q) ~ log(PCHICK), dat=broiler)
#note that this is identical to "with(broiler, ols.easy <- lm(log(Q) ~ log(PCHICK)))"
summary(ols.easy)
head(broiler)

#d)

#PCOR
cor(broiler$PCOR,broiler$PCHICK)

#PF
cor(broiler$PF,broiler$PCHICK)

#Meatex
cor(broiler$MEATEX,broiler$PCHICK)
summary(ivreg(log(Q) ~log(PCHICK)|log(MEATEX)+log(PCOR),dat=broiler), diagnostics=TRUE)
summary(ivreg(log(Q) ~log(PCHICK)|log(MEATEX)+log(PF),dat=broiler), diagnostics=TRUE)
summary(ivreg(log(Q) ~log(PCHICK)|log(PCOR)+log(PF),dat=broiler), diagnostics=TRUE)

#QPRODA
cor(broiler$QPRODA,broiler$PCHICK)
summary(ivreg(log(Q) ~log(PCHICK)|log(PCOR)+log(QPRODA),dat=broiler), diagnostics=TRUE)
summary(ivreg(log(Q) ~log(PCHICK)|log(MEATEX)+log(QPRODA),dat=broiler), diagnostics=TRUE)

#e)
library(AER)
iv <- ivreg(log(Q)~log(CPI) + log(PBEEF) + log(PCHICK) + log(POP) + log(Y)|. - log(PCHICK) + log(MEATEX) + log(PCOR),dat=broiler)
summary(iv, diagnostics=TRUE)

#f)
library(texreg)
iv <- ivreg(log(Q)~log(CPI) + log(PBEEF) + log(PCHICK) + log(POP) + log(Y)|. - log(PCHICK) + log(MEATEX) + log(PCOR),dat=broiler)
ols.easy <- lm(log(Q) ~ log(PCHICK), dat=broiler)
iv.alt1 <- ivreg(log(Q)~log(CPI) + log(PBEEF) + log(PCHICK) + log(POP) + log(Y) + YEAR|. - log(PCHICK) + log(MEATEX) + log(PCOR),dat=broiler)
summary(iv.alt1, diagnostics=TRUE)
iv.alt2 <- ivreg(log(Q)~log(CPI) + log(PBEEF) + log(PCHICK) + log(POP) + YEAR|. - log(PCHICK) + log(MEATEX) + log(PCOR) + log(PF),dat=broiler)
summary(iv.alt2, diagnostics=TRUE)
screenreg(list(iv, ols.easy, iv.alt1, iv.alt2), custom.model.names=c("iv","ols.easy","iv.alt1", "iv.alt2"))

#Excursus
broiler$p.real <- broiler$PCHICK/broiler$CPI
broiler$p.beef.real <- broiler$PBEEF/broiler$CPI
broiler$p.corn.real <- broiler$PCOR/broiler$CPI
broiler$p.feed.real <- broiler$PF/broiler$CPI
broiler$y.real <- broiler$Y/broiler$CPI


iv.alt3 <- ivreg(log(Q)~p.beef.real + p.real + POP + YEAR|. - p.real + log(MEATEX) + p.corn.real + p.feed.real,dat=broiler)
summary(iv.alt3, diagnostics=TRUE)

acf(broiler$Q)
pacf(broiler$Q)

broiler$logQ <- log(broiler$Q)
broiler$logPop <- log(broiler$POP)
broiler$logMeatEx <- log(broiler$MEATEX)
broiler.diff <- broiler[-1,]-broiler[-nrow(broiler),]
iv.alt4 <- ivreg(logQ~p.beef.real  + p.real + logPop |. - p.real + logMeatEx + p.feed.real,dat=broiler.diff)
summary(iv.alt4, diagnostics=TRUE)

lm.alt <- lm(logQ~p.beef.real  + p.real + logPop,dat=broiler.diff)
summary(lm.alt)
