library(sktools)
library(dplyr)
library(data.table)

#library(devtools)
#install_github(repo="dplyrExtras", username="skranz")
library(dplyrExtras)

# Remove all objects from workspace
rm(list=ls())

setwd("D:/lehre/empIOUlm")
source("sim_logit_prod.r")

# Ex 8 d) ####



##############################################
# i) Simulate a data set
##############################################

# Generate some random products
K = 2
mod = sim.logit.prod(
  T=100,J=200,K=K,
  beta0=1,beta=c(2,4),alpha=3,
  gamma0=0.8,gamma = rep(0.8,K),
  sd.xi.jt  = 0.5,
  sd.xi.j  = 0.1,
  sd.xi.t  = 0.1,
  attr.max.perc.change.across.t = 0.2
)
# Copy content of mod into global environment for easier use
copy.into.env(source=mod)


dat$p = dat$cost * runif(length(dat$cost), 1.05, 1.10)

beta1 = beta[1]; beta2 = beta[2]

dat$delta = beta0 + beta1 * dat$x1 + beta2*dat$x2 - alpha*dat$p + dat$xi
library(dplyr)
dat = mutate(dat, s.numerator = exp(delta))
dat = mutate(group_by(dat,t),  s.denominator = sum(1+exp(delta)) )

dat = ungroup(dat)
dat$s = dat$s.numerator / dat$s.denominator


# test if sum of market shares in each market is below one
# the remaining market share is the market share of the outside good
summarise(group_by(dat,t), sum(s))

M = 100

dat$q = M*dat$s


# dat is our simulated data set

# Now perform OLS estimation on dat

M.guess = 100 # guess market size
dat$s.guess = dat$q / M.guess # compute market shares based on M.guess

dat = mutate(group_by(dat,t), s0 = 1-sum(s.guess) )

# test if market shares add up to 1
summarise(group_by(dat,t), sum(s.guess)+first(s0))

# Markup prices
#dat$p = dat$cost * runif(NROW(dat),1.05,1.1)

# Check whether p and xi are indeed uncorrelated
cor.test(dat$p,dat$xi)

dat$y = log(dat$s.guess) - log(dat$s0)

# Perform estimation
summary(lm(y~x1+x2+p, data=dat))




# dat has J*T rows (Number of markets * number of products)

##############################################
# ii) Estimate parameters of utility function
##############################################

# For estimatition
M.guess = 1000
# Berechne Marktanteile gegeben der angenommenen Marktgr??e
dat$s = dat$q / M.guess

# Berechne entsprechenden Marktanteil des Outside goods
dat = mutate(group_by(dat,t), s0 = 1- sum(s))
dat

# Generate left hand side variable
dat$y = log(dat$s)- log(dat$s0)

# Conduct OLS regression
ols = lm(y~p+x1+x2,data=dat)
summary(ols)
c(alpha,beta)


# Ex 9 b) Simulating Bertrand Prices ######

bertrand.prices.logit = function(mdat,x.names = mod$attr.names, alpha=mod$alpha, beta0=mod$beta0, beta=mod$beta, mod=NULL) {
  restore.point("bertrand.prices.logit")

  mdat = as_data_frame(mdat)
  library(nleqslv)
  X = as.matrix(mdat[,x.names])
  xi = mdat$xi
  c = mdat$c

  FOC = function(p) {
    delta = -alpha*p + X %*% beta + xi
    s = exp(delta) / (1+sum(exp(delta)))
    p-c-(1/((alpha)*(1-s))) # = 0
  }

  # Guess some initial values
  s.init = 1 / (J+1)
  p.init = c + (1/((alpha)*(1-s.init)))

  # Solve the system of FOC numerically
  nleqslv(p.init,FOC)$x


  # Test if results are correct
  #p = nleqslv(p.init,FOC)$x
  #delta = -alpha*p + X %*% beta + xi
  #s = exp(delta) / (1+sum(exp(delta)))
  #round(rbind(p=p,c=c, s=as.numeric(s)),3)
  #round(FOC(p.result),8)
}

#examples.bertrand.prices.logit = function() {
K = 2
mod = sim.logit.prod(
  T=1000,J=3,K=K,
  beta0=1,beta=c(5,rep(5,K-1)),alpha=3,
  gamma0=0.8,gamma = rep(0.8,K),
  sd.xi.jt  = 5,
  sd.xi.j  = 0.1,
  sd.xi.t  = 0.1,
  attr.max.perc.change.across.t = 0.6
)
copy.into.env(mod)

# compute market prices for a single market
mdat = dat[dat$t==1,]
bertrand.prices.logit(mdat,mod=mod)



# Computes the market shares of a single market according to a simple berry logit model. The function assumes that variables alpha and beta are defined in the global environment
compute.berry.market.shares = function(mdat,
  x.names = mod$attr.names, alpha=mod$alpha, beta0=mod$beta0, beta=mod$beta, mod=NULL) {
  restore.point("compute.berry.market.shares")
  X = as.matrix(as_data_frame(mdat)[,x.names])
  delta = beta0 -alpha*mdat$p + X %*% beta + mdat$xi
  s.berry = exp(delta) / (1 + sum(exp(delta)))
  s.berry
}
# Computes the market shares of all markets according to a simple berry logit model.
compute.all.berry.market.shares = function(dat,mod=NULL,...) {
  #restore.point("compute.all.berry.market.shares")
  library(dplyrExtras)
  modify(dat,.by="t",
    s = compute.berry.market.shares(.SD,mod=mod,...),
    s0=1-sum(s)
  )
}


library(dplyrExtras)
# the modfiy function is from the package dplyrExtrasm works similar than mutate, but allows to pass the whole subgroup dataframe, as argument .SD to a called function, and has an explizit by argument for grouping
dat = modify(dat, .by="t", p=bertrand.prices.logit(.SD,mod=mod))
dat = compute.all.berry.market.shares(dat,mod=mod)

# check correlations for IV
cor(dat$p,dat$xi)
cor(dat$p,dat$x1)

# Generate instruments proposed by Berry (1994)
dat = mutate(group_by(dat,t),
    sum.x1.other=sum(x1)-x1,
    sum.x2.other=sum(x2)-x2
)

cor(dat$p,dat$sum.x1.other)

cor.test(dat$sum.x1.other, dat$xi)

# Generate left hand side variable
dat$y = log(dat$s)- log(dat$s0)

# True coefficients
c(beta0,beta,-alpha)
# OLS
ols = lm(y~p+x1+x2,data=dat)
summary(ols)

# IV etsimation
library(AER)
iv = ivreg(y~p+x1+x2|x1+x2+sum.x1.other+sum.x2.other,data=dat)
summary(iv, diagnostics=TRUE)

alpha.hat = -coef(iv)[2]

# Ex. 10 a) Estimate cost functions ####

# Generate a variable for 1 / (1-s.j)
dat$inv_1_s = 1 / (1-dat$s)

gamma
ols.cost = lm(p~inv_1_s+x1+x2,data=dat)
summary(ols.cost)

iv.cost = ivreg(p~inv_1_s+x1+x2|x1+x2+sum.x1.other+sum.x2.other,data=dat)
summary(iv.cost)


