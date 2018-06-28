library(sktools)
library(dplyrExtras) # install from git ub

#library(devtools);
#install_github(repo="dplyrExtras", username="skranz")

# a)

setwd("D:/lehre/empIOUlm")
library(foreign)
cars = read.dta("cars.dta")

library(dplyr)
d = filter(cars, ma=="Germany")
d = mutate(d, ma = as.character(ma), loc=as.character(loc))

unique(d$cla)
d = mutate(d, imported = (ma != loc))

dg = group_by(d, ye, cla, imported)
ds = summarise(dg, Q=sum(qu))



library(ggplot2)
qplot(data=ds, x = ye, y = Q, color=imported, facets = ~cla, geom="line", size=I(1.5))

getwd()


# b) Compute market shares
dtemp = summarise(group_by(d, ye), Q=sum(qu))
M = 10e6
d = mutate(group_by(d,ye),
       Q.ye = sum(qu),
       s0  = (M-Q.ye)/M,
       s   = qu / M
    )
d = mutate(group_by(d,ye,cla),
       Q.ye.cla = sum(qu),
       s.in.g = qu / Q.ye.cla
    )
d = mutate(group_by(d,ye,frm),
       s.f =  sum(qu) / M
    )
d = mutate(group_by(d,ye,frm, cla),
       s.f.in.g =  sum(qu) / Q.ye.cla
    )
d1 = arrange(d,ye, frm, model)



# c) Estimate OLS model ######
dat = d
dat$y = log(dat$s)-log(dat$s0)

# Generate the formula as a text string
# The relevant product attributes that I consider and use for instruments
attr = c("imported","li","hp","le","sp")

# Additional attributes that I don't use as instruments
extra.attr = "cla"
#extra.attr = "cla+brand+as.factor(ye)"
extra.attr = "cla+brand"

form.ols = paste("y~princ+log(s.in.g)+",paste(attr,collapse="+"),"+",extra.attr)
form.ols

ols = lm(form.ols,data=dat)
summary(ols)

# Generate instruments: sum up attributes of different products

library(dplyr)
library(dplyrExtras)

# same firm & same group
code = subst.expr("firm.group.sum.ATTR=sum(ATTR)-ATTR",subst=list(ATTR=attr))
dat = s_mutate(group_by(dat,ye,frm,cla),code)

# same firm & other group
code = subst.expr("firm.sum.ATTR=sum(ATTR)-firm.group.sum.ATTR",subst=list(ATTR=attr))
dat = s_mutate(group_by(dat,ye,frm),code)

# other firms & same group
code = subst.expr("group.sum.ATTR=sum(ATTR)-firm.group.sum.ATTR",subst=list(ATTR=attr))
dat = s_mutate(group_by(dat,ye,cla),code)

# other firms & other groups
code = subst.expr("other.sum.ATTR=sum(ATTR)-firm.sum.ATTR-group.sum.ATTR",subst=list(ATTR=attr))
dat = s_mutate(group_by(dat,ye),code)

# Collect names of instruments
inst = paste(paste(attr,collapse="+"),
             paste("firm.sum.",attr,collapse="+",sep=""),
             paste("firm.group.sum.",attr,collapse="+",sep=""),
             paste("other.sum.",attr,collapse="+",sep=""),
             paste("group.sum.",attr,collapse="+",sep=""),
             sep="+")
inst


# Run IV regression
library(AER)
form.iv = paste(form.ols,"|",inst, "+",extra.attr)
iv = ivreg(form.iv,data=dat)
summary(iv, diagnostics = TRUE)
summary(ols)

# d) Compute cross price effects for a selection of cars #####

# Compute price derivatives of market shares for nested logit model
# j and k can be vectors of different product indices: the function then returns a corresponding vector of elasticities for each row of j and k
nested.logit.price.deriv = function(j,k,data,alpha,sigma,group.var="cla") {

  restore.point("nested.logit.price.deriv")

  data = as.data.frame(data)
  # Shortcuts for market shares
  s.j = data[j,"s"]
  s.k.g = data[k,"s.in.g"]
  s.k = data[k,"s"]

  # Same product
  own.deriv = -alpha*(1/(1-sigma))*s.j*(1-sigma*s.k.g-(1-sigma)*s.k)
  # Same group different product
  group.deriv = alpha*(1/(1-sigma))*s.j*(sigma**s.k.g+(1-sigma)*s.k)
  # Different product
  other.deriv = alpha*s.j*s.k

  # Assign the appropriate derivatives
  deriv = other.deriv
  # same product
  rows = j == k
  deriv[rows] = own.deriv[rows]
  # same group, other product
  rows = (j != k) & data[j,group.var] == data[k,group.var]
  deriv[rows] = group.deriv[rows]

  deriv
}

# Compute price elasticities
nested.logit.price.elast = function(j,k,data,alpha,sigma, group.var="cla",price.var = "princ") {

  restore.point("nested.logit.price.elast")
   # Compute derivatives
  deriv = nested.logit.price.deriv(j,k,data,alpha,sigma,group.var)
  round(deriv,4)
  # elast = ds/dp * (p / s)
  elast = deriv * (data[k,price.var] / data[j,"s"])
  return(elast)
}



# Restrict attention to some year
year = 99
ydat = dat[dat$ye==year,]

# Specify some car models and store the data in cdat
mod = c("polo","astra","golf","306", "passat","avensis","C","5","A8")
ncar =length(mod)
cdat = filter(ydat, model %in% mod)

rows = match(mod,ydat$model)
cdat = ydat[rows,]
cdat

cdat = as.data.frame(cdat)

# Choose a regression (ols or iv) and retrieve estimates for alpha and sigma
reg = iv
alpha = -coef(reg)["princ"]
sigma = coef(reg)["log(s.in.g)"]

j = 1
k = 2
nested.logit.price.deriv(j,k,data=dat,alpha,sigma,group.var="cla")
nested.logit.price.elast(j,k,data=dat,alpha,sigma,group.var="cla")

# Compute matrix of cross price derivatives
deriv.mat = outer(1:ncar,1:ncar,nested.logit.price.deriv,
                  alpha=alpha,sigma=sigma,dat=cdat)
colnames(deriv.mat) = paste(cdat[,"model"])
round(deriv.mat,2)

# Compute matrix of elasticities conveniently with the function outer
elast.mat = outer(1:ncar,1:ncar,nested.logit.price.elast,
                  alpha=alpha,sigma=sigma,dat=cdat)
colnames(elast.mat) = rownames(elast.mat) = paste(cdat[,"model"])

round(elast.mat,2)
# Cross price derivatives are mainly driven by the exogenous classification
# into groups, the differences within group and outside of groups seem to be too strong


# e) Compare estimates for alpha for different assumptions on market size ####

# I will change manually with rel.M

estimate.alpha = function(rel.M=0.25) {
  restore.point("estimate.alpha")

  # Potential market size is a fraction rel.M of the total population
  dat$M = dat$pop * rel.M

  # compute market shares of each car and of outside product in each year
  dat = mutate(group_by(dat,ye),s = qu/M, s0 =(M-Q.ye)/M)
  dat$y = log(dat$s)-log(dat$s0)

  #print(rbind(range(dat$s0),range(dat$s),range(dat$y)))
  iv = ivreg(form.iv,data=dat)
  alpha = - as.numeric(coef(iv)["princ"])
  sigma = coef(iv)["log(s.in.g)"]
  alpha
}
coef(iv)
estimate.alpha(0.10)
estimate.alpha(0.25)
estimate.alpha(1)

# Estimate alpha immediately for different assumptions on market size
rel.M.vec = c(0.1,0.25,0.5,0.75,1,2)
alpha.vec = sapply(rel.M.vec,estimate.alpha)

plot(rel.M.vec, alpha.vec,type="o",pch=19,col="blue", main="alpha vs assumed market size",ylim=c(0,max(alpha.vec)),xlim=c(0,max(rel.M.vec)),ylab="alpha",xlab="M / Pop")
abline(h=min(alpha.vec),col="grey",lty=2)

# f) Estimate costs #####

dat$frm = as.character(dat$frm)


# Compute market shares of firms: total and within group
dat = mutate(group_by(dat,ye,frm), s.f=sum(s))
dat = mutate(group_by(dat,ye,frm,cla), s.f.in.g=sum(s.in.g))

alpha = - coef(iv)["princ"]
sigma = coef(iv)["log(s.in.g)"]
alpha

dat$v.hat = 1 / ( 1- sigma*dat$s.f.in.g - (1-sigma)* dat$s.f)
form.cost = paste("princ ~  v.hat +", paste(attr,collapse="+"))
form.cost # Not a very good specification of a cost function....

form.iv.cost = paste(form.cost,"|",inst)
iv.cost = ivreg(form.iv.cost,data=dat)
summary(iv.cost)  # Quite insensible results...

v.coef   =  coef(iv.cost)[2]
# Ideally these two values should be as equal as possible
c(v.coef,(1-sigma) / alpha)

dat$markup = v.coef * dat$v.hat
dat$cost = dat$princ - dat$markup
plot(dat$princ,dat$cost)
abline(a=0,b=1,col="red")


# Different approach, estimate p - ((1-sigma) / alpha)*v = X*gamma + omega
#dat$markup = ((1-sigma) / alpha)*dat$v.hat
#dat$cost = dat$princ - dat$markup
#form.cost = paste("cost ~ ", paste(attr,collapse="+"))
#form.iv.cost = paste(form.cost,"|",inst)
#iv.cost = ivreg(form.iv.cost,data=dat)
#summary(iv.cost)  # Quite insensible results...
#v.coef = ((1-sigma) / alpha)

# g) Simulate prices under Bertrand competition ####


# Simulate bertrand prices
bertrand.prices.nested.logit = function(mdat,X=NULL,alpha,beta,sigma, v.coef = (1-sigma) / alpha, p.init=NULL,x.names=NULL) {
  restore.point("bertrand.prices.nested.logit")
  library(nleqslv)

  if (is.null(X)) {
    X = as.matrix(mdat[,x.names])
  }

  xi = mdat$xi
  c = mdat$cost

  FOC = function(p) {
    restore.point("FOC")
    message(".",appendLF=runif(1)<0.03)
    sdat = compute.market.shares.nested.logit(
      mdat=mdat,p=p,xi=xi,X=X,alpha=alpha,beta=beta,sigma=sigma)
    v = 1 / ( 1- sigma*sdat$s.f.in.g - (1-sigma)* sdat$s.f)


    #p-c-((1-sigma)/alpha)*v # = 0
    # v.coef can be (1-sigma)/alpha as computed by demand estimation or by the cost estimation. Ideally, we should have used a system estimator with the restriction that both are equal. Since no such stable estimation routine exists for R or Stata, our simulation is a bit dirty by choosing one of the two values.
    p-c-v.coef*v # = 0

  }
  if (is.null(p.init)) {
    # Guess some initial values, using the normal berry logit formula
    s.init = 1 / (NROW(mdat)+1)
    p.init = c + (1/((alpha)*(1-s.init)))
  }
  # Solve the system of FOC numerically
  ret = nleqslv(p.init,FOC,control=list(trace=1,ftol=1e-10,xtol=1e-10))
  message(paste("\n",ret$message,sep=""))
  ret$x
}


# Computes market shares for a single market according to the nested logit model, also computes firm wide shares
compute.market.shares.nested.logit = function(mdat,beta,alpha,sigma,x.names=NULL,X=NULL,p=mdat$p,xi=mdat$xi,group.col="cla", firm.col = "frm") {
  restore.point("compute.market.shares.nested.logit")
  if (is.null(X)) {
    X = as.matrix(mdat[,x.names])
  }

  mdat[order(mdat$s),c("model", "s")]

  mdat$delta = as.numeric(-alpha*p + X %*% beta + xi)
  mdat$exp.delta = exp(mdat$delta/(1-sigma))

  D.g = summarize(s_group_by(mdat,group.col), D.g=sum(exp.delta))$D.g

  mdat = mutate(s_group_by(mdat,group.col),D.g=sum(exp.delta))

  # Market share of each group g
  mdat$s.g = (mdat$D.g^(1-sigma)) / (1+sum(D.g^(1-sigma)))
  # Within group market share
  mdat$s.in.g = mdat$exp.delta / mdat$D.g
  # Market share is the product of both
  mdat$s = mdat$s.g * mdat$s.in.g

  mdat[order(mdat$s),c("model","D.g", "s","s.g","s.in.g")]

  if (is.null(firm.col)) {
    return(mdat)
  }

  #sdat[,firm.col] = mdat[,firm.col]
  # Compute market shares of firms: total and within group

  mdat = mutate(s_group_by(mdat,firm.col), s.f=sum(s))
  mdat = mutate(s_group_by(mdat,c(firm.col,group.col)), s.f.in.g=sum(s.in.g))


  return(ungroup(mdat))
}


# Repeat estimations
#ols = lm(form,data=dat)
#summary(ols)

iv = ivreg(form.iv,data=dat)
summary(iv)

# Select one estimation
reg = iv

# Remove NA rows
edat = dat[-as.numeric(reg$na.action),]
c(NROW(dat),NROW(edat),NROW(resid(reg)))

# Speciy xi as residuals from estimation
edat$xi = resid(reg)

# Give names to price and "observed" market shares
edat$p = edat$princ
edat$p.act = edat$p
edat$s.act = edat$s
edat$id.num = 1:NROW(edat) # used for later sorting

# Select data for a single year
mdat = edat[edat$ye==99,,drop=FALSE]

# Generate X matrix used in the regression
form = as.formula(form.ols)
X = model.matrix(form,mdat)[,-(2:3)]
# Get coefficients from estimation
alpha = - coef(reg)[2]
sigma = coef(reg)["log(s.in.g)"]
beta = coef(reg)[colnames(X)]
beta

# Compute market shares using the formulas for the nested logit model
mdat = compute.market.shares.nested.logit(mdat,X=X,beta=beta,alpha=alpha,sigma=sigma,x.names=attr,group.col="cla", firm.col = "frm",p = mdat$p)

# Compare computed market shares with the actual market shares
# They should be equal...
plot(mdat$s.act,mdat$s, main="Actual vs Nested Logit Market Shares")
abline(a=0,b=1,col="red")

# Compute equilibrium prices according to Bertrand competition
mdat$p.sim = bertrand.prices.nested.logit(mdat=mdat,X=X,alpha=alpha,beta=beta,sigma=sigma, v.coef = v.coef, p.init=mdat$p)


# Given that we computed the cost and markup under the assumption that firms compete ? la Bertrand, the actual prices should then be equal to the equilibrium prices

plot(mdat$p,mdat$p.sim, main="Actual vs Simulated Prices")
abline(a=0,b=1,col="red")

mdat$markup.sim = mdat$p.sim - mdat$cost


# h) Simulate effects of merger ####

# Premerger data
predat = mdat

predat = compute.market.shares.nested.logit(predat,beta=beta,alpha=alpha,sigma=sigma,X=X,group.col="cla", firm.col = "frm",p = predat$p.sim)

plot(predat$s.act,predat$s, main="Actual vs Nested Logit Market Shares")
abline(a=0,b=1,col="red")

# Generate Postmerger data
postdat = predat
unique(postdat$frm)
unique(postdat$brand)

# Assume VW buys Opel from General Motors
merged.firm = "VW"
postdat = predat
postdat$frm[postdat$brand=="opel"] = merged.firm


# Simulate post-merger prices and market shares
postdat$p.sim = bertrand.prices.nested.logit(mdat=postdat,X=X, alpha=alpha, beta=beta, sigma=sigma, p.init=predat$p.sim, v.coef = v.coef)

postdat = compute.market.shares.nested.logit(postdat,X=X,beta=beta,alpha=alpha,sigma=sigma,group.col="cla", firm.col = "frm", p = postdat$p.sim)

# Check if predat and postdat do have the same order. This should hold true if you use the new version of qick.by, which by default maintains the row order
all(predat$id.num == postdat$id.num)



plot(predat$s,postdat$s, main="Pre- vs Postmerger Market Shares")
abline(a=0,b=1,col="red")

plot(predat$p.sim,postdat$p.sim, main="Pre- vs Postmerger Prices")
abline(a=0,b=1,col="red")


# Compute some aggregate statistics and store them in a new data.frame medat
medat = postdat
medat$has.merged = (postdat$frm == merged.firm)
medat$p.pre = predat$p.sim
medat$p.post = postdat$p.sim
medat$p.rel.change = (postdat$p.sim - predat$p.sim) / predat$p.sim
medat$firm.pre = predat$frm
medat$s.pre = predat$s
medat$s.post = postdat$s
medat$s.f.pre = predat$s.f
medat$s.f.post = postdat$s.f
medat$s.f.in.g.pre = predat$s.f.in.g
medat$s.f.in.g.post = postdat$s.f.in.g

medat$s.abs.change = postdat$s-predat$s
medat$s.rel.change = (postdat$s-predat$s) / predat$s

medat$profit.pre = predat$s*predat$M*(predat$p.sim-predat$cost)
medat$profit.post = postdat$s*postdat$M*(postdat$p.sim-postdat$cost)
medat$profit.rel.change = (medat$profit.post - medat$profit.pre) / medat$profit.pre

# Compute consumer surplus using the formula in Ivaldi & Verbooven
D.g.pre = summarize(group_by(predat,cla),D.g=first(D.g))$D.g
CS.pre = (1/alpha)*log(1+sum(D.g.pre^(1-sigma)))
D.g.post = summarize(group_by(postdat,cla),D.g=first(D.g))$D.g
CS.post = (1/alpha)*log(1+sum(D.g.post^(1-sigma)))

# Relative change in consumer surplus
CS.change = as.numeric((CS.post - CS.pre) / CS.pre)
CS.change * 100

# Analysie price changes with ggplot
library(ggplot2)
qplot(x=p.pre,y=p.rel.change, data=medat, col=has.merged)

# To analyse the merger effects in an interactive graph we can use the package googleVis

# Convenience interface to gvisMotionChart that allows to set default columns
myMotionChart = function(df,idvar=colnames(df)[1],timevar=colnames(df)[2],xvar=colnames(df)[3],yvar=colnames(df)[4], colorvar=colnames(df)[5], sizevar = colnames(df)[6],...) {
  restore.point("myMotionChart")
  library(googleVis)

  # Generate a constant variable as column for time if not provided
  # Unfortunately the motion plot still shows 1900...
  if (is.null(timevar)) {
    .TIME.VAR = rep(0,NROW(df))
    df = cbind(df,.TIME.VAR)
    timevar=".TIME.VAR"
  }

  # Transform booleans into 0 and 1 since otherwise an error will be thrown
  for (i in  1:NCOL(df)) {
    if (is.logical(df [,i])[1])
      df[,i] = df[,i]*1
  }

  # Rearrange columns in order to have the desired default values for
  # xvar, yvar, colorvar and sizevar
  firstcols = c(idvar,timevar,xvar,yvar,colorvar,sizevar)
  colorder = c(firstcols, setdiff(colnames(df),firstcols))
  df = df[,colorder]

  gvisMotionChart(df,idvar=idvar,timevar=timevar,...)
}

medat$time = 0
medat$brand_model  = paste(medat$brand, medat$model,sep=" ")
gplot <- myMotionChart(medat,idvar="brand_model",timevar=NULL,xvar="p.pre",yvar="p.rel.change",colorvar="firm.pre",sizevar="s.pre",chartid="MergerSimulationNestedLogit")
plot(gplot)













