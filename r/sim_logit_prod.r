BERRY.DEFAULT = list(
  T=100,
  J=2,
  K=2,
  F=NA,
  j.per.firm = 1,
  attr.names=NULL,
  attr.min=0,
  attr.max=1,
  M=1,
  scale=1,
  attr.max.perc.change.across.t=0.1,
  sd.xi.jt  = 0.5,
  sd.xi.t  = 0.5,
  sd.xi.j  = 0.5,
  sd.omega.jt  = 0.4,
  sd.omega.t  = 0.4,
  sd.omega.j  = 0.4,
  alpha  = -1,
  gamma0 = 0,
  beta0 = 0,
  beta = NULL,
  gamma = NULL,
  dat = NULL,
  est.par.true = NULL,
  round.attr = FALSE
)


# Simulates product characteristics and specifies coefficients for cost function and delta.j for a berry logit model or BLP
# Does not yet compute prices
sim.logit.prod = function(...,.args=NULL,.source.env = sys.frame(sys.parent(1))) { 
  args = c(list(...),.args)

  # Copys parameters from model into local environment
  # So we can e.g. type, alpha instead of mod$alpha 
  restore.point("make.logit.prod.and.coef", deep.copy=FALSE)
  require(sktools)
  
  # Parameters are taken in this precedence order from
  # 1. Function arguments in ... 
  # 2. The global environment
  # 3. BERRY.DEFAULT
  
  m = as.environment(BERRY.DEFAULT)
  #names = intersect(names(BERRY.DEFAULT), ls(.source.env))
  #copy.into.env(dest=m, names=names, source=.source.env)
  #browser()
  copy.into.env(source=args,dest=m)
  copy.into.env(source=m)
  #restore.point("make.logit.prod.and.coef")
  
  
  ##################################### 
  # Specify firms for each product
  ##################################### 
  
  if (is.na(F)) {
    F = ceiling(J/j.per.firm)
  }
  if (F>=J) {
    F = J
    firm=1:J  
  } else {
    # Each firm has at least one product, remaining products are
    # randomly assigned
    firm = c(1:F,sample(1:F,J-F,replace=TRUE))
  }
  
  
  ##################################### 
  # Draw attributes for one market
  ##################################### 
  
  attr.min = rep(attr.min,length.out=K)
  attr.max = rep(attr.max,length.out=K)

  attr.names = paste0("x",1:K)
  # Draw attributes for one market 
  # uniformely between attr.min and attr.max for one market
  X.one.market = sapply(1:K, function(k) runif(J,attr.min[k],attr.max[k]))
  if (round.attr)
    X.one.market = round(X.one.market)
  
  

  ###########################################
  # Generate t,j,firm and X for all markets
  ###########################################
  
  dat = data.frame(t=rep(1:T,each=J),j=rep(1:J,times=T),firm=rep(firm,times=T))
  
  
  # Generate X values for all markets
  X.all.markets = lapply(1:T,function(t) {
    change.vec = runif(J*K, 1-attr.max.perc.change.across.t,1+attr.max.perc.change.across.t)
    change.mat = matrix(change.vec,J,K)
    X.one.market * change.mat
  })
  X.all.markets = do.call(rbind,X.all.markets)
  colnames(X.all.markets) = attr.names
  
  #######################################################
  # Draw random cost and demand shocks
  #######################################################
  
  # xi combines three types of aggregate demand shock
  # product fixed shocks xi.j
  # market fixed shocks xi.t
  # shock for product / market combination xi.jt
  xi.j = rnorm(J,0,sd.xi.j)
  xi.t = rnorm(T,0,sd.xi.t)
  xi.jt = rnorm(T*J,0,sd.xi.jt)
  
  xi = xi.jt + xi.j[dat$j] + xi.t[dat$t]

  # omega combines three types of cost shocks
  # product fixed cost shocks cost.j
  # market fixed cost shocks cost.t
  # shock for product / market combination cost.jt
  omega.j = rnorm(J,0,sd.omega.j)
  omega.t = rnorm(T,0,sd.omega.t)
  omega.jt = rnorm(T*J,0,sd.omega.jt)
  
  omega = omega.jt + omega.j[dat$j] + omega.t[dat$t]
  
  cost = gamma0 + X.all.markets %*% gamma + omega

  dat = cbind(dat,X.all.markets,cost,xi,omega)  
  copy.into.env(dest=m, names=names(m))
  
  return(as.list(m))
}

examples.sim.logit.prod = function() {
  library(sktools)
  
  T=5
  J=4
  K = 2
  j.per.firm = 1

  scale  = 500  
  alpha  = 1 / scale
  
  beta = rep(1,K)
  gamma = beta * runif(NROW(beta),0.8,1)
  
  attr.max.perc.change.across.t = 0.1
  attr.min = 0
  attr.max = 100
  
  sd.xi.j = 500 /scale
  sd.xi.t = 100 /scale
  sd.xi.jt = 100 /scale
  
  sd.omega.j = 500 /scale
  sd.omega.t = 100 /scale
  sd.omega.jt = 100 /scale
  
    
  M  = 1000
  m = sim.logit.prod()
  m
}
