# Simulate data and estimate BLP model
# using the R package BLPestimatoR
#

library(BLPestimatoR)

K<-2 #number of random coefficients
Xlin_example <-  c("price", "x1", "x2", "x3", "x4", "x5")
Xexo_example <- c("x1", "x2", "x3", "x4", "x5")
Xrandom_example <- paste0("x",1:K)
instruments_example <- paste0("iv",1:10)

data <- get.BLP.dataset(
  nmkt = 25,
  nbrn = 20,
  Xlin = Xlin_example,
  Xexo = Xexo_example,
  Xrandom = Xrandom_example,
  instruments = instruments_example,
  true.parameters = list(
    Xlin.true.except.price = rep(0.2,5),
    Xlin.true.price = -0.2,
    Xrandom.true = rep(2,K),
    instrument.effects = rep(2,10),
    instrument.Xexo.effects = rep(1,5)
  ),
  price.endogeneity = list(
    mean.xi = -2,
    mean.eita = 0,
    cov = cbind( c(1,0.7), c(0.7,1))
  ),
  printlevel = 0, seed = 234234
)


BLP_est<- estimateBLP(
  Xlin = Xlin_example,
  Xrandom = Xrandom_example,
  Xexo =  Xexo_example,
  instruments = instruments_example,
  shares = "shares",
  cdid = "cdid",
  productData = data,
  starting.guesses.theta2 = rep(1,K),
  solver.control = list(maxeval = 5000),
  solver.method = "BFGS_matlab",
  starting.guesses.delta =  rep(1, length(data$cdid)),
  blp.control = list(inner.tol = 1e-6, inner.maxit = 5000),
  integration.control= list(method="MLHS",amountNodes= 100,seed= 3   ),
  postEstimation.control= list(standardError = "robust",extremumCheck = TRUE,elasticities = "price"),
  printLevel = 2
)

# Show results
summary(BLP_est)


######################################################
# Use car data for Germany
######################################################

library(BLPestimatoR)
setwd("D:/lehre/empIOUlm")
dat = readRDS("car_with_iv.Rds")

# Price variable
dat$p = dat$princ

# Market identifier
dat$cdid = paste0(dat$ye,"_",dat$ma)


attr = c("imported","li","hp","le","sp")
# Additional attributes that I don't use as instruments
#extra.attr = c("cla","brand")
extra.attr = NULL
#extra.attr = "cla"


Xlin = c("p",attr, extra.attr)
Xrandom = c("p", attr)
Xexo = setdiff(Xlin,"p")
instruments = c(
  paste0("group.sum.", attr),
  paste0("firm.sum.", attr),
  paste0("other.sum.", attr)
)

vars = unique(c(Xlin, Xrandom, Xexo, instruments))

missing = vars[! vars %in% colnames(dat)]
missing

BLP_est<- estimateBLP(
  Xlin = Xlin,
  Xrandom = Xrandom,
  Xexo =  Xexo,
  instruments = instruments,
  shares = "s",
  cdid = "cdid",
  prodid = "type",
  productData = dat,
  starting.guesses.theta2 = rep(0.01,length(Xrandom)),
  solver.control = list(maxeval = 5000),
  solver.method = "BFGS_matlab",
  starting.guesses.delta =  rep(1, length(dat$cdid)),
  blp.control = list(inner.tol = 1e-6, inner.maxit = 5000),
  integration.control= list(method="MLHS",amountNodes= 100,seed= 3   ),
  postEstimation.control= list(
    standardError = "robust",
    extremumCheck = TRUE,
    elasticities = "p"
  ),
  printLevel = 2
)

summary(BLP_est)

e = elasticitiesBLP(BLP_est,"p","90_Germany")
rownames(e)

elasticitiesBLP(BLP_est,"p","90_Germany",c("volkswagen polo", "volkswagen golf","volkswagen passat","BMW5"))
names(BLP_est$elasticities$p)

cars = c("polo","astra","golf","306", "passat","avensis","C","5","A8")
elasticitiesBLP(BLP_est,"p","90_Germany",cars)

