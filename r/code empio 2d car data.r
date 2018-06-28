######################################################
# Use car data for Germany
# and estimate demand with BLP model
#
######################################################

# Note this package uses a modified version of the BLPestimatoR package
# On Linux and Mac, you can install it via
# devtools::install_github("skranz/BLPestimatoR")
# On Windows you can download the compiled package .zip from Moodle
# and then install it locally from the ZIP file.

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


blp <- estimateBLP(
  Xlin = Xlin,
  Xrandom = Xrandom,
  Xexo =  Xexo,
  instruments = instruments,
  shares = "s",
  cdid = "cdid",
  prodid = "model",
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

summary(blp)

e = elasticitiesBLP(blp,"p","99_Germany")
rownames(e)

elasticitiesBLP(blp,"p","99_Germany",c("polo", "golf","passat","5"))

cars = c("polo","astra","golf","306", "passat","avensis","C","5","A8")
elasticitiesBLP(blp,"p","99_Germany",cars)

