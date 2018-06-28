
#1. Run a model with installation cost and operating cost, without intercepts

library("mlogit")

data("Heating", package = "mlogit")
H <- mlogit.data(Heating, shape="wide", choice="depvar", varying=c(3:12))
#View(as.data.frame(H))
m <- mlogit(depvar~ic+oc|0, H)
summary(m)

#(a) Do the estimated coefficients have the expected signs?

#(b) Are both coefficients significantly different from zero?

#(c) How closely do the average probabilities match the shares of
#customers choosing each alternative?

# Returns predicted choice probabilities, in a matrix form

# fitted.nj = Pr(U.hat.nj >= U.hat.ni for all i != j)

P.hat = fitted(m, outcome=FALSE)
P.hat

# Compute average shares of P.hat
# For each heating system (column)
# the average over each household (rows)
share.est = colMeans(P.hat)
sum(share.est)

# Average share in actual data
share.act = table(Heating$depvar) / NROW(Heating)

share.est= share.est[names(share.act)]

rbind(share.est,share.act)

# Actual shares are not very well predicted.

#(d) The ratio of coefficients usually provides economically meaningful information. The willingness to pay ($wtp$) through higher installation cost for a one-dollar reduction in operating costs is the ratio of the operating cost coefficient to the installation cost coefficient. What is the estimated $wtp$ from this model? Is it reasonable in magnitude?

# Formula for willingness to pay:
#
# wtp = - dic / doc | V=const
#
# V = beta.ic * ic + beta.oc * oc
#
# Implicit differentiation w.r.t. doc yields
#
# 0 = beta.ic * dic/doc + beta.oc
# - dic / doc = beta.oc / beta.ic

wtp = coef(m)["oc"]/coef(m)["ic"]
wtp


#(e) We can use the estimated $wtp$ to obtain an estimate of the discount rate that is implied by the model of choice of operating system.

# Derive formula on white board

r.hat = 1 /wtp
r.hat # 136% interest rate per year!

#2. Estimate a model that imposes the constraint that $r=0.12$ (such that
#$wtp=8.33$). Test the hypothesis that $r=0.12$.

# # This restraint means mathematicyll that as
# dIC/dOC | LCC=const = - (dLCC / dOC)/(dLCC/dIC) =
#               - beta.oc / beta.ic = wtp = 1 / r     <=>
#
# beta.ic = r*beta.oc <=>
# beta.ic = 0.12 * beta.oc
#
# A simpler way to impose this constraint, it to compute a lifecycle cost
# that embodies the constraint
# lcc=ic+oc/0.12
# and estimate the model with this variable.



H$lcc=H$ic+H$oc/0.12
mlcc <- mlogit(depvar~lcc|0, H)
mlcc


summary(mlcc) # restricted model
summary(m)    # unrestricted model

# Likelihood ratio test
# Null hypothesis: the unrestricted model and the restricted model are not different

library(lmtest)
lrtest(m, mlcc)

# Test statistic manually computed (see e.g. Wikipedia entry on likelihood ratio test)
L = -2* as.numeric(mlcc$logLik)+2 * as.numeric(m$logLik)
L
# Critical value at 5% . Null hypothesis is rejected if L bigger than that crictical value
critical.L = qchisq(0.95, df = 2-1)
c(L, critical.L)

# p-value
pchisq(L, df=2-1,lower.tail=FALSE)

# 3. Add alternative-specific constants to the model. ####

# With $J$ alternatives, at most $J-1$ alternative-specific constants can be estimated. The coefficients of $J-1$ constants are interpreted as relative to alternative $J$th alternative. Normalize the constant for the alternative \va{hp} to 0.

mc <- mlogit(depvar~ic+oc, H, reflevel = 'hp')
summary(mc)


#(a) How well do the estimated probabilities match the shares of customers choosing each alternative? ####

P.hat = fitted(mc, outcome=FALSE)
share.est = colMeans(P.hat)
share.act = table(Heating$depvar) / NROW(Heating)
share.est= share.est[names(share.act)]
rbind(share.est,share.act)

#Note that they match exactly: alternative-specific constants in a logit model insure that the average probabilities equal the observed shares.


# (b) Calculate the $wtp$ and discount rate $r$ that is implied by the estimates. Are these reasonable? ####
wtp <- coef(mc)["oc"]/coef(mc)["ic"]
names(wtp) = NULL
wtp
r.hat <- 1/wtp
r.hat

#The decision-maker is willing to pay \$4.56 for a \$1 year stream of savings. This implies $r = 0.22$. The decision-maker applies a 22\% discount rate.  These results are certainly more reasonable that in the previous model. The decision-maker is still estimated to be valuing saving somewhat less than would seem rational (ie applying a higher discount rate than seems reasonable). However, we need to remember that the decision-maker here is the builder. If home buyers were perfectly informed, then the builder would adopt the buyer's discount rate. However, the builder would adopt a higher discount rate if home buyers were not perfectly informed about (or believed) the stream of saving.

# (c) This model contains constants for all alternatives \va{ec}-\va{er}-\va{gc}-\va{gr}, with the constant for alternative \va{hp} normalized to zero. Suppose you had included constants for alternatives \va{ec}-\va{er}-\va{gc}-\va{hp}, with the constant for alternative \va{gr} normalized to zero. What would be the estimated coefficient of the constant for alternative \va{gc}? Figure this out logically rather than actually estimating the model.

summary(mlogit(depvar~ic+oc, H, reflevel = 'ec'))

update(mc, reflevel="gr")


# 4. Now we try a model with sociodemographic variables entering. Enter alternative-specific income effects.  What do the estimates imply about the impact of income on the choice of central systems versus room system? Do these income terms enter significantly?
mi2 <- mlogit(depvar~oc+ic | income, H, reflevel="hp")
summary(mi2)

summary(update(mi2, reflevel="gr"))

#The model implies that as income rises, the probability of heat pump rises relative to all the others (since income in the heat pump alt is normalized to zero, and the others enter with negative signs such that they are lower than that for heat pumps. Also, as income rises, the probability of gas room drops relative to the other non-heat-pump systems (since it is most negative).

#Do these income terms enter significantly?

# Different tests for joint significance
lrtest(mc, mi2)
#waldtest(mc, mi2)
#scoretest(mc, mi2)

#No. It seems that income doesn't have a significant effect. Maybe this is because income is for the family that lives in the house, whereas the builder made decision of which system to install.


# 5. The California Energy Commission cec is considering whether to offer rebates on heat pumps. The cec wants to predict the effect of the rebates on the heating system choices of customers in California. The rebates will be set at 10\% of the installation cost.
# a) First estimate again a model with installation costs, operating costs, and alternative specific constants.
mc <- mlogit(depvar~ic+oc, H, reflevel = 'hp')
summary(mc)

# b) Using your estimated model, make a prediction of how many percent and by how many percentage points the 10% rebates on heat-pumps raise the share of houses with heat pumps.
View(H)
Hn <- H
# Reduce investmentent cost by 10%

Hn[Hn$alt == "hp", "ic"] <- 0.9 * H[Hn$alt == "hp","ic"]

# The function predict uses the model to compute choice probabilities
P.pred = predict(mc, newdata = H)

Pn.pred = predict(mc, newdata = Hn)

rbind(P.pred[1,],Pn.pred[1,])

s.pred = colMeans(P.pred)
sn.pred = colMeans(Pn.pred)

round(rbind("orgignal" = s.pred, "with subdidy"=sn.pred, change=(sn.pred-s.pred)/s.pred)*100,2)


head(P.pred)

share.pred = colMeans(P.pred)
share.act = table(Heating$depvar) / NROW(Heating)
share.pred= share.pred[names(share.act)]

rbind(share.act,
      share.pred,
      percentage.point.change=(share.pred - share.act),
      percentage.change=(share.pred / share.act)-1)*100

