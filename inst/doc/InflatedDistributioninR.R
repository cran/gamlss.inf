## ----logitTFDist, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
# generate the distribution 
library(gamlss)
gen.Family("TF", type="logit")
# different mu
curve(dlogitTF(x, mu=-5, sigma=1, nu=10), 0,1, ylim=c(0,3), lwd=3, lty=2, col=2)
title("(a)")
curve(dlogitTF(x, mu=-1, sigma=1, nu=10), 0,1, add=TRUE, lwd=3, lty=3, col=3)
curve(dlogitTF(x, mu=0,  sigma=1, nu=10), 0,1, add=TRUE, lwd=3, lty=4, col=4)
curve(dlogitTF(x, mu=1,  sigma=1, nu=10), 0,1, add=TRUE, lwd=3, lty=5, col=5)
curve(dlogitTF(x, mu=5,  sigma=1, nu=10), 0,1, add=TRUE, lwd=3, lty=6, col=6)
legend("top",
       legend=c("mu = -5","mu = -1","mu = 0","mu = 1","mu = 5"),
       lty=2:6, col=2:6, cex=1, lwd=3)
# different sigma
curve(dlogitTF(x, mu=0, sigma=.5, nu=10), 0,1, ylim=c(0,3), lwd=3, lty=2, col=2)
title(("(b)"))
curve(dlogitTF(x, mu=0, sigma=1, nu=10), 0,1, add=TRUE, lwd=3,lty=3, col=3)
curve(dlogitTF(x, mu=0, sigma=2, nu=10), 0,1, add=TRUE, lwd=3,lty=4, col=4)
curve(dlogitTF(x, mu=0, sigma=5, nu=10), 0,1, add=TRUE, lwd=3,lty=5, col=5)
legend("topleft",
       legend=c("sigma = .5","sigma = 1","sigma = 2","sigma = 5"),
       lty=2:5, col=2:5, cex=1, lwd=3)
# different nu
curve(dlogitTF(x, mu=0, sigma=1, nu=1000), 0,1, ylim=c(0,3), lwd=3, lty=2, col=2)
title("(c)")
curve(dlogitTF(x, mu=0, sigma=1, nu=10), 0,1, add=TRUE, lwd=3, lty=3, col=3)
curve(dlogitTF(x, mu=0, sigma=1, nu=5 ), 0,1, add=TRUE, lwd=3,lty=4, col=4)
curve(dlogitTF(x, mu=0, sigma=1, nu=1 ), 0,1, add=TRUE, lwd=3,lty=5, col=5)
legend("top",
       legend=c("nu = 1000","nu = 10","nu = 5","nu = 1"),
       lty=2:5, col=2:5, cex=1, lwd=3)

## ----trunSST, echo=TRUE,fig.show='hide',fig.path="",fig.asp=1,fig.path="",message=FALSE----
# generate the distribution 
library(gamlss.tr)
gen.trun(c(0,1),"SST",type="both")
# different mu
curve(dSSTtr(x, mu=.1, sigma=.1, nu=1, tau=10), 0,1, lwd=3, lty=2, col=2)
curve(dSSTtr(x, mu=.5, sigma=.1, nu=1, tau=10), 0,1, lwd=3, lty=3, col=3, add=TRUE)
curve(dSSTtr(x, mu=.9, sigma=.1, nu=1, tau=10), 0,1, lwd=3, lty=4, col=4, add=TRUE)
title("(a)")
legend("top",
       legend=c("mu = .1","mu = .5","mu = .9"),
       lty=2:4, col=2:4, cex=1, lwd=3)
# different sigma
curve(dSSTtr(x, mu=.5, sigma=.1, nu=1, tau=10), 0,1, lwd=3, lty=2, col=2)
curve(dSSTtr(x, mu=.5, sigma=.2, nu=1, tau=10), 0,1, lwd=3, lty=3, col=3, add=TRUE)
curve(dSSTtr(x, mu=.5, sigma=.5, nu=1, tau=10), 0,1, lwd=3, lty=4, col=4, add=TRUE)
title("(b)")
legend("topleft",
       legend=c("sigma = .1","sigma = .2","sigma = .5"),
       lty=2:4, col=2:4, cex=1, lwd=3)
# different nu
curve(dSSTtr(x, mu=.5, sigma=.2, nu=.1, tau=10), 0,1, lwd=3, lty=2, col=2)
curve(dSSTtr(x, mu=.5, sigma=.2, nu=1,  tau=10), 0,1, lwd=3, lty=3, col=3, add=TRUE)
curve(dSSTtr(x, mu=.5, sigma=.2, nu=2,  tau=10), 0,1, lwd=3, lty=4, col=4, add=TRUE)
title("(c)")
legend("topleft",
       legend=c("nu = .1","nu = 2","nu = .5"),
       lty=2:4, col=2:4, cex=1, lwd=3)
# different tau 
curve(dSSTtr(x, mu=.5, sigma=.2, nu=1, tau=3 ), 0,1, lwd=3, lty=2, col=2)
curve(dSSTtr(x, mu=.5, sigma=.2, nu=1, tau=4) , 0,1, lwd=3, lty=3, col=3, add=TRUE)
curve(dSSTtr(x, mu=.5, sigma=.2, nu=1, tau=10), 0,1, lwd=3, lty=4, col=4, add=TRUE)
title("(d)")
legend("topleft",
       legend=c("tau = 3","tau = 4","tau = 10"),
       lty=2:4, col=2:4, cex=1, lwd=3)

## ------------------------------------------------------------------------
library(gamlss.inf)
gen.Family(family="SST", type="logit")
gen.Inf0to1(family="logitSST",  type.of.Inflation="Zero&One")

## ----INFlogitSST, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
plotlogitSSTInf0to1(mu= 1, sigma=1, nu=1, tau=10, xi0=.1, xi1=.2); title("(a)")
plotlogitSSTInf0to1(mu=-1, sigma=1, nu=1, tau=10, xi0=.1, xi1=.2); title("(b)")
plotlogitSSTInf0to1(mu=-1, sigma=2, nu=1, tau=10, xi0=.1, xi1=.2); title("(c)")
plotlogitSSTInf0to1(mu=0,  sigma=2, nu=1, tau=10, xi0=.1, xi1=.2); title("(d)")
plotlogitSSTInf0to1(mu=0,  sigma=1, nu=10,tau=10, xi0=.1, xi1=.2); title("(e)")
plotlogitSSTInf0to1(mu=0,  sigma=1, nu=1, tau=3,  xi0=.1, xi1=.2); title("(f)")
plotlogitSSTInf0to1(mu=0,  sigma=1, nu=2, tau=3,  xi0=.5, xi1=.1); title("(g)")
plotlogitSSTInf0to1(mu=0,  sigma=1, nu=.3,tau=100,xi0=.1, xi1=.5); title("(h)")

## ----ALLINFlogitSST, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
# plotting the pdf -------------------------------
curve(dlogitSSTInf0to1(x, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2), 
      0.001,0.999, ylab="pdf",  main="(a)") 
# getting the probabilities
p0 <- dlogitSSTInf0to1(x=0, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2)
p1 <- dlogitSSTInf0to1(x=1, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2)
points(c(0,1), c(p0,p1), col="blue")
lines(c(0,1), c(p0,p1), col="blue", type="h")
# plotting the cdf -------------------------------
curve(plogitSSTInf0to1(x, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2), 
      0.0001,0.999, ylim=c(0,1), ylab="cdf", main="(b)")
#points(c(0), c(p0), col="blue")
lines(c(0), c(p0), col="blue", type="h")
p1 <- plogitSSTInf0to1(q=.999, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2)
lines(c(1,1),c(p1,1))
# plotting the inverse cdf -----------------------
curve(qlogitSSTInf0to1(x, mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2), 
      0.0001,0.999, ylim=c(0,1), ylab="inverse cdf", main="(c)")
# plottind simulated data 
truehist(rlogitSSTInf0to1(1000,mu=0, sigma=1, nu=.8, tau=10, xi0=.1, xi1=.2), 
         main="(d)")

## ------------------------------------------------------------------------
library(gamlss)     # loading gamlss package    
library(gamlss.inf) 
# creating  data 
set.seed(324)
 y0 <- rBEINF0(1000, mu=.3, sigma=.3, nu=.15)# p0=0.13
 y1 <- rBEINF1(1000, mu=.3, sigma=.3, nu=.15)# p1=0.13
y01 <- rBEINF(1000, mu=.3, sigma=.3, nu=0.1, tau=0.2) # p0=0.769, p1=0.1538

## ----BEINF,fig.path="",  fig.asp=1, fig.show='hide',fig.path="",  fig.asp=1----
library(MASS)
truehist(y0)
truehist(y1)
truehist(y01)

## ----cache=TRUE----------------------------------------------------------
 g0 <- gamlss(y0~1, family=BEINF0)
 t0 <- gamlssInf0to1(y=y0, mu.formula=~1, family=BE, trace=TRUE)
AIC(g0,t0, k=0)

## ------------------------------------------------------------------------
coef(g0, "nu")
coef(t0, "xi0")

## ------------------------------------------------------------------------
fitted(t0, "xi0")[1]
fitted(g0, "nu")[1]

## ------------------------------------------------------------------------
fitted(g0, "nu")[1]/(1+fitted(g0, "nu"))[1]

## ------------------------------------------------------------------------
summary(t0)
summary(g0)

## ------------------------------------------------------------------------
vcov(t0)
vcov(g0)

## ----RESIDZERO,fig.path="",  fig.asp=1, fig.show='hide'------------------
plot(resid(t0), pch="+")
points(resid(g0), col="red")

## ----FITTEDZERO, fig.show='hide',fig.path="",  fig.asp=1-----------------
# generate the beta distribution inflated at 0
gen.Inf0to1("BE", type="Zero")

plotBEINF0(mu=fitted(g0, "mu")[1], sigma=fitted(g0, "sigma")[1],
           nu=fitted(g0, "nu")[1], main="(a)", ylab="density")

plotBEInf0(mu=fitted(t0, "mu")[1], sigma=fitted(t0, "sigma")[1],
           xi0=fitted(t0, "xi0")[1]) ; title("(b)")

## ------------------------------------------------------------------------
 g1 <- gamlss(y1~1, family=BEINF1)
 t1 <- gamlssInf0to1(y=y1, mu.formula=~1, family=BE)
AIC(g1,t1, k=0)

## ------------------------------------------------------------------------
coef(g1, "nu")
coef(t1, "xi1")

## ------------------------------------------------------------------------
fitted(t1, "xi1")[1]
fitted(g1, "nu")[1]

## ------------------------------------------------------------------------
fitted(g1, "nu")[1]/(1+fitted(g1, "nu"))[1]

## ------------------------------------------------------------------------
summary(t1)
summary(g1)

## ------------------------------------------------------------------------
vcov(t1)
vcov(g1)

## ----FITTEDONE, fig.show='hide',fig.path="",  fig.asp=1------------------
# generate the
gen.Inf0to1("BE", type="One")

plotBEINF1(mu=fitted(g1, "mu")[1], sigma=fitted(g1, "sigma")[1],
           nu=fitted(g1, "nu")[1], main="(a)", ylab="density")

plotBEInf1(mu=fitted(t1, "mu")[1], sigma=fitted(t1, "sigma")[1],
           xi1=fitted(t1, "xi1")[1]) ; title("(b)")

## ------------------------------------------------------------------------
 g01 <- gamlss(y01~1, family=BEINF)
 t01 <- gamlssInf0to1(y=y01, mu.formula=~1, family=BE)
AIC(g01,t01, k=0)

## ------------------------------------------------------------------------
coef(g01, "nu")
coef(t01, "xi0")
coef(g01, "tau")
coef(t01, "xi1")

## ------------------------------------------------------------------------
fitted(t01, "xi0")[1]
fitted(g01, "nu")[1]
fitted(t01, "xi1")[1]
fitted(g01, "tau")[1]

## ------------------------------------------------------------------------
# probability for y=0
fitted(g01, "nu")[1]/(1+fitted(g01, "nu")+fitted(g01, "tau"))[1]
fitted(t01, "xi0")[1]/(1+fitted(t01, "xi0")+fitted(t01, "xi1"))[1]
# probability for y=1
fitted(g01, "tau")[1]/(1+fitted(g01, "nu")+fitted(g01, "tau"))[1]
fitted(t01, "xi1")[1]/(1+fitted(t01, "xi0")+fitted(t01, "xi1"))[1]

## ------------------------------------------------------------------------
summary(t01)

## ------------------------------------------------------------------------
vcov(t01)
vcov(g01)

## ----RESIDZEROONE,fig.path="",  fig.asp=1, fig.show='hide'---------------
plot(resid(t01), pch="+")
points(resid(g01), col="red")

## ----FITTEDZEROONE, fig.show='hide',fig.path="",  fig.asp=1--------------
# generate the
gen.Inf0to1("BE", type="Zero&One")
plotBEINF(mu=fitted(g01, "mu")[1], sigma=fitted(g01, "sigma")[1],
           nu=fitted(g01, "nu")[1], tau=fitted(g01, "tau")[1],
           main="(a)", ylab="density")
plotBEInf0to1(mu=fitted(t01, "mu")[1], sigma=fitted(t01, "sigma")[1],
            xi0=fitted(t01, "xi0")[1],  xi1=fitted(t01, "xi1")[1])
title("(b)")

## ----SIMULREG, fig.show='hide',fig.path="",  fig.asp=1-------------------
# generating x ----------
set.seed(3210)
x <- (runif(1000)*4)-2
range(x)
data(sda)
fmu <- splinefun(sda$x, sda$mu)
curve(fmu, -2,2)
fsigma <- splinefun(sda$x, sda$sigma)
curve(fsigma, -2,2)
fnu <- splinefun(sda$x, sda$nu)
curve(fnu, -2,2)
ftau <- splinefun(sda$x, sda$tau)
curve(ftau, -2,2)

## ----SIMULHIST, fig.show='hide',fig.path="",  fig.asp=1------------------
# generating x ----------
set.seed(1234)
 y0 <- rBEINF0(1000, mu=fmu(x), sigma=fsigma(x), nu=fnu(x))
 y1 <- rBEINF1(1000, mu=fmu(x), sigma=fsigma(x), nu=ftau(x))
y01 <- rBEINF(1000,  mu=fmu(x), sigma=fsigma(x), nu=fnu(x), tau=ftau(x))
# plotting the y's
plot(x,y0, col="darkgray")
plot(x,y1, col="darkgray")
plot(x,y01, col="darkgray")

## ----cache=TRUE----------------------------------------------------------
 g0p <- gamlss(y0~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), family=BEINF0)
g0p1 <- gamlss(y0~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), family=BEZI)
 t0p <- gamlssInf0to1(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x),
                  xi0.fo=~pb(x), family="BE", trace=TRUE)

AIC(g0p,g0p1, t0p)
summary(t0p)

## ----BETA0FIT, fig.show='hide',fig.path="",  fig.asp=1-------------------
# mu
curve(fmu, -2,2, main="mu", lwd=3)
lines(fitted(g0p)[order(x)]~x[order(x)], col="red",   lty=2, lwd=3)
lines(fitted(t0p, "mu")[order(x)]~x[order(x)], col="blue",   lty=2, lwd=3)
# sigma
curve(fsigma, -2,2, main="sigma", lwd=3)
lines(fitted(g0p, "sigma")[order(x)]~x[order(x)], col="red",   lty=2, lwd=3)
lines(fitted(t0p, "sigma")[order(x)]~x[order(x)], col="blue",   lty=2, lwd=3)
# nu
curve(fnu, -2,2, main="nu", lwd=3)
lines(fitted(g0p, "nu")[order(x)]~x[order(x)], col="red",   lty=2, lwd=3)
fp <- fitted(t0p, "xi0")
fn <- fp/(1-fp)
lines(fn[order(x)]~x[order(x)], col="blue",   lty=2, lwd=3)

## ----termplotat0, fig.show='hide',fig.path="",  fig.asp=1, eval=FALSE----
#  # generating x ----------
#  term.plot(g0p);title("gamlss")
#  term.plotInf0to1(t0p,parameter="mu");title("gamlssInf0to1")
#  term.plot(g0p, "sigma")
#  term.plotInf0to1(t0p,parameter="sigma")
#  term.plot(g0p, "nu")
#  term.plotInf0to1(t0p,parameter="xi0")

## ----diagnosticsat0, fig.show='hide',fig.path="",  fig.asp=1-------------
plot(t0p)
wp(t0p)
#par(mar = c(0,1,0,1))
Q.stats(t0p, xvar=x)
#par(mar = c(0,0,1,0))n
centiles.Inf0to1(t0p, xvar=x)

## ----cache=TRUE----------------------------------------------------------
# generate a logit transformed distribution from 0 to 1
gen.Family("SST", "logit")
# fit the model
t0sst <- gamlssInf0to1(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x),
                  xi0.fo=~pb(x), family="logitSST")

## ----cache=TRUE----------------------------------------------------------
# generate a truncated distribution from 0 to 1
library(gamlss.tr)
gen.trun(c(0,1),"SST",type="both")
# fit the model
t0ssttr <- gamlssInf0to1(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x),
                       xi0.fo=~pb(x), family="SSTtr")
GAIC(g0p, t0p, t0sst,t0ssttr )

## ----cache=TRUE----------------------------------------------------------
library(survival)
y0surv<- Surv(y0, y0!=0, type="left")
# creating the distribution
library(gamlss.cens)
# Gaussian
gen.cens("NO", type="left")
# SST distribution
gen.cens("SST", type="left")
# fitting the  model
# Tobit model
s0no <- gamlss( y0surv ~ pb(x), sigma.formula=~pb(x),
               family=NOlc)
# generalized Tobit
s0sst <- gamlss( y0surv ~ pb(x), sigma.formula=~pb(x),
               family=SSTlc)
GAIC(g0p, t0p, t0sst,t0ssttr, s0no, s0sst )

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  s0sst <- gamlss( y0surv ~ pb(x), sigma.formula=~pb(x),
#                 family=SSTlc, n.cyc=100)
#  

## ----cache=TRUE----------------------------------------------------------
# BEINF1
g1p <- gamlss(y1~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), family=BEINF1)
# BEOI
g1p1 <- gamlss(y1~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), family=BEOI)
# BE inflated at 1
t1p <- gamlssInf0to1(y=y1, mu.fo=~pb(x), sigma.fo=~pb(x),
                       xi1.fo=~pb(x), trace=TRUE, family="BE")

# logitSST inflated at 1
gen.Family("SST", "logit")
t1sst <- gamlssInf0to1(y=y1, mu.fo=~pb(x), sigma.fo=~pb(x),
                  xi1.fo=~pb(x), family="logitSST", trace=T)
# truncated SST inflated at 1
# generate a truncated distribution from 0 to 1
library(gamlss.tr)
gen.trun(c(0,1),"SST",type="both")
# fit for starting values
m1 <-gamlss(y1~pb(x), sigma.fo=~pb(x), family=SST)
# fit model
t1ssttr <- gamlssInf0to1(y=y1, mu.fo=~pb(x), sigma.fo=~pb(x),
      xi1.fo=~pb(x), family="SSTtr",
      sigma.start=fitted(m1,"sigma"), trace=T)

## ----cache=TRUE----------------------------------------------------------
# Tobit models
library(survival)
# creating the y variable as survival response
y1surv<- Surv(y1, y1!=1, type="right")
# creating the distributions
library(gamlss.cens)
gen.cens("SST", type="right")
gen.cens("NO", type="right")
# fitting the  models
# tobit model
s1no <- gamlss( y1surv ~ pb(x), sigma.formula=~pb(x),
               family=NOrc)
# generalised Tobit
s1sst <- gamlss( y1surv ~ pb(x), sigma.formula=~pb(x),
               family=SSTrc)
GAIC(g1p, g1p1, t1p, t1sst, t1ssttr, s1no, s1sst)

## ----cache=TRUE----------------------------------------------------------
# BEINF using gamlss
g01p <- gamlss(y01~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), tau.fo=~pb(x),
              family=BEINF)
# Beta inflated using gamlssInf0to1
t01p <- gamlssInf0to1(y=y01, mu.fo=~pb(x), sigma.fo=~pb(x),
              xi0.fo=~pb(x),   xi1.fo=~pb(x),  trace=TRUE, family="BE")

# logistic SST using gamlssInf0to1
gen.Family("SST", "logit")
t01sst <- gamlssInf0to1(y=y01, mu.fo=~pb(x), sigma.fo=~pb(x),
           xi0.fo=~pb(x), xi1.fo=~pb(x), family="logitSST", trace=TRUE)
# generate a truncated distribution from 0 to 1
m1 <-gamlss(y1~pb(x), sigma.fo=~pb(x), family=SST, trace=FALSE)
gen.trun(c(0,1),"SST",type="both")
t01ssttr <- gamlssInf0to1(y=y1, mu.fo=~pb(x), sigma.fo=~pb(x),
       xi0.fo=~pb(x),xi1.fo=~pb(x), family="SSTtr",  trace=TRUE,
       sigma.start=fitted(m1,"sigma"))

AIC(g01p, t01p, t01sst, t01ssttr)

