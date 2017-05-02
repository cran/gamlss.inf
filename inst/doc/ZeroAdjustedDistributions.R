## ----logTFDist, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
# generate the distribution
library(gamlss)
gen.Family("TF", type="log")
# different mu
curve(dlogTF(x, mu=-5, sigma=1, nu=10), 0,10, n = 1001, ylim=c(0,1),
      lwd=2, lty=2, col=2)
title("(a)")
curve(dlogTF(x, mu=-1, sigma=1, nu=10), 0,10, add=TRUE, lwd=2, lty=3, col=3)
curve(dlogTF(x, mu=0,  sigma=1, nu=10), 0,10, add=TRUE, lwd=2, lty=4, col=4)
curve(dlogTF(x, mu=1,  sigma=1, nu=10), 0,10, add=TRUE, lwd=2, lty=5, col=5)
curve(dlogTF(x, mu=2,  sigma=1, nu=10), 0,10, add=TRUE, lwd=2, lty=6, col=6)
legend("topright",
       legend=c("mu = -5","mu = -1","mu = 0","mu = 1","mu = 2"),
       lty=2:6, col=2:6, cex=1)
# different sigma
curve(dlogTF(x, mu=0, sigma=.5, nu=10), 0,2, ylim=c(0,3),
      lwd=2, lty=2, col=2)
title(("(b)"))
curve(dlogTF(x, mu=0, sigma=1, nu=10),  0,2, add=TRUE, lwd=2, lty=3, col=3)
curve(dlogTF(x, mu=0, sigma=2, nu=10),  0,2, add=TRUE, lwd=2, lty=4, col=4)
curve(dlogTF(x, mu=0, sigma=5, nu=10 ), 0,2, add=TRUE, lwd=2, lty=5, col=5)
legend("topright",
       legend=c("sigma = .5","sigma = 1","sigma = 2","sigma = 5"),
       lty=2:5, col=2:5, cex=1)
# different nu
curve(dlogTF(x, mu=0, sigma=.5, nu=1000), 0,3, ylim=c(0,1.1),
       lwd=2, lty=2, col=2)
title("(c)")
curve(dlogTF(x, mu=0, sigma=.5, nu=10), 0,3, add=TRUE, lwd=2, lty=3, col=3)
curve(dlogTF(x, mu=0, sigma=.5, nu=2 ), 0,3, add=TRUE, lwd=2, lty=4, col=4)
curve(dlogTF(x, mu=0, sigma=.5, nu=1 ), 0,3, add=TRUE, lwd=2, lty=5, col=5)
legend("topright",
       legend=c("nu = 1000","nu = 10","nu = 2","nu = 1"),
       lty=2:5, col=2:5, cex=1)

## ----trunSST, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
# generate the distribution 
library(gamlss.tr)
gen.trun(0,"SST",type="left")
# different mu
curve(dSSTtr(x, mu=.1, sigma=.5, nu=1, tau=10), 0,4, lwd=2, lty=2, col=2)
curve(dSSTtr(x, mu= 1, sigma=.5, nu=1, tau=10), 0,4, lwd=2, lty=3, col=3, 
      add=TRUE)
curve(dSSTtr(x, mu= 2, sigma=.5, nu=1, tau=10), 0,4, lwd=2, lty=4, col=4,
       add=TRUE)
title("(a)")
legend("topright",
       legend=c("mu = .1","mu = 1","mu = 2"),
       lty=2:4, col=2:4, cex=1)
# different sigma
curve(dSSTtr(x, mu=2, sigma=0.5, nu=1, tau=10), 0,4, lwd=2, lty=2, col=2 )
curve(dSSTtr(x, mu=2, sigma=  1, nu=1, tau=10), 0,4, lwd=2, lty=3, col=3,
      add=TRUE)
curve(dSSTtr(x, mu=2, sigma=  2, nu=1, tau=10), 0,4, lwd=2, lty=4, col=4,
      add=TRUE)
title("(b)")
legend("topright",
       legend=c("sigma = .5","sigma = 1","sigma = 2"),
       lty=2:4, col=2:4, cex=1)
# different nu
curve(dSSTtr(x, mu=2, sigma=.5, nu=0.1, tau=10), 0,4, lwd=2, lty=2, col=2)
curve(dSSTtr(x, mu=2, sigma=.5, nu=  1, tau=10), 0,4, lwd=2, lty=3, col=3, 
      add=TRUE)
curve(dSSTtr(x, mu=2, sigma=.5, nu=  2, tau=10), 0,4, lwd=2, lty=4, col=4,
      add=TRUE)
title("(c)")
legend("topright",
       legend=c("nu = 0.1","nu = 1","nu = 2"),
       lty=2:4, col=2:4, cex=1)
# different tau 
curve(dSSTtr(x, mu=2, sigma=.5, nu=1, tau=  3),  0,4, lwd=2, lty=2, col=2)
curve(dSSTtr(x, mu=2, sigma=.5, nu=1, tau=  5),  0,4, lwd=2, lty=3, col=3,
      add=TRUE)
curve(dSSTtr(x, mu=2, sigma=.5, nu=1, tau=100),  0,4, lwd=2, lty=4, col=4,
       add=TRUE)
title("(d)")
legend("topright",
       legend=c("tau = 3","tau = 5","tau = 100"),
       lty=2:4, col=2:4, cex=1)

## ------------------------------------------------------------------------
library(gamlss.inf)
gen.Family(family="SST", type="log")
gen.Zadj(family="logSST")

## ----logSSTZadj, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
plotlogSSTZadj(mu= 1, sigma=1, nu=1, tau=10, xi0=.1); title("(a)")
plotlogSSTZadj(mu=-1, sigma=1, nu=1, tau=10, xi0=.1); title("(b)")
plotlogSSTZadj(mu=-1, sigma=2, nu=1, tau=10, xi0=.1); title("(c)")
plotlogSSTZadj(mu=0,  sigma=2, nu=1, tau=10, xi0=.1); title("(d)")
plotlogSSTZadj(mu=0,  sigma=1, nu=10,tau=10, xi0=.1); title("(e)")
plotlogSSTZadj(mu=0,  sigma=1, nu=1, tau=3,  xi0=.1); title("(f)")
plotlogSSTZadj(mu=0,  sigma=1, nu=2, tau=3,  xi0=.5); title("(g)")
plotlogSSTZadj(mu=0,  sigma=1, nu=.3,tau=100,xi0=.1); title("(h)")

## ----ALLlogSSTZadj, echo=TRUE, fig.show='hide',fig.path="",  fig.asp=1, message=FALSE----
# plotting the pdf -------------------------------
curve(dlogSSTZadj(x, mu=1, sigma=1, nu=.8, tau=10, xi0=.1),
      0.001,10, ylab="pdf",  main="(a)")
# getting the probabilities
p0 <- dlogSSTZadj(x=0, mu=1, sigma=1, nu=.8, tau=10, xi0=.1)
points(0, p0, col="blue")
lines(0, p0, col="blue", type="h")
# plotting the cdf -------------------------------
curve(plogSSTZadj(x, mu=1, sigma=1, nu=.8, tau=10, xi0=.1),
      0.0001,10, ylim=c(0,1), ylab="cdf", main="(b)")
points(c(0), c(p0), col="blue")
lines(c(0), c(p0), col="blue", type="h")
# plotting the inverse cdf -----------------------
curve(qlogSSTZadj(x, mu=1, sigma=1, nu=.8, tau=10, xi0=.1),
      0.0001,0.99, ylim=c(0,10), ylab="inverse cdf", main="(c)")
# plottind simulated data
set.seed(1000)
truehist(rlogSSTZadj(1000,mu=1, sigma=1, nu=.8, tau=10, xi0=.1),
         main="(d)")

## ----ZAGAplot,fig.path="",  fig.asp=1, fig.show='hide',fig.path="",  fig.asp=1----
library(gamlss)     # loading gamlss package
library(gamlss.inf)
# creating  data
set.seed(3223)
 y0 <- rZAGA(1000, mu=3, sigma=.5, nu=.15)
truehist(y0) 

## ------------------------------------------------------------------------
 g0 <- gamlss(y0~1, family=ZAGA)
 t0 <- gamlssZadj(y=y0, mu.formula=~1, family=GA, trace=TRUE)
AIC(g0,t0, k=0)

## ------------------------------------------------------------------------
coef(g0, "nu")
coef(t0, "xi0")

## ------------------------------------------------------------------------
fitted(t0, "xi0")[1]
fitted(g0, "nu")[1]

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
# generate the 
gen.Zadj("GA")
plotZAGA(mu=fitted(g0, "mu")[1], sigma=fitted(g0, "sigma")[1], 
            nu=fitted(g0, "nu")[1], main="(a)", ylab="density")
plotGAZadj(mu=fitted(g0, "mu")[1], sigma=fitted(g0, "sigma")[1], 
            xi0=fitted(g0, "nu")[1]); title("(b)")

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
fnu <- function(x)
  {f <- splinefun(sda$x, sda$nu) 
  f(x)/6
  }
curve(fnu, -2,2)

## ----SIMULHIST, fig.show='hide',fig.path="",  fig.asp=1------------------
# generating y0 ----------
set.seed(123)
 y0 <- rZAGA(1000, mu=fmu(x), sigma=fsigma(x), nu=fnu(x))
plot(x,y0)

## ------------------------------------------------------------------------
g0p <- gamlss(y0~pb(x), sigma.fo=~pb(x), nu.fo=~pb(x), family=ZAGA)
t0p <- gamlssZadj(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x), 
                  xi0.fo=~pb(x), family="GA", trace=TRUE)
AIC(g0p, t0p)
summary(t0p)

## ----ZeroInfGAT, fig.show='hide',fig.path="",  fig.asp=1-----------------
# mu
curve(fmu, -2,2, main="mu")
lines(fitted(g0p)[order(x)]~x[order(x)], col="red",   lty=2, lwd=2)
lines(fitted(t0p, "mu")[order(x)]~x[order(x)], col="blue",   lty=2, lwd=2)
# sigma
curve(fsigma, -2,2, main="sigma")
lines(fitted(g0p, "sigma")[order(x)]~x[order(x)], col="red",   lty=2, lwd=2)
lines(fitted(t0p, "sigma")[order(x)]~x[order(x)], col="blue",   lty=2, lwd=2)
# nu
curve(fnu, -2,2, main="nu")
lines(fitted(g0p, "nu")[order(x)]~x[order(x)], col="red",   lty=2, lwd=2)
lines(fitted(t0p, "xi0")[order(x)]~x[order(x)], col="blue",   lty=2, lwd=2)

## ----termplotatZadj, fig.show='hide',fig.path="",  fig.asp=1-------------
term.plot(g0p);title("gamlss")
term.plotZadj(t0p);title("gamlssZadj")
term.plot(g0p, "sigma")
term.plotZadj(t0p, "sigma")
term.plot(g0p, "nu")
term.plotZadj(t0p, "xi0")

## ----diagnosticsZasdj, fig.show='hide',fig.path="",  fig.asp=1-----------
plot(t0p)
wp(t0p, ylim.all=1)
Q.stats(t0p, xvar=x)
centiles.Zadj(t0p, xvar=x)

## ----cache=TRUE----------------------------------------------------------
# generate a log-tranform distribution from 0 to infinity
gen.Family("SST", "log")
# fit the model
t0sst <- gamlssZadj(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x), 
                   xi0.fo=~pb(x), family="logSST")

## ----cache=TRUE----------------------------------------------------------
# generate a left truncated distribution SST from 0 to Infty
library(gamlss.tr)
gen.trun(c(0),"SST",type="left")
# fit the model
t0ssttr <- gamlssZadj(y=y0, mu.fo=~pb(x), sigma.fo=~pb(x), 
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
# generalised Tobit
s0sst <- gamlss( y0surv ~ pb(x), sigma.formula=~pb(x),
               family=SSTlc)
GAIC(g0p, t0p, t0sst,t0ssttr, s0no, s0sst )

## ------------------------------------------------------------------------
data(mvi)
# zero adjusted GA
m1 <- gamlss(claimcst0~vehmake+vehbody+vehage+gender+area,
             nu.fo=~vehmake+vehbody+vehage+gender+area, 
             family=ZAGA, data=mvi )
# zero adjusted IG
m2 <- gamlss(claimcst0~vehmake+vehbody+vehage+gender+area,
             nu.fo=~vehmake+vehbody+vehage+gender+area, 
             family=ZAIG, data=mvi )
# zero adjusted GG
m3 <- gamlssZadj(claimcst0, ~vehmake+vehbody+vehage+gender+area, 
             xi0.fo=~vehmake+vehbody+vehage+gender+area,
             family=GG, data=mvi, trace=T, n.cyc=30 )
# zero adjusted BCTo
m4 <- gamlssZadj(claimcst0, ~vehmake+vehbody+vehage+gender+area, 
             xi0.fo=~vehmake+vehbody+vehage+gender+area,
             family=BCTo, data=mvi, trace=T )
AIC(m1,m2,m3,m4)

## ----mviWormPlot, fig.show='hide',fig.path="",  fig.asp=1----------------
wp(m3, ylim.all=1); title('(a)')
wp(m4, ylim.all=1); title('(b)')

