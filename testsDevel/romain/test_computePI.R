library(ggplot2)
npdeDir<-"/home/eco/work/npde/romain/romain2101/npde"
testDir<-file.path(npdeDir,"tests","testthat")
progDir<-file.path(npdeDir,"R")
setwd(testDir)

source(file.path(progDir,"plotNpde-unitFunctionsPI.R"))

obsmat <- readRDS( "obs_mat_xtheo_cens.rds")
pimat <- readRDS("pi_mat_xtheo_cens.rds")

# context("Prediction bands - function compute.bands)
test_that("Computing prediction bands for different number of observations - simulating from N(0,1) for different nobs", {
  nobs<-c(10,50,100,200,1000)
  xb<-compute.bands(nobs, nseuil=100)
  xtab<-rbind(
    data.frame(x=nobs, ymin=xb$binf[,1], y=xb$bmed[,1], ymax=xb$bsup[,1], ci="0.025"),
    data.frame(x=nobs, ymin=xb$binf[,2], y=xb$bmed[,2], ymax=xb$bsup[,2], ci="0.5"),
    data.frame(x=nobs, ymin=xb$binf[,3], y=xb$bmed[,3], ymax=xb$bsup[,3], ci="0.975"))
  ggplot(data=xtab, aes(x=x, y=y, group=ci, colour=ci)) + geom_line() + geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=ci), alpha=0.5)
}
cat("Should look like funnel shapes centered on 0 with 3 bands at +/-",qnorm(0.025),"and 0\n")
)

test_that("Computing prediction bands - simulating from N(0,1) for obsmat", {
  nobs<-tapply(obsmat$grp, obsmat$grp, length)
  xb<-compute.bands(sort(unique(nobs))) # just once
  print(xb)
}
)

test_that("Computing prediction bands - simulating from U(0,1) for different nobs", {
  nobs<-c(10,50,100,200,1000)
  xb<-compute.bands(nobs, distrib="unif")
  xtab<-rbind(
    data.frame(x=nobs, ymin=xb$binf[,1], y=xb$bmed[,1], ymax=xb$bsup[,1], ci="0.025"),
    data.frame(x=nobs, ymin=xb$binf[,2], y=xb$bmed[,2], ymax=xb$bsup[,2], ci="0.5"),
    data.frame(x=nobs, ymin=xb$binf[,3], y=xb$bmed[,3], ymax=xb$bsup[,3], ci="0.975"))
  ggplot(data=xtab, aes(x=x, y=y, group=ci, colour=ci)) + geom_line() + geom_ribbon(aes(ymin=ymin, ymax=ymax, fill=ci),alpha=0.5)
}
)

test_that("Computing prediction bands - simulating from N(0,1) for obsmat", {
  nobs<-tapply(obsmat$grp, obsmat$grp, length)
  xb<-compute.bands(nobs)
  print(xb)
  cat("Should look like funnel shapes between 0 and 1 with 3 bands at",qunif(0.025),",",qunif(0.975),"and 0.5\n")
}
)

# context("Prediction bands without approximation - function compute.bands.true)

# context("Prediction intervals - function aux.npdeplot.computepi)
