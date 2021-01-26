# context("Testing plot functions\n")

# context("Testing binning\n")
test_that("Binning methods", {
#  skip_on_cran()
  test.xvec<-runif(100)
  par(mfrow=c(2,2))
  test.plot.opt <- list(bin.method="equal", bin.number=10, bin.breaks=c(1:10)/11, bin.extreme=NULL, xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="Equal")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")
  xcheck<-tapply(xbin$xgrp,xbin$xgrp,length)
  expect_equal(min(xcheck),10)
  expect_equal(max(xcheck),10)

  test.plot.opt <- list(bin.method="equal", bin.number=10, bin.breaks=c(1:10)/11, bin.extreme=c(0.02,0.98), xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="Equal with 2% extremes")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")
  xcheck<-tapply(xbin$xgrp,xbin$xgrp,length)
  expect_equal(min(xcheck),2)
  expect_equal(max(xcheck),12)
  expect_equal(length(xcheck[xcheck==2]),2)
  expect_equal(length(xcheck[xcheck==12]),8)

  test.plot.opt <- list(bin.method="user", bin.number=10, bin.breaks=c(0.25,0.4,0.75,0.9),  bin.extreme=NULL, xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="User")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")

  test.plot.opt <- list(bin.method="user", bin.number=10, bin.breaks=c(0.25,0.4,0.75,0.9),  bin.extreme=c(0.02,0.98), xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="User with 2% extremes")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")

  test.plot.opt <- list(bin.method="width", bin.number=10, bin.breaks=c(1:10)/11, bin.extreme=NULL, xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods",main="Width")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")

  test.plot.opt <- list(bin.method="width", bin.number=10, bin.breaks=c(1:10)/11, bin.extreme=c(0.02,0.98), xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="Width with 2% extremes")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")

  test.plot.opt <- list(bin.method="optimal", bin.number=10, bin.breaks=c(1:10)/11,  bin.extreme=NULL, xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="Optimal")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")

  test.plot.opt <- list(bin.method="optimal", bin.number=10, bin.breaks=c(1:10)/11,  bin.extreme=c(0.02,0.98), xlog=FALSE)
  xbin<-npde.binning(test.xvec, test.plot.opt, verbose=TRUE)
  plot(test.xvec,rep(0,length(test.xvec)),pch=20,col="slategray3",xlab="X ~ U(0,1)",ylab="Binning methods", main="Optimal with 2% extremes")
  abline(v=xbin$xbound,lwd=2,col="DarkRed")
  par(mfrow=c(1,1))
})

test_that("Plotting prediction interval", {
#  skip_on_cran()
  par(mfrow=c(1,1))
  nsamp <- c(1,5, 10, 100, 200, 500, 1000)
  xsamp <- compute.bands(nsamp)
  poly.XY <- data.frame(x=rep(nsamp,3), y=c(xsamp$bmed), ylow=c(xsamp$binf),yup=c(xsamp$bsup),
    group=rep(c(rep("Lower",length(nsamp)),rep("Median",length(nsamp)),rep("Upper",length(nsamp))),2) )
  poly.XY$fillcol<-ifelse(poly.XY$group=="Median","pink","lightblue")

  p1<-ggplot(poly.XY, aes(x=x, y=y, group=group)) + geom_line(aes(x=x, y=y, col=fillcol)) + geom_ribbon(aes(ymax=yup, ymin=ylow, fill=fillcol),alpha=1/2) + scale_colour_manual("", values=c("lightblue","pink"), guide="none") + scale_fill_manual("", values=c("lightblue","pink"),labels=c("95% PI","Median")) + theme_minimal() + labs(x="Nb of samples",y="PI obtained by simulations", fill="PI") + theme(legend.position = "bottom") + geom_vline(xintercept=200, color="slategray3", lty=2)
  print(p1)
})
