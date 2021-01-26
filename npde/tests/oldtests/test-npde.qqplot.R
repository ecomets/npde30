context("QQ plot of Y versus X\n")

test_that("QQ-plot of Y versus X, same distribution", {
  x<-rnorm(100)
  y<-rnorm(100)
  npde.qqplot(x,y, main="x,y ~ N(0,1)")
})

test_that("QQ-plot of Y versus X", {
  x<-rnorm(100)
  y<-rnorm(100,mean=2)
  npde.qqplot(x,y, main="x~N(0,1), y~N(0,2)")
})

context("QQ plot of X compared to a theoretical distribution \n")

test_that("QQ-plot of X versus a normal distribution N(0,1)", {
  x<-rnorm(100)
  npde.qqplot(x,"qnorm", main="x~N(0,1) vs theoretic N(0,1)")
})

test_that("QQ-plot of X versus a normal distribution N(0,2)", {
  x<-rnorm(100)
  npde.qqplot(x,"qnorm",add.args=list(sd=2), main="x~N(0,1) vs theoretic N(0,1)")
})

test_that("QQ-plot of X versus a normal distribution N(0.5,1)", {
  x<-rnorm(100)
  npde.qqplot(x,"qnorm",add.args=list(mean=0.5), main="x~N(0,1) vs theoretic N(0.5,1)")
})

test_that("QQ-plot of a uniform sample versus a uniform distribution", {
  x<-runif(100)
  npde.qqplot(x,"qunif", main="x~N(0,1) vs theoretic U(0,1)")
})

context("QQ plot of Response element in NpdeData object (just to have a 3rd object) \n")
#data("theopp",package='npde')
data("theopp")
write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")

test_that("QQ-plot of Conc versus uniform distribution (silly)", {
  x<-new(Class="NpdeData")
  x@name.predictor<-"Time"
  x@name.group <- "ID"
  x@name.response <- "Conc"
  x@name.covariates <- "Wt"
  tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  x@N <- length(unique(tab[,x@name.group]))
  x@data <- data.frame(tab[,x@name.group], tab[,x@name.predictor], tab[,x@name.response], mdv=as.integer(is.na(tab[,x@name.response])), tab[,x@name.covariates] )
  colnames(x@data) <- c(x@name.group, x@name.predictor, x@name.response, "mdv", x@name.covariates)
  x@ntot.obs <- dim(x@data)[1]-sum(x@data$mdv)
  nobs<-unlist(tapply(x@data[x@data$mdv==0,x@name.group], x@data[x@data$mdv==0,x@name.group], length ))
  x@nind.obs <- as.numeric(nobs)
  xpl<-x@data[,x@name.response]
  xpl<-xpl[!is.na(xpl) & xpl>0]
  npde.qqplot(xpl,"qunif",add.args=list(0,1), main="Conc vs U(0,1)")
  npde.qqplot(xpl,"qunif",add.args=list(0,10), main="Conc vs U(0,10)")
  par(pty="s")
  npde.qqplot(xpl,"qunif",add.args=list(0,10), main="Conc vs U(0,10), user layout", xlab="My X-axis", ylab="My Y-axis", col="steelblue4", lcol="Blue", pch=2, lty=2, lwd=2, cex=0.8, cex.axis=1.3, cex.lab=1.3, cex.main=1.2)
  par(pty="m")
  # TODO - change to create a third npde.qqplot function
})
