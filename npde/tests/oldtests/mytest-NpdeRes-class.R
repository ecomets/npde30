context("Testing creation of NpdeRes\n")
#data("theopp",package='npde')
data("theopp")
write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")

test_that("Successful creation of a NpdeRes object by hand", {
  x<-new(Class="NpdeRes")
  tab <- read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  nobs <- dim(tab)[1]
  xres <- data.frame(id=tab$ID, xobs=tab$Time, yobs=tab$Conc, cens=1, ypred=tab$Conc+rnorm(nobs), pd=runif(nobs), npde=rnorm(nobs))
  x@res <- xres
  x@not.miss <- (!is.na(tab$Conc))
  x@xerr <- 0
  x@pd.sim <- matrix(runif(sum(x@not.miss)),ncol=1)
  x@npde.sim <- matrix(rnorm(sum(x@not.miss)),ncol=1)
  x@ntot.obs <- sum(x@not.miss)
  print(x)
  show(x)
  showall(x)
  cat("Summary of simulated pd (should be roughly U(0,1))\n")
  print(summary(x@res$pd))
  cat("Summary of simulated npde (should be roughly N(0,1))\n")
  print(summary(x@res$npde))
  expect_equal(sum(x@not.miss), 120)
  expect_equal(dim(x@pd.sim), c(120,1))
  expect_equal(dim(x@res), c(132,7))
})
