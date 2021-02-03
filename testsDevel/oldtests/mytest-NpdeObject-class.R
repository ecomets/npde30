context("Testing creation of NpdeObject\n")
#data("theopp",package='npde')
data("theopp")
data("simtheopp")
write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")
write.table(simtheopp,file.path(tempdir(),"simtheopp.tab"),quote=F, row.names=F, na="NA")

test_that("Successful creation of a NpdeObject object by hand", {
  xdat<-new(Class="NpdeData")
  xdat@name.predictor<-"Time"
  xdat@name.group <- "ID"
  xdat@name.response <- "Conc"
  xdat@name.covariates <- "Wt"
  tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  xdat@N <- length(unique(tab[,xdat@name.group]))
  xdat@data <- data.frame(tab[,xdat@name.group], tab[,xdat@name.predictor], tab[,xdat@name.response], mdv=as.integer(is.na(tab[,xdat@name.response])), tab[,xdat@name.covariates] )
  colnames(xdat@data) <- c(xdat@name.group, xdat@name.predictor, xdat@name.response, "mdv", xdat@name.covariates)
  xdat@ntot.obs <- dim(xdat@data)[1]-sum(xdat@data$mdv)
  nobs<-unlist(tapply(xdat@data[xdat@data$mdv==0,xdat@name.group], xdat@data[xdat@data$mdv==0,xdat@name.group], length ))
  xdat@nind.obs <- as.numeric(nobs)
  
  xsim<-new(Class="NpdeSimData")
  simtab<-read.table(file.path(tempdir(),"simtheopp.tab"), header=T, na=c("NA","."))
  xsim@datsim <- simtab
  xsim@nrep <- dim(simtab)[1]/dim(tab)[1]
  
  xres<-new(Class="NpdeRes")
  nobs <- dim(tab)[1]
  tabres <- data.frame(id=tab$ID, xobs=tab$Time, yobs=tab$Conc, cens=1, ypred=tab$Conc+rnorm(nobs), pd=runif(nobs), npde=rnorm(nobs))
  xres@res <- tabres
  xres@not.miss <- (!is.na(tab$Conc))
  xres@xerr <- 0
  xres@pd.sim <- matrix(runif(sum(xres@not.miss)),ncol=1)
  xres@npde.sim <- matrix(rnorm(sum(xres@not.miss)),ncol=1)
  xres@ntot.obs <- sum(xres@not.miss)
  
  xobj<-new(Class="NpdeObject", data=xdat, sim.data=xsim)
  expect_equal(length(xobj@options), 12)
  expect_equal(length(xobj@prefs), 71)
  expect_equal(length(xobj@prefs$ilist), xobj@data@N)
  expect_equal(xobj@prefs$name.X,xobj@data@name.predictor)
  
  xobj@results<-xres
  
  print(xobj)
  show(xobj)
  cat("Summary of simulated pd (should be roughly U(0,1))\n")
  print(summary(xobj@results@res$pd))
  cat("Summary of simulated npde (should be roughly N(0,1))\n")
  print(summary(xobj@results@res$npde))
})
