# context("Testing creation of NpdeData\n")
#data("theopp",package='npde')

#data<-read.table("../../data/theopp.tab", header=T)
#data("theopp")
#write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")

test_that("Creating an empty NpdeData object", {
  x<-new(Class="NpdeData")
  expect_is(x,"NpdeData")
  expect_equal(x@N,0)
})

context("Creating an npdeData \n")

test_that("Successful creation of a NpdeData object by hand", {
  x<-new(Class="NpdeData")
  x@name.predictor<-"Time"
  x@name.group <- "ID"
  x@name.response <- "Conc"
  x@name.covariates <- "Wt"
 # tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  tab<-read.table("../../data/theopp.tab", header=T, na=c("NA","."))
  x@N <- length(unique(tab[,x@name.group]))
  x@data <- data.frame(tab[,x@name.group], tab[,x@name.predictor], tab[,x@name.response], mdv=as.integer(is.na(tab[,x@name.response])), tab[,x@name.covariates] )
  colnames(x@data) <- c(x@name.group, x@name.predictor, x@name.response, "mdv", x@name.covariates)
  x@ntot.obs <- dim(x@data)[1]-sum(x@data$mdv)
  nobs<-unlist(tapply(x@data[x@data$mdv==0,x@name.group], x@data[x@data$mdv==0,x@name.group], length ))
  x@nind.obs <- as.numeric(nobs)
  expect_is(x,"NpdeData")
  expect_equal(sum(x@nind.obs),120)
})


test_that("Covariates given as a vector of names", {
  x<-new(Class="NpdeData")
  x@name.predictor<-"time"
  x@name.group <- "id"
  x@name.response <- "dv"
  x@name.covariates <- c("wt","sex","age")
  # tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  x@N <- length(unique(tab[,x@name.group]))
  x@data <- data.frame(tab[,x@name.group], tab[,x@name.predictor], tab[,x@name.response], mdv=as.integer(is.na(tab[,x@name.response])), tab[,x@name.covariates] )
  colnames(x@data) <- c(x@name.group, x@name.predictor, x@name.response, "mdv", x@name.covariates)
  x@ntot.obs <- dim(x@data)[1]-sum(x@data$mdv)
  nobs<-unlist(tapply(x@data[x@data$mdv==0,x@name.group], x@data[x@data$mdv==0,x@name.group], length ))
  x@nind.obs <- as.numeric(nobs)
  expect_is(x,"NpdeData")
  expect_equal(sum(x@nind.obs),dim(x@data)[1])
  expect_equal(x@name.covariates, c("wt","sex","age"))
  expect_equal(match(x@name.covariates, colnames(x@data)), c(5,6,7))
})

test_that("Covariates given as a vector of integers", {
  x<-new(Class="NpdeData")
  x@name.predictor<-"time"
  x@name.group <- "id"
  x@name.response <- "dv"
  # tab<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  x@name.covariates <- colnames(tab)[c(3,6:8)]
  x@N <- length(unique(tab[,x@name.group]))
  x@data <- data.frame(tab[,x@name.group], tab[,x@name.predictor], tab[,x@name.response], mdv=as.integer(is.na(tab[,x@name.response])), tab[,x@name.covariates] )
  colnames(x@data) <- c(x@name.group, x@name.predictor, x@name.response, "mdv", x@name.covariates)
  x@ntot.obs <- dim(x@data)[1]-sum(x@data$mdv)
  nobs<-unlist(tapply(x@data[x@data$mdv==0,x@name.group], x@data[x@data$mdv==0,x@name.group], length ))
  x@nind.obs <- as.numeric(nobs)
  expect_is(x,"NpdeData")
  expect_equal(sum(x@nind.obs),dim(x@data)[1])
  expect_equal(x@name.covariates, c("amt","wt","sex","age"))
  expect_equal(match(x@name.covariates, colnames(tab)), c(3,6:8))
})



