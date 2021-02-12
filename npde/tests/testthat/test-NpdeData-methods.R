# context("Testing NpdeData methods\n")
#data("warfarin",package='npde')

#data<-read.table("../../data/warfarin.tab", header=T)
#write.table(warfarin,file.path(tempdir(),"warfarin.tab"),quote=F, row.names=F, na="NA")

test_that("Names given explicitly", {
  iid <- "id"
  ix<- "time"
  iy <- "dv"
  imdv<-icens<-icov<-iipred <- 0
  units<-list(x="hr",y="mg/L")
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  xdat<-npdeData(name.data=tab, header=T, name.group=iid, name.predictor=ix, 
                 name.response=iy, name.covariates=icov, name.miss=imdv, 
                 name.cens=icens, name.ipred=iipred, units=units,
                 verbose=TRUE,detect=FALSE)
  expect_equal(sum(xdat@nind.obs),dim(xdat@data)[1])
  expect_length(xdat@name.covariates, 0)
})

test_that("Names given explicitly, with covariates", {
  iid <- "id"
  ix<- "time"
  iy <- "dv"
  icov <-  c("wt","sex","age")
  imdv<-icens<-iipred <- 0
  units<-list(x="hr",y="mg/L")
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  xdat<-npdeData(name.data=tab, header=T, name.group=iid, name.predictor=ix, 
                 name.response=iy, name.covariates=icov, name.miss=imdv, 
                 name.cens=icens, name.ipred=iipred, units=units,
                 verbose=TRUE,detect=FALSE)
  expect_equal(sum(xdat@nind.obs),dim(xdat@data)[1])
  expect_equal(xdat@name.covariates, c("wt","sex","age"))
  expect_equal(match(xdat@name.covariates, colnames(tab)), c(6:8))
})

test_that("Names given a vector of integers", {
  iid <- 1
  ix<- 2
  iy <- 4
  icov <-  c(3, 6:8)
  imdv<-icens<-iipred <- 0
  units<-list(x="hr",y="mg/L")
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  xdat<-npdeData(name.data=tab,header=T,name.group=iid, name.predictor=ix, 
                 name.response=iy, name.covariates=icov, name.miss=imdv, 
                 name.cens=icens, name.ipred=iipred, units=units,
                 verbose=FALSE,detect=FALSE)
  expect_equal(sum(xdat@nind.obs),dim(xdat@data)[1])
  expect_equal(xdat@name.covariates, c("amt","wt","sex","age"))
  expect_equal(match(xdat@name.covariates, colnames(tab)), c(3,6:8))
  
})

test_that("Mixing integers and characters for covariate names, duplicate covariate name", {
  iid <- "id"
  ix<- "time"
  iy <- "dv"
  icov <-  c(3, "wt","sex",7)
  imdv<-icens<-iipred <- 0
  units<-list(x="hr",y="mg/L")
  tab<-read.table("../../data/warfarin.tab", header=T, na=c("NA","."))
  xdat<-npdeData(name.data=tab, header=T, name.group=iid, name.predictor=ix, 
                 name.response=iy, name.covariates=icov, name.miss=imdv, 
                 name.cens=icens, name.ipred=iipred, units=units,
                 verbose=TRUE,detect=FALSE)
  expect_equal(sum(xdat@nind.obs),dim(xdat@data)[1])
  expect_equal(xdat@name.covariates, c("amt","wt","sex"))
  expect_equal(match(xdat@name.covariates, colnames(tab)), c(3,6,7))
})
