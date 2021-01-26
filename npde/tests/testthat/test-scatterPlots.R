wbase<-autonpde(namobs="../../data/warfarin.tab",namsim="../../data/simwarfarinBase.tab",
                iid=1,ix=2,iy=4,icov=c(3,6:8),namsav="warfBase", units=list(x="hr",y="ug/L", covariates=c("mg","kg","-","yr")))

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase)
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase, which.x="pred")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase)
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by sex", {
  p1<-npde.plot.scatterplot(wbase, covsplit=TRUE, which.y.cov="sex")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by covariates", {
  p1<-npde.plot.scatterplot(wbase, covsplit=TRUE, which.y.cov="all")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase,  which.y="pd")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase,  which.y="yobs")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.scatterplot(wbase,  which.y="pd")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by sex", {
  p1<-npde.plot.scatterplot(wbase,  which.y="pd", covsplit=TRUE, which.y.cov="sex")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by covariates", {
  p1<-npde.plot.scatterplot(wbase,  which.y="pd", covsplit=TRUE, which.y.cov="all")
  expect_type(p1, "list")
})
