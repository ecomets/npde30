wbase<-autonpde(namobs="../../data/warfarin.tab",namsim="../../data/simwarfarinBase.tab",
                iid=1,ix=2,iy=4,icov=c(3,6:8),namsav="warfBase", units=list(x="hr",y="ug/L", covariates=c("mg","kg","-","yr")))

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.dist(wbase,  which.y="npde", dist.type="qqplot")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.dist(wbase,  which.y="npde", dist.type="ecdf")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, no covariate", {
  p1<-npde.plot.dist(wbase,  which.y="npde", dist.type="hist")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by sex", {
  p1<-npde.plot.dist(wbase,  which.y="npde", dist.type="qqplot", covsplit=TRUE, which.y.cov="sex")
  expect_type(p1, "list")
})

test_that("Testing main functions for scatterplots, split by covariates", {
  p1<-npde.plot.dist(wbase,  which.y="npde", dist.type="hist", covsplit=TRUE, which.y.cov="all")
  expect_type(p1, "list")
})
