context("Computing npde\n")


context("Package examples - long tests skipped on CRAN\n")

test_that("Theophylline example from documentation", {
  data(theopp)
  data(simtheopp)
  # Calling autonpde with dataframes
  x<-autonpde(theopp,simtheopp,ix="Time",iy="Conc",iid="ID",boolsave=FALSE)
  print(x)
})

test_that("Theophylline example with covariates", {
  skip_on_cran()
  data(theopp)
  data(simtheopp)
  # Calling autonpde with dataframes
  x<-autonpde(theopp,simtheopp,ix="Time",iy="Conc",iid="ID",icov="Wt",boolsave=FALSE)
  y<-gof.test(x,covsplit=TRUE)
  print(y)
})

test_that("Viral load example", {
  skip_on_cran()
})

