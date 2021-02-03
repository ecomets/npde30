context("Testing creation of NpdeSimData\n")
#data("theopp",package='npde')
data("theopp")
data("simtheopp")
write.table(theopp,file.path(tempdir(),"theopp.tab"),quote=F, row.names=F, na="NA")
write.table(simtheopp,file.path(tempdir(),"simtheopp.tab"),quote=F, row.names=F, na="NA")

test_that("Creating an empty NpdeSimData object", {
  x<-new(Class="NpdeSimData")
  expect_is(x,"NpdeSimData")
  expect_equal(x@nrep,numeric(0))
})

test_that("Successful creation of a NpdeSimData object by hand", {
  xsim<-new(Class="NpdeSimData")
  tab<-read.table(file.path(tempdir(),"simtheopp.tab"), header=T, na=c("NA","."))
  orig<-read.table(file.path(tempdir(),"theopp.tab"), header=T, na=c("NA","."))
  xsim@datsim <- tab
  xsim@nrep <- dim(tab)[1]/dim(orig)[1]
  print(xsim)
  show(xsim) #same Results
  expect_equal(xsim@nrep,100)
})
