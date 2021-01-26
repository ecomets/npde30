pkgname <- "npde"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "npde-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('npde')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("NpdeData-class")
### * NpdeData-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeData-class
### Title: Class "NpdeData" representing the structure of the longitudinal
###   data
### Aliases: NpdeData-class NpdeData print,NpdeData-method
###   summary,NpdeData-method npde.qqplot,NpdeData-method
### Keywords: classes

### ** Examples

## Not run: 
##D methods(class="NpdeData")
##D 
##D showClass("NpdeData")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeData-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeObject-class")
### * NpdeObject-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeObject-class
### Title: Class "NpdeObject"
### Aliases: NpdeObject-class NpdeObject print,NpdeObject-method
###   showall,NpdeObject-method summary,NpdeObject-method
###   test,NpdeObject-method npde.main,NpdeObject npde.save,NpdeObject
###   npde.graphs,NpdeObject plot,NpdeObject
### Keywords: classes

### ** Examples

## Not run: 
##D methods(class="NpdeObject")
##D 
##D showClass("NpdeObject")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeObject-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeRes-class")
### * NpdeRes-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeRes-class
### Title: Class "NpdeRes"
### Aliases: NpdeRes-class NpdeRes NpdeRes-class, print,NpdeRes-method
###   showall,NpdeRes-method summary,NpdeRes-method test,NpdeRes-method
### Keywords: classes internal

### ** Examples

## Not run: 
##D data(theopp)
##D 
##D methods(class="NpdeRes")
##D 
##D showClass("NpdeRes")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeRes-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NpdeSimData-class")
### * NpdeSimData-class

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NpdeSimData-class
### Title: Class "NpdeSimData" representing the structure of the
###   longitudinal data
### Aliases: NpdeSimData-class NpdeSimData
### Keywords: classes

### ** Examples

## Not run: 
##D showClass("NpdeSimData")
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NpdeSimData-class", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("dist.pred.sim")
### * dist.pred.sim

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: dist.pred.sim
### Title: Compute distribution of pd/npde using simulations
### Aliases: dist.pred.sim calcnpde.sim

### ** Examples

## Not run: 
##D data(theopp)
##D data(simtheopp)
##D x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
##D # Use random samples from N(0,1) to obtain a prediction interval on the empirical cdf of the npde
##D plot(x,plot.type="ecdf",bands=TRUE,approx.pi=TRUE)
##D # defaults to computing the pd and npde for 100 simulated datasets
##D # (in the theophylline example, this uses all the simulated datasets)
##D x<-dist.pred.sim(x)
##D # Use the npde from the simulated datasets to obtain a prediction interval on the empirical cdf
##D plot(x,plot.type="ecdf",bands=TRUE,approx.pi=FALSE)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("dist.pred.sim", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("gof.test")
### * gof.test

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: gof.test
### Title: Goodness-of-fit tests for npde
### Aliases: gof.test gof.test.default gof.test.numeric gof.test.NpdeRes
###   printgoftest gof.test.NpdeObject
### Keywords: methods test

### ** Examples

## Not run: 
##D data(theopp)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("gof.test", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("kurtosis")
### * kurtosis

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: kurtosis
### Title: Kurtosis
### Aliases: kurtosis
### Keywords: univar

### ** Examples

## Not run: 
##D x <- rnorm(100)
##D kurtosis(x)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("kurtosis", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("npde-package")
### * npde-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: npde-package
### Title: Normalised prediction distribution errors for nonlinear
###   mixed-effect models
### Aliases: npde npde-package
### Keywords: models

### ** Examples

## Not run: 
##D data(theopp)
##D data(simtheopp)
##D 
##D # Calling autonpde with dataframes
##D 
##D x<-autonpde(theopp,simtheopp,ix="Time",iy="Conc",iid="ID",boolsave=FALSE)
##D print(x)
##D 
##D # Calling autonpde with names of files to be read from disk
##D 
##D write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
##D write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
##D x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
##D ix = 3, iy = 4, boolsave = FALSE)
##D 
##D print(x)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("npde-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("npde")
### * npde

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: npde
### Title: Compute normalised prediction distribution errors
### Aliases: npde autonpde
### Keywords: models

### ** Examples

## Not run: 
##D data(theopp)
##D data(simtheopp)
##D 
##D # Calling autonpde with dataframes
##D 
##D x<-autonpde(theopp,simtheopp,1,3,4,boolsave=FALSE)
##D x
##D 
##D # Calling autonpde with names of files to be read from disk
##D 
##D write.table(theopp,"theopp.tab",quote=FALSE,row.names=FALSE)
##D write.table(simtheopp,"simtheopp.tab",quote=FALSE,row.names=FALSE)
##D x<-autonpde(namobs="theopp.tab", namsim="simtheopp.tab", iid = 1,
##D ix = 3, iy = 4, imdv=0, boolsave = FALSE)
##D 
##D head(x["results"]["res"])
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("npde", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("npdeData")
### * npdeData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: npdeData
### Title: Creates a NpdeData object
### Aliases: npdeData
### Keywords: models

### ** Examples

## Not run: 
##D data(theopp)
##D 
##D x<-npdeData(theopp) # Automatic detection
##D print(x)
##D x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc",
##D name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg")) # Explicit
##D print(x)
##D plot(x)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("npdeData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeData")
### * plot.NpdeData

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeData
### Title: Plots a NpdeData object
### Aliases: plot.NpdeData
### Keywords: plot

### ** Examples

## Not run: 
##D data(theopp)
##D 
##D x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc",
##D name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg"))
##D plot(x)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeData", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeObject")
### * plot.NpdeObject

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeObject
### Title: Plots a NpdeObject object
### Aliases: plot.NpdeObject
### Keywords: plot

### ** Examples

## Not run: 
##D data(theopp)
##D data(simtheopp)
##D 
##D x<-autonpde(theopp,simtheopp,iid="ID",ix="Time", iy="Conc", boolsave=FALSE)
##D plot(x)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeObject", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plot.NpdeRes")
### * plot.NpdeRes

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.NpdeRes
### Title: Plots a NpdeRes object
### Aliases: plot.NpdeRes
### Keywords: internal plot

### ** Examples

## Not run: 
##D data(theopp)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.NpdeRes", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("remifent")
### * remifent

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: remifent
### Title: Pharmacokinetics of Remifentanil
### Aliases: remifent simremifent simremifent_base
### Keywords: datasets

### ** Examples

data(remifent)
str(remifent)

#Plotting the remifentanil data
plot(Conc~Time,data=remifent,xlab="Time after dose (min)", ylab="Theophylline concentration (ug/L)")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("remifent", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("simtheopp")
### * simtheopp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: simtheopp
### Title: Simulated data for the computation of normalised prediction
###   distribution errors
### Aliases: simtheopp
### Keywords: datasets

### ** Examples

## Not run: 
##D data(simtheopp)
##D 
##D # Plotting the simulated data for subject 1 in the first simulation
##D plot(ysim[2:12]~xsim[2:12],data=simtheopp,xlab="Time after dose (hr)",
##D ylab="Theophylline concentration (mg/L)",type="l",
##D main="Example of simulated data for subject 1")
##D 
##D # Plotting a 90% prediction interval for the observations in theopp
##D # using the simulated data in simtheopp
##D # note : differences in doses between subjects are not taken into account
##D data(theopp)
##D xpl<-c(0,0.25,0.5,1,2,3.5,5,7,9,12,24)
##D xpl1<-list(c(0,0.1),c(0.2,0.4),c(0.5,0.65),c(0.9,1.2),c(1.9,2.2),c(3.4,4),
##D c(4.9,5.2),c(6.9,7.2),c(8.8,9.4),c(11.5,12.2),c(23.7,24.7))
##D 
##D ypl<-cbind(xpl=xpl,binf=xpl,median=xpl,bsup=xpl)
##D for(i in 1:(length(xpl))) {
##D   vec<-simtheopp$ysim[simtheopp$xsim>=xpl1[[i]][1] &simtheopp$xsim<=xpl1[[i]][2]]
##D   ypl[i,2:4]<-quantile(vec,c(0.05,0.5,0.95))
##D }
##D plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
##D ylab="Theophylline concentration (mg/L)")
##D lines(ypl[,1],ypl[,3],lwd=2)
##D lines(ypl[,1],ypl[,2],lty=2)
##D lines(ypl[,1],ypl[,4],lty=2)
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("simtheopp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("skewness")
### * skewness

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: skewness
### Title: Skewness
### Aliases: skewness
### Keywords: univar

### ** Examples

## Not run: 
##D x <- rnorm(100)
##D skewness(x)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("skewness", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("theopp")
### * theopp

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: theopp
### Title: Pharmacokinetics of theophylline
### Aliases: theopp
### Keywords: datasets

### ** Examples

## Not run: 
##D data(theopp)
##D 
##D #Plotting the theophylline data
##D plot(Conc~Time,data=theopp,xlab="Time after dose (hr)",
##D ylab="Theophylline concentration (mg/L)")
##D 
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("theopp", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("virload")
### * virload

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: virload
### Title: Simulated HIV viral loads in HIV patients
### Aliases: virload virload20 virloadMDV20 virload50
### Keywords: datasets

### ** Examples

data(virload)
str(virload)
data(virload50)

#Plotting the data
plot(Log_VL~Time,data=virload,xlab="Time (d)",ylab="Viral loads, base 10 log-scale (cp/mL)")
plot(Log_VL~Time,data=virload50,xlab="Time (d)",ylab="Viral loads, base 10 log-scale (cp/mL)")




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("virload", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
