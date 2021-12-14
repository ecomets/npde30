wbase<-autonpde(namobs="../../data/warfarin.tab",namsim="../../data/simwarfarinBase.tab",
                iid=1,ix=2,iy=4,icov=c(3,6:8),namsav="warfBase", units=list(x="hr",y="ug/L", covariates=c("mg","kg","-","yr")))

test_that("Testing auxiliary functions for scatterplots, no covariate", {
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"x"
  plot.opt$ylab<-"npde"
  xbin<-npde.binning(wbase@data@data$time, wbase@prefs, verbose=FALSE)
  matbin<-data.frame(grp=1:length(xbin$xcent), xcent=xbin$xcent, binlabel = names(xbin$xcent))
  obs.nocov<-data.frame(x=wbase@data@data$time, y=wbase@results@res[,"npde"], grp=xbin$xgrp)
  obs.nocov$cens<-0
  obs.nocov$category<-factor("all")
  pimat.nocov<-aux.npdeplot.pimat(obs.nocov, xcent=matbin$xcent, distrib="norm", approx.pi=TRUE, sim.ypl=NULL)
  p1<-aux.npdeplot.scatter(obs.nocov, pimat.nocov, plot.opt)
  expect_type(p1, "list")
})

test_that("Testing auxiliary functions for scatterplots, covariate sex", {
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"x"
  plot.opt$ylab<-"npde"
  xbin<-npde.binning(wbase@data@data$time, wbase@prefs, verbose=FALSE)
  matbin<-data.frame(grp=1:length(xbin$xcent), xcent=xbin$xcent, binlabel = names(xbin$xcent))
  obs.sex<-data.frame(x=wbase@data@data$time, y=wbase@results@res[,"npde"], grp=xbin$xgrp)
  obs.sex$cens<-0
  obs.sex$category<-paste("sex:", wbase@data@data$sex)
  obs.sex$category <- factor(obs.sex$category)
  obsmat<-obs.sex
  pimat<-NULL
  for(icat in levels(obsmat$category)) {
    obsmat.cov<-obsmat[obsmat$category==icat,]
    pimat.cov<-aux.npdeplot.pimat(obsmat.cov, xcent=matbin$xcent, distrib="norm", approx.pi=TRUE, sim.ypl=NULL)
    pimat<-rbind(pimat,pimat.cov)
  }
  pimat.sex<-pimat
  plot.opt<-plot.opt
  plot.opt$which.cov<-c("sex")
  plot.opt$covsplit<-TRUE
  p1<-aux.npdeplot.scatter(obs.sex, pimat.sex, plot.opt)
  expect_type(p1, "list")
})

test_that("Testing auxiliary functions for scatterplots, covariate weight", {
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"x"
  plot.opt$ylab<-"npde"
  xbin<-npde.binning(wbase@data@data$time, wbase@prefs, verbose=FALSE)
  matbin<-data.frame(grp=1:length(xbin$xcent), xcent=xbin$xcent, binlabel = names(xbin$xcent))
  obsmat<-data.frame(x=wbase@data@data$time, y=wbase@results@res[,"npde"], grp=xbin$xgrp)
  obsmat$cens<-0
  idobs <- wbase["data"]["data"][wbase["data"]["not.miss"], wbase["data"]["name.group"]]
  zecov<-wbase@data@data$wt
  lcov<-"Weight"
  ucov = zecov[match(unique(idobs),idobs)]
  if(is.numeric(ucov) & length(unique(ucov))>plot.opt$ncat){ # Continuous covariatewith more than plot.opt$ncat (default 3)
    if(plot.opt$ncat!=3) { # 3 categories or less
      ncat<-plot.opt$ncat
      seqcat<-seq(0,1,length.out=(ncat+1))
      zecov.cat<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
      nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
      namcat<-paste(namcov,nam1,sep=": ")
      zecov.cat<-factor(zecov.cat, labels=namcat)
    } else { # if more than 3 categories, split in 3 ranges
      zecov.cat<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
      namcat<-paste(lcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
      zecov.cat<-factor(zecov.cat, labels=namcat)
    }
  } else { # Categorical covariate defined as factor, or covariate with less than plot.opt$ncat categories
    namcat<-paste(lcov,unique(ucov), sep=": ")
    zecov.cat<-paste(lcov, zecov, sep=": ")
    zecov.cat<-factor(zecov.cat, labels=namcat)
  }
  obsmat$category<-zecov.cat

  pimat<-NULL
  for(icat in levels(obsmat$category)) {
    obsmat.cov<-obsmat[obsmat$category==icat,]
    pimat.cov<-aux.npdeplot.pimat(obsmat.cov, xcent=matbin$xcent, distrib="norm", approx.pi=TRUE, sim.ypl=NULL)
    pimat<-rbind(pimat,pimat.cov)
  }
  pimat.sex<-pimat
  plot.opt<-plot.opt
  plot.opt$which.cov<-c("sex")
  plot.opt$covsplit<-TRUE
  p1<-aux.npdeplot.scatter(obsmat, pimat.sex, plot.opt)
  expect_type(p1, "list")
})
