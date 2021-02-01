library(ggplot2)

wbase<-autonpde(namobs="../../data/warfarin.tab",namsim="../../data/simwarfarinBase.tab",
                iid=1,ix=2,iy=4,icov=c(3,6:8),namsav="warfBase", units=list(x="hr",y="ug/L", covariates=c("mg","kg","-","yr")))

test_that("Testing auxiliary functions for distribution plots, no covariate", {
  obs.nocov<-wbase@results@res[,"npde",drop=FALSE]
  obs.nocov$category<-factor("all")
  colnames(obs.nocov)[1]<-"x"
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.hist(obs.nocov,plot.opt=plot.opt)
  expect_type(p1, "list")
})


test_that("Testing auxiliary functions for distribution plots, no covariate", {
  obs.nocov<-wbase@results@res[,"npde",drop=FALSE]
  obs.nocov$category<-factor("all")
  obs.nocov$cens<-0
  colnames(obs.nocov)[1]<-"x"
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.dist(obs.nocov,plot.opt=plot.opt)
  expect_type(p1, "list")
})

test_that("Testing auxiliary functions for distribution plots, no covariate", {
  obs.nocov<-wbase@results@res[,"npde",drop=FALSE]
  obs.nocov$category<-factor("all")
  obs.nocov$cens<-0
  colnames(obs.nocov)[1]<-"x"
  plot.opt<-wbase["prefs"]
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.dist(obs.nocov,plot.opt=plot.opt, dist.type="ecdf")
  expect_type(p1, "list")
})


test_that("Testing auxiliary functions for distribution plots, covariate sex", {
  npdeObject<-wbase
  plot.opt<-wbase["prefs"]
  idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  obs.sex<-data.frame(x=wbase@results@res[,"npde"], category=paste("sex:", wbase@data@data$sex))
  obs.sex$category <- factor(obs.sex$category)
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.hist(obs.sex,plot.opt=plot.opt)
  expect_type(p1, "list")
})

test_that("Testing auxiliary functions for distribution plots, covariate sex", {
  npdeObject<-wbase
  plot.opt<-wbase["prefs"]
  idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  obs.sex<-data.frame(x=wbase@results@res[,"npde"], category=paste("sex:", wbase@data@data$sex))
  obs.sex$category <- factor(obs.sex$category)
  obs.sex$cens<-0
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.dist(obs.sex,plot.opt=plot.opt)
  expect_type(p1, "list")
})

test_that("Testing auxiliary functions for distribution plots, covariate weight", {
  npdeObject<-wbase
  plot.opt<-wbase["prefs"]
  idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  zecov<-wbase@data@data$wt
  lcov<-"Weight:"
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
  obs.weight<-data.frame(x=wbase@results@res[,"npde"], category=zecov.cat)
  plot.opt$xlab<-"npde"
  plot.opt$ylab<-"Counts"
  p1<-aux.npdeplot.hist(obs.weight,plot.opt=plot.opt)
  expect_type(p1, "list")
  obs.weight$cens<-0
  p1<-aux.npdeplot.dist(obs.weight,plot.opt=plot.opt, dist.type="ecdf")
  expect_type(p1, "list")
})
