# Folders - change to relative folder
workDir<-"/home/eco/work/npde/romain/romain2101"
notesDir<-file.path(workDir,"notebooks")
source(file.path(workDir,"kompareCode","computePI.R")) # Working files => reinsert in main code files TODO

# Datasets
dataRef<-readRDS(file.path(notesDir,"obsdat_ecopk.rds"))
matsim<-readRDS(file.path(notesDir,"simdat_ecopk.rds"))

# context("Check data")  # Context deprecated in testthat

test_that("Check consistency of dataframes", {
  expect_equal(dim(dataRef),c(1000,7))
  expect_equal(dim(matsim),c(200000,8))
  expect_equal(unique(dataRef$id),unique(matsim$id))
  expect_equal(unique(dataRef$time),unique(matsim$time))
}
)

cat("Creating a dataframe with unique times, sorted, and creating group from 1 to length(unique times)\n")
xtim<-unique(dataRef$time)
grmat<-data.frame(xat=sort(xtim), grp=1:length(xtim))
dataRef$grp<-grmat$grp[match(dataRef$time,grmat$xat)]
matsim$grp<-rep(dataRef$grp,nrep)

# context("Mean profile")

test_that("Mean profile for subject 1, no interpolation", {
  dataRef1<-dataRef[dataRef$id==1,]
  mbin<-data.frame(xat=sort(unique(dataRef1$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  msim<-matsim[matsim$id==1,]
  msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
  colnames(msim)[4]<-"ysim"
  mpref<-aux.npdeplot.meanprof(mbin,msim)
  expect_equal(sum(mpref$int),0)
  expect_equivalent(tapply(msim$ysim,msim$time,mean), unlist(mpref$mean))
  expect_equivalent(tapply(msim$ysim,msim$time,sd), unlist(mpref$sd))
}
)

test_that("Mean profile for subject 1, interpolation for all data points", {
  mbin<-data.frame(xat=sort(unique(dataRef$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  msim<-matsim[matsim$id==1,]
  msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
  colnames(msim)[4]<-"ysim"
  mpref<-aux.npdeplot.meanprof(mbin,msim)
  expect_equal(sum(mpref$int),3)
  q1<-tapply(msim$ysim,msim$grp,quantile,c(0.025,0.5,0.975))
  gr1<-unique(msim$grp)
  pltab<-NULL
  for(i in 1:length(q1))
    pltab<-rbind(pltab,
                 c(gr1[i], q1[[i]]))
  colnames(pltab)<-c("grp","lower","ymed","upper")
  pltab<-data.frame(pltab)
  pltab<-cbind(xat=mbin$xat[match(pltab$grp,mbin$grp)],pltab)
  mpref$ymin<-mpref$mean-1.96*mpref$sd
  mpref$ymax<-mpref$mean+1.96*mpref$sd
  p1 <- ggplot(pltab,aes(x=xat,y=ymed)) + geom_ribbon(aes(x=xat, ymin=lower, ymax=upper), fill="lightblue",alpha=0.5) + geom_line(colour="blue") + geom_line(data=mpref, aes(x=xat, y=mean),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymin),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymax),linetype="dashed", col="red", alpha=0.8)
  print(p1)
  cat("We should see close agreement between the ribbon and the lines from the reference profile. Check spline interpolation\n")
}
)

test_that("Mean profile for different doses", {
  mbin<-data.frame(xat=sort(unique(dataRef$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  mpref1<-pltab<-NULL
  for(idose in c(10,100, 1000)) {
    msim<-matsim[matsim$dose==idose,]
    msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
    colnames(msim)[4]<-"ysim"
    mpref<-aux.npdeplot.meanprof(mbin,msim)
    q1<-tapply(msim$ysim,msim$grp,quantile,c(0.025,0.5,0.975))
    gr1<-sort(unique(msim$grp))
    pltab1<-NULL
    for(i in 1:length(q1))
      pltab1<-rbind(pltab1,
                   c(gr1[i], q1[[i]]))
    colnames(pltab1)<-c("grp","lower","ymed","upper")
    pltab1<-data.frame(pltab1)
    pltab1<-pltab1[order(pltab1$grp),]
    pltab1<-cbind(xat=mbin$xat[match(pltab1$grp,mbin$grp)],pltab1, dose=idose)
    pltab<-rbind(pltab,pltab1)
    mpref$ymin<-mpref$mean-1.96*mpref$sd
    mpref$ymax<-mpref$mean+1.96*mpref$sd
    mpref$dose<-idose
    mpref1<-rbind(mpref1, mpref)
  }
  p1 <- ggplot(pltab,aes(x=xat,y=ymed)) + geom_ribbon(aes(x=xat, ymin=lower, ymax=upper), fill="lightblue",alpha=0.5) + geom_line(colour="blue") + geom_line(data=mpref1, aes(x=xat, y=mean),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref1, aes(x=xat, y=ymin),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref1, aes(x=xat, y=ymax),linetype="dashed", col="red", alpha=0.8) + facet_wrap(.~dose,ncol=3,scales="free")
  print(p1)
  cat("Reference profiles for 3 doses (different scales on the Y-awxis.\n")
}
)

test_that("Mean profile for women", {
  mbin<-data.frame(xat=sort(unique(dataRef$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  msim<-matsim[matsim$sex==1,]
  msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
  colnames(msim)[4]<-"ysim"
  mpref<-aux.npdeplot.meanprof(mbin,msim)
  q1<-tapply(msim$ysim,msim$grp,quantile,c(0.025,0.5,0.975))
  gr1<-sort(unique(msim$grp))
  pltab<-NULL
  for(i in 1:length(q1))
    pltab<-rbind(pltab,
                 c(gr1[i], q1[[i]]))
  colnames(pltab)<-c("grp","lower","ymed","upper")
  pltab<-data.frame(pltab)
  pltab<-pltab[order(pltab$grp),]
  pltab<-cbind(xat=mbin$xat[match(pltab$grp,mbin$grp)],pltab)
  mpref$ymin<-mpref$mean-1.96*mpref$sd
  mpref$ymax<-mpref$mean+1.96*mpref$sd
  p1 <- ggplot(pltab,aes(x=xat,y=ymed)) + geom_ribbon(aes(x=xat, ymin=lower, ymax=upper), fill="lightblue",alpha=0.5) + geom_line(colour="blue") + geom_line(data=mpref, aes(x=xat, y=mean),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymin),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymax),linetype="dashed", col="red", alpha=0.8)
  print(p1)
  cat("Very different doses so the reference profile should be much too large. Check spline interpolation\n")
}
)

test_that("Mean profile for women with dose=100", {
  mbin<-data.frame(xat=sort(unique(dataRef$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  msim<-matsim[matsim$sex==1 & matsim$dose==100,]
  msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
  colnames(msim)[4]<-"ysim"
  mpref<-aux.npdeplot.meanprof(mbin,msim)
  q1<-tapply(msim$ysim,msim$grp,quantile,c(0.025,0.5,0.975))
  gr1<-sort(unique(msim$grp))
  pltab<-NULL
  for(i in 1:length(q1))
    pltab<-rbind(pltab,
                 c(gr1[i], q1[[i]]))
  colnames(pltab)<-c("grp","lower","ymed","upper")
  pltab<-data.frame(pltab)
  pltab<-pltab[order(pltab$grp),]
  pltab<-cbind(xat=mbin$xat[match(pltab$grp,mbin$grp)],pltab)
  mpref$ymin<-mpref$mean-1.96*mpref$sd
  mpref$ymax<-mpref$mean+1.96*mpref$sd
  p1 <- ggplot(pltab,aes(x=xat,y=ymed)) + geom_ribbon(aes(x=xat, ymin=lower, ymax=upper), fill="lightblue",alpha=0.5) + geom_line(colour="blue") + geom_line(data=mpref, aes(x=xat, y=mean),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymin),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymax),linetype="dashed", col="red", alpha=0.8)
  print(p1)
  cat("Restricting to dose=100 works much better \n")
}
)

test_that("Mean profile for age between 40 and 55", {
  mbin<-data.frame(xat=sort(unique(dataRef$time)))
  mbin$grp<-grmat$grp[match(mbin$xat,grmat$xat)]
  msim<-matsim[matsim$age >= 40 & matsim$age <= 55,]
  msim$grp<-grmat$grp[match(msim$time,grmat$xat)]
  colnames(msim)[4]<-"ysim"
  mpref<-aux.npdeplot.meanprof(mbin,msim)
  q1<-tapply(msim$ysim,msim$grp,quantile,c(0.025,0.5,0.975))
  gr1<-sort(unique(msim$grp))
  pltab<-NULL
  for(i in 1:length(q1))
    pltab<-rbind(pltab,
                 c(gr1[i], q1[[i]]))
  colnames(pltab)<-c("grp","lower","ymed","upper")
  pltab<-data.frame(pltab)
  pltab<-pltab[order(pltab$grp),]
  pltab<-cbind(xat=mbin$xat[match(pltab$grp,mbin$grp)],pltab)
  mpref$ymin<-mpref$mean-1.96*mpref$sd
  mpref$ymax<-mpref$mean+1.96*mpref$sd
  p1 <- ggplot(pltab,aes(x=xat,y=ymed)) + geom_ribbon(aes(x=xat, ymin=lower, ymax=upper), fill="lightblue",alpha=0.5) + geom_line(colour="blue") + geom_line(data=mpref, aes(x=xat, y=mean),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymin),linetype="dashed", col="red", alpha=0.8) + geom_line(data=mpref, aes(x=xat, y=ymax),linetype="dashed", col="red", alpha=0.8)
  print(p1)
  cat("Peak at time=3 because only dose=1000 for this time so mean much higher. Again poor reference profile in this case as too heterogenous Check spline interpolation\n")
}
)
