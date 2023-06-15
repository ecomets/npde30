# Changed call
## need to compute quantiles before and also pass pi.size=plot.opt$pi.size,approx.pi=plot.opt$approx.pi
## Changed return: only returns one list containing the PI for the quantiles passed in quantiles

aux.npdeplot.computepi<-function(obsmat, quantiles=c(0.025, 0.5, 0.975), pi.size=0.95,
    distrib="norm", approx.pi=TRUE, sim.ypl=NULL) {

  # Compute prediction intervals for the observed data, the size of which depends on the number of observations in each bin
  # Input
  ## obsmat: matrix of values used to compute the PI, with columns
  ### grp: the bin for each observation
  ### cens (if approx.pi=FALSE): indicating whether data is censored
  ## quantiles: the quantiles for which PI are to be obtained ()
  ## pi.size: size of the PI (plot.opt$pi.size)
  ## distrib: reference distribution
  ## approx.pi: if TRUE, simulations from distrib will be used to compute the PI; if FALSE, and sim.ypl present, computes the PI using the simulated data
  ## if approx.pi=FALSE, must provide sim.ypl
  ### vector of replicated npde/npd (ie npde/npd computed on the replicates, in the same way as obsmat contains npde/pd on the original dataset) or yobs for VPC
  # Output
  ## bnds: boundaries of the prediction intervals for each bin present in obsmat [may be < plot.opt$bin.number if all bins don't appear in obsmat]
  xinf<-sqrt(12)
  nseuil<-200
  alp.pi<-pi.size
  if(alp.pi<0.5) alp.pi<-(1-alp.pi)
  if(!approx.pi & !is.null(sim.ypl)) {
    nrep<-length(sim.ypl)/dim(obsmat)[1]
    if(is.na(match("cens",colnames(obsmat)))) obsmat$cens<-0
    ypl<-matrix(sim.ypl,ncol=nrep)
    ypl<-ypl[order(obsmat$grp),]
    obsmat<-obsmat[order(obsmat$grp),]
    yprov<-list(grp=obsmat$grp,cens=obsmat$cens,ypl=ypl)
    bnds<-compute.bands.true(yprov,quantiles,alpha=alp.pi)
    for(i in 1:3)
      bnds[[i]]<-cbind(grp=sort(unique(obsmat$grp)),bnds[[i]])
  } else {
    nobs<-tapply(obsmat$grp,obsmat$grp,length)
    bnds<-compute.bands(nobs,nseuil,quantiles,distrib,alp.pi)
    for(i in 1:3)
      bnds[[i]]<-cbind(grp=sort(unique(obsmat$grp)),bnds[[i]])
  }
  return(bnds)
}

## Removed xlab, now using grp as a match
### => does not return xlab anymore

aux.npdeplot.meanprof<-function(mbin,msim) {
  # Compute a reference profile based on simulations from the model
  # mbin : matrix with columns xcent (centre of the bins), grp (bin number/group)
  # msim : matrix with simulations, 2 columns used (grp=which bin, ysim=simulated value)
  ymed<-tapply(msim[,"ysim"],msim[,"grp"],mean)
  sdmed<-tapply(msim[,"ysim"],msim[,"grp"],sd)
  ymed<-ymed[match(mbin$grp,names(ymed),nomatch=0)]
  sdmed<-sdmed[match(mbin$grp,names(sdmed),nomatch=0)]
  #	ymed<-ymed[order(names(ymed))]
  #	sdmed<-sdmed[order(names(sdmed))]
  if(length(ymed)<dim(mbin)[1]) {
    # Linear interpolation
    #				ypmed<-approx(xcent,ymed,xout=mbin$xcent,rule=2)$y
    # Spline interpolation
    message("Not all time points/bins are represented in the subset used for the reference profile: spline interpolation will be used to predict the entire profile, but this may distort the aspect of the plot significantly; we advise using another reference profile.\n")
    xcent<-mbin$xcent[mbin$grp %in% names(ymed)]
    if ( length(xcent)!=0){
      ypmed<-spline(xcent,ymed,xout=mbin$xcent)$y
      spmed<-spline(xcent,sdmed,xout=mbin$xcent)$y
      iint<-1-as.integer(mbin$grp %in% names(ymed))
      mpref<-data.frame(xcent=mbin$xcent,grp=mbin$grp,mean=ypmed,sd=spmed,int=iint)
    } else {
      stop("Could not interpolate with the profile given")
    } # TODO check what happens
  } else mpref<-data.frame(xcent=mbin$xcent,grp=mbin$grp,mean=ymed,sd=sdmed,int=rep(0,length(sdmed)))
  mpref$lower<-mpref$mean-qnorm(0.975)*mpref$sd
  mpref$upper<-mpref$mean+qnorm(0.975)*mpref$sd
  ## alternative/non-parametric option: compute lower and upper from the quantiles (before interpolation)
  return(mpref)
}

aux.npdeplot.pimat <-function(obsmat, xcent, quantiles=c(0.025, 0.5, 0.975), pi.size=0.95, distrib="norm", approx.pi=TRUE, sim.ypl=NULL) {
  # Creates pimat
  # input:
  ## obsmat: matrix of observed values used to compute the empirical percentiles
  ### needs columns grp (bin for the observation), y (observation, can be yobs, npde, npd, pd)
  ## xcent: the values of the centers of the bins on the X axis [NB: could create a default to sort(unique(obsmat$x)) but tricky and would require another column x to be mandatory]
  ## quantiles: the percentiles of interest in the dataset  (computed using plot.opt$vpc.interval))
  ## pi.size: the size of the PI computed for the percentiles of interest
  ## distrib: if approx.pi=TRUE, the distribution used to compute the PI (see aux.npdeplot.computepi)
  ## sim.ypl: if approx.pi=FALSE, the data used to compute the PI (see aux.npdeplot.computepi)
  #
  # output: a dataframe containing the PI (boundaries of size and median value) with columns:
  ## grp: unique bins in obsmat
  ## xcent: xat (center of the bins)
  ## 3 sets of columns for the three PI: pinf, pmid, psup (eg 0.025, 0.5 and 0.975 for 95% PI, controlled by argument quantiles
  ## for each PI, the boundaries of size pi.size and the median of the PI, denoted by: lower, median, upper
  ### resulting in columns "pinf.lower","pmid.lower","psup.lower","pinf.median", "pmid.median", "psup.median","pinf.upper","pmid.upper","psup.upper"
  ## empirical percentiles computed for the three PI on the observations: "obs.inf","obs.median","obs.sup"

  bnds<-aux.npdeplot.computepi(obsmat, quantiles=quantiles, pi.size=pi.size, distrib=distrib, approx.pi=approx.pi, sim.ypl=sim.ypl)
  pimat<-data.frame(grp=unlist(bnds$binf[,"grp"]), xcent=xcent[unlist(bnds$binf[,"grp"])])
  pimat$category<-obsmat$category[1]
  for(i in c(1,3,2))
    pimat<-cbind(pimat, bnds[[i]][,2:4, drop=FALSE])
  pimat<-cbind(pimat,matrix(unlist(tapply(obsmat$y,obsmat$grp,quantile,quantiles)), ncol=3, byrow=TRUE))
  colnames(pimat)[4:15]<-c("pinf.lower","pmid.lower","psup.lower","pinf.median", "pmid.median", "psup.median","pinf.upper","pmid.upper","psup.upper","obs.inf","obs.median","obs.sup")
  return(pimat)
}

aux.npdeplot.transformPI <-function(pimat, mpref, distrib="norm") {
  # Transform prediction intervals in pimat using the reference profile in mpref
  ## pimat returned by a call to aux.npdeplot.pimat (see function)
  ## mpref returned by a call to aux.npdeplot.meanprof (see function)
  ## distrib: nature of the distribution of y
  ### if distrib is normal, tnpde = E(f()) + SD(f())*npde
  ### if distrib is uniform, tnpd = XXX TODO
  # Returns modified pimat
  for(icol in 4:15) {
    if(distrib=="norm")
      pimat[,icol]<-mpref$mean[pimat$grp] + mpref$sd[pimat$grp]*pimat[,icol] else message("Not implemented for pd - TODO\n")
  }
  return(pimat)
}

aux.npdeplot.transformObs <-function(obsmat, mpref, distrib="norm") {
  # Transform y in obsmat using the reference profile in mpref
  ## both dataframes should have a grp column indicating which bin they are in
  ## matches grp in obsmat to grp in mpref
  # Returns modified obsmat
  if(distrib=="norm")
    obsmat$y<-mpref$mean[obsmat$grp] + mpref$sd[obsmat$grp]*obsmat$y else message("Not implemented for pd - TODO\n")
  return(obsmat)
}
