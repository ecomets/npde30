####################################################################################
######################	Computing boundaries for PI ################################

# Compute PI bands using an approximation (simulations)
compute.bands<-function(nsamp,nseuil=200,quant=c(0.025,0.5,0.975),distrib="norm", alpha=0.95) {
  # Compute a prediction interval around selected quantiles of the normal or uniform distribution, for different sizes of samples by randomly sampling from N(0,1) or U(0,1)
  ### nsamp: a vector giving the sizes of the samples
  ### size: alpha (defaults to a 95% PI)
  ### quantile: quant (defaults to 5th, 50th (median) and 95th percentiles)
  ### distribution: normal (default) or uniform
  # When the number of samples isamp is larger than 200, the PI is computed for n=200 and the size of the PI is then adjusted through sqrt(200/isamp)

  # Returns
  ### binf: lower bounds (as many columns as elements in quant) for the PI with level alpha
  ### bmed: median
  ### bsup: upper bounds
  #  msim<-10000
  msim<-1000 # number of replications used to compute the prediction interval
  idx1<-which(nsamp>=nseuil)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2)

  if(length(idx1)>0) {
    xsamp<-matrix(switch(distrib,norm=rnorm(msim*nseuil),unif=runif(msim*nseuil)), ncol=msim)
    mat<-apply(xsamp,2,quantile,quant)
    xseuil<-apply(mat,1,quantile,quant.pi)
    demi<-(xseuil[3,]-xseuil[1,])/2
  }

  binf<-bsup<-bmed<-matrix(nrow=length(nsamp),ncol=length(quant), dimnames=list(nsamp,quant))

  for(isamp in unique(nsamp)) {
    if(isamp<nseuil) {
        mean=0
        sd=1
      if(isamp>1) {
        xsamp<-matrix(switch(distrib,norm=rnorm(msim*isamp,mean, sd),unif=runif(msim*isamp)), ncol=msim)
        mat<-apply(xsamp,2,quantile,quant)
        xtab<-apply(mat,1,quantile,quant.pi)
      } else xtab<-matrix(data=NA, nrow=length(quant.pi), ncol=length(quant)) # if isamp=1, we can't compute PI

    } else {
      xtab<-matrix(nrow=3,ncol=length(quant))
      xtab[2,]<-switch(distrib,norm=qnorm(quant),unif=quant)
      xtab[1,]<-xtab[2,]-demi*sqrt(nseuil/isamp)
      xtab[3,]<-xtab[2,]+demi*sqrt(nseuil/isamp)
    }
    for(i in which(nsamp==isamp)) {
      binf[i,]<-xtab[1,]
      bmed[i,]<-xtab[2,]
      bsup[i,]<-xtab[3,]
    }
  }
  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

compute.bands.true<-function(sim.ypl,quant=c(0.025,0.5,0.975), alpha=0.95) {

  # Compute a prediction interval around selected quantiles of the data sim.ypl over the values given by the grouping factor grp
  ## columns in sim.ypl: ypl (data), grp (grouping factor)
  quant.pi<-c((1-alpha)/2,0.5,1-(1-alpha)/2)
  ugrp<-unique(sim.ypl$grp)
  binf<-bsup<-bmed<-matrix(nrow=length(ugrp),ncol=length(quant), dimnames=list(ugrp,quant),0.0)

  for(igrp in ugrp) {
    ind_grp = which(sim.ypl$grp==igrp)
    mat <- apply(sim.ypl$ypl[ind_grp,,drop=FALSE],2,quantile,quant)
    xtab<-apply(mat,1,quantile,quant.pi)
    if(dim(sim.ypl$ypl[ind_grp,,drop=FALSE])[1]==1)
      xtab<-matrix(data=NA, nrow=length(quant.pi), ncol=length(quant)) # if isamp=1, we can't compute PI

    ugrp <- ugrp[!is.na(ugrp)]
    binf[ugrp==igrp,]<-xtab[1,]
    bmed[ugrp==igrp,]<-xtab[2,]
    bsup[ugrp==igrp,]<-xtab[3,]
  }

  return(list(binf=binf,bsup=bsup,bmed=bmed))
}

####################################################################################
######################	Binning to produce PI ################################

#' Internal functions used to produce prediction intervals
#'
#' Functions used by plot functions to define the boundaries of the bins on the X-axis
#'
#' These functions are normally not called by the end-user but are now exported for use in the saemix package.
#'
#' @param xvec a vector of values for the X-axis of the plot
#' @param plot.opt graphical options
#' @param verbose boolean (defaults to FALSE). If TRUE, a table showing how the binning was performed
#'
#' @return a list with 3 elements, xgrp (the number of the bin associated with each element of xvec), xcent (a named vector containing the mean of the elements of xvec contained in each bin; the name of the bin is the interval), and xgroup (a vector with the group associated to each element of xvec after binning)
#' If verbose is TRUE, a table showing the bins is shown, giving the interval of xvec associated with each bin, the mean value
#' of xvec in each bin, and the number of observations
#'
#' @name npde.binning
## #' @aliases
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#'
#' @importFrom mclust Mclust
#' @export

# Binning the X data
npde.binning<-function(xvec,plot.opt,verbose=FALSE) {
  # plot.opt should contain the following items
  # bin.method: binning method (one of "optimal","width","user","equal")
  # bin.number: number of bins (default: 10, maximum: 20)
  # bin.breaks
  # bin.extreme
  # xlog

  nbin<-plot.opt$bin.number
  bnds <- plot.opt$bin.breaks

  xvec1<-xvec
  xvec<-xvec[!is.na(xvec)]
  if(is.na(pmatch(plot.opt$bin.method,c("optimal","width","user","equal")))) {
    if(verbose) cat("Binning method",plot.opt$bin.method,"not found, reverting to equal binning\n")
    plot.opt$bin.method<-"equal"
  }

  if(!is.na(pmatch(plot.opt$bin.method,"optimal"))) {
    if(!("mclust"%in%.packages(all.available = TRUE))) {
      if(verbose) cat("mclust library not installed, reverting to equal binning\n")
      plot.opt$bin.method<-"equal"
    } else {
      #			require(mclust)
      if(is.null(plot.opt$bin.number) || is.na(plot.opt$bin.number)) plot.opt$bin.number<-10
    }
  }

  if(!is.na(pmatch(plot.opt$bin.method,"user")) & is.null(plot.opt$bin.breaks)) {
    if(verbose) cat("User-defined method specified, but bin.breaks is empty; reverting to equal binning\n")
    plot.opt$bin.method<-"equal"
  }

  if(!is.na(pmatch(plot.opt$bin.method,c("equal","width"))) & is.null(plot.opt$bin.number)) {
    nbin<-length(unique(xvec))
    if(nbin>20) nbin<-20
    plot.opt$bin.number<-nbin
  }

  if(is.na(pmatch(plot.opt$bin.method,c("optimal","user"))) && length(unique(xvec))<=nbin) {
    xgrp<-match(xvec,sort(unique(xvec)))
    xpl<-tapply(xvec,xgrp,mean)
  } else {

    if(!is.na(pmatch(plot.opt$bin.method,"user"))) {
      bnds<-plot.opt$bin.breaks
      if(min(bnds)>=min(xvec)) bnds<-c(min(xvec)*(1-sign(min(xvec))*0.001),bnds)
      if(max(bnds)<max(xvec)) bnds<-c(bnds,max(xvec))
    }

    if(!is.na(pmatch(plot.opt$bin.method,"equal"))) {
      xvec2<-xvec;xvec2[xvec2==min(xvec)]<-min(xvec)-1
      if(!is.null(plot.opt$bin.extreme) & length(plot.opt$bin.extreme)==2) {
        xq<-plot.opt$bin.extreme
        xquant<-c(0,seq(xq[1],xq[2],length.out=(nbin-1)),1)
      } else xquant<-(0:nbin)/nbin
      bnds<-unique(quantile(xvec2,xquant,type=8))
    }

    if(!is.na(pmatch(plot.opt$bin.method,"width"))) {
      if(plot.opt$xlog) xvec2<-log(xvec[xvec>0]) else xvec2<-xvec
      if(!is.null(plot.opt$bin.extreme) & length(plot.opt$bin.extreme)==2) {
        xq<-plot.opt$bin.extreme
        xq1<-quantile(xvec2,xq,type=8)
        bnds<-c(min(xvec2),seq(xq1[1],xq1[2],length.out=(nbin-1)),max(xvec2))
        bnds<-sort(unique(bnds))
      } else bnds<-seq(min(xvec2),max(xvec2),length.out=(nbin+1))
      if(plot.opt$xlog) {
        bnds<-exp(bnds)
        bnds[length(bnds)]<-bnds[length(bnds)]*(1+sign(bnds[length(bnds)])*0.001)
        if(sum(xvec<=0)>0) bnds<-c(min(xvec),bnds)
      }
      bnds[1]<-bnds[1]*(1-sign(bnds[1])*0.001)
    }

    if(!is.na(pmatch(plot.opt$bin.method,"optimal"))) {
      yfit<-mclust::Mclust(xvec,G=((nbin-5):(nbin+5)))
      xgrp<-yfit$classification
      xpl<-yfit$parameters$mean
      xpl<-xpl[match(names(table(xgrp)),names(xpl))]
      minx<-tapply(xvec,xgrp,min)
      maxx<-tapply(xvec,xgrp,max)
      bnds <- c(minx[1],(minx[-c(1)]+maxx[-length(maxx)])/2,maxx[length(maxx)])
      names(xpl)<-paste("[",tapply(xvec,xgrp,min),"-",tapply(xvec,xgrp,max),"]", sep="")
    } else {
      xgrp<-factor(cut(xvec,bnds,include.lowest=F))
      xpl<-tapply(xvec,xgrp,mean)
    }
  }

  nbin<-length(unique(xgrp))
  npl<-tapply(xvec,xgrp,length)
  tab<-cbind(Interval=names(xpl),Centered.On=format(xpl,digits=2),Nb.obs=npl)
  row.names(tab)<-1:dim(tab)[1]

  if(verbose) {
    xnam<-switch(EXPR=plot.opt$bin.method,equal="by quantiles on X", width="equal sized intervals",user="user-defined bins",optimal="clustering algorithm")
    cat("Method used for binning:",xnam,", dividing into the following",nbin,"intervals\n")
    print(tab,quote=F)
  }

  xgrp2<-rep(NA,length(xvec1))
  xgrp2[!is.na(xvec1)]<-xgrp

  # ------------------------------------------------------------------------------------------------
  # Take the min and max values of xpl to extend prediction band to min/max X
  # start to 0 instead of the minimal value of bnds (can guves negative values for Time in x-axis)
  # is.null test for non numerical covariate

  xpl.extended = xpl

  if ( !is.null(bnds)) #
  {
    xpl.extended[1] = min(bnds)
    xpl.extended[nbin] = max(bnds)
  }

  xpl = xpl.extended
  # ------------------------------------------------------------------------------------------------

  return(list(xgrp=xgrp2,xcent=xpl,xbound=bnds))

}
