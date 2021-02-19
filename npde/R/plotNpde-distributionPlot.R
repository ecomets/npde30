#################    	     Distributions - npde/pd          ###################

#' Distribution plots of pd/npde
#'
#' Produces a plot of the cdistribution of a metric compared to their theoretical distribution. Three types of distribution plots are available:
#' a histogram, a QQ-plot, or the empirical cdf.
#'
#' @usage npde.plot.dist(npdeObject, which="npd", dist.type="qqplot", ...)
#'
#' @aliases aux.npdeplot.hist aux.npdeplot.dist
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which a string determining which metric to plot (one of "npde", "pd" or "npd"), defaults to "npd"
#' @param dist.type string, one of "ecdf" (empirical cumulative density function), "hist" (histogram) or "qqplot" (QQ-plot of the empirical distribution versus the theoretical quantiles) to determine which type of plot (default is "qqplot")
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.  Mentre.
#' Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide.
#' \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @keywords plot
#' @export
#' @importFrom stats pnorm

npde.plot.dist<-function(npdeObject, which="npd", dist.type="qqplot", ...) {

  userPlotOptions = list(...)
  plot.opt <- set.plotoptions.default( npdeObject )
  plot.opt <- modifyList( plot.opt, userPlotOptions[ intersect( names( userPlotOptions ), names( plot.opt ) ) ] )
  # cat("In npde.plot.dist\n")
  # print(plot.opt$covsplit)
  # print(plot.opt$which.cov)

  # size replace size.pobs
  if ( plot.opt$size %in% userPlotOptions) plot.opt$size.pobs = plot.opt$size

  # col replace  col.pobs
  if ( plot.opt$col %in% userPlotOptions)    plot.opt$col.pobs = plot.opt$col

  # which modified or not for "npde","pd","npd"
  # which = plot.opt$which # which is passed in the arguments !!! don't change it here !!!

  # -----------------------------------------------------------
  # Check inputs

  # args1<-match.call(expand.dots=TRUE)
  # i1<-match("main",names(args1))
  #
  # if(!is.na(i1)) {
  #   change.main<-TRUE
  # } else change.main<-FALSE
  #
  # i1<-match("ncat",names(args1))
  # if(!is.na(i1)) {
  #   change.ncat<-TRUE
  # } else change.ncat<-FALSE

  if(match(which,c("npde","pd","npd"),nomatch=0)==0) {
    cat("Option which=",which,"not recognised\n")
    return()
  }
  if(match(dist.type,c("ecdf","qqplot","hist"),nomatch=0)==0) {
    cat("Option dist.type=",dist.type,"not recognised\n")
    return()
  }
  if(which %in% c("npde","pde") & length(npdeObject["results"]["res"]$npde)==0)  {
    cat("    Missing npde object to plot.\n")
    return()
  }
  if(which %in% c("pd","npd") & length(npdeObject["results"]["res"]$pd)==0) {
    cat("    Missing pd object to plot.\n")
    return()
  }

  # -----------------------------------------------------------
  # Set inputs

  if(which %in% c("pd","pde")) distrib<-"unif" else distrib<-"norm"
  if(length(npdeObject["data"]["icens"])>0) has.cens<-TRUE else has.cens<-FALSE

  covsplit<-plot.opt$covsplit
  if(covsplit) {
    if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov] # convert to names of covariates
    if(plot.opt$which.cov=="all" | plot.opt$which.cov=="") plot.opt$which.cov<-npdeObject["data"]["name.covariates"]
  }

  # covariates in the npdeObject => hasCovariates TRUE / FALSE
  if (length(npdeObject["data"]["name.covariates"])>0) hasCovariates = TRUE else hasCovariates = FALSE

  # if(covsplit==FALSE & plot.opt$plot.default==TRUE) {
  #   covsplit<-FALSE
  # } else if(covsplit==TRUE & length(npdeObject["data"]["name.covariates"])==0) {
  #   print("No covariates in the dataset\n")
  #   covsplit<-FALSE
  # }   else if(covsplit==FALSE &  (length(dots.plot.opt)!=2) &length(npdeObject["data"]["name.covariates"])!=0) {
  #   covsplit<-TRUE
  # }
  # # case for plot without optio,s, only type.plot
  # else if( (length(dots.plot.opt)==2) & (covsplit==FALSE) & (length(npdeObject["data"]["name.covariates"])!=0)) {
  #   covsplit==FALSE
  # }

  sim.ypl<-NULL
  if(!plot.opt$approx.pi) {
    if(which %in% c("pd","npd")) {
      if(length(npdeObject["results"]["pd.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated pd are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else {
        sim.ypl<-npdeObject["results"]["pd.sim"]
        if(which=="npd") sim.ypl<-qnorm(sim.ypl)
      }
    }
    if(which=="npde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-npdeObject["results"]["npde.sim"]
    }
    if(which=="pde") {
      if(length(npdeObject["results"]["npde.sim"])==0) {
        cat("You have requested to use simulations to provide the prediction intervals, but the simulated npde are not present.\n")
        plot.opt$approx.pi<-TRUE
      } else sim.ypl<-pnorm(npdeObject["results"]["npde.sim"])
    }
  }
  list_plot = list()   # list to stack the ggplot (not needed ?)

  # -----------------------------------------------------------
  # Prepare observations

  if(which %in% c("npde","pde")) ypl<-npdeObject["results"]["res"]$npde
  if(which %in% c("pd")) ypl<-npdeObject["results"]["res"]$pd
  if(which %in% c("npd")) ypl<-npdeObject["results"]["res"]$npd
  if(which=="pde") ypl<-pnorm(ypl) # we don't store pde, only npde
  # Define nsim
  if(length(sim.ypl)>0) nsim<-length(sim.ypl)/length(ypl)
    
  obsmat<-data.frame(x=ypl)
  obsmat$category<-"all"
  if(length(npdeObject["data"]["icens"])>0) obsmat$cens<-npdeObject["data"]["data"]$cens else obsmat$cens<-0
  not.miss = npdeObject["data"]["not.miss"] # not.miss : not missing data in the data TRUE/FALSE
  obsmat<-obsmat[not.miss,]
  if(length(sim.ypl)>0) sim.ypl<-sim.ypl[rep(not.miss, nsim)] # simulated data file also has MDV
  if(sum(is.na(obsmat$x))>0) {# missing data because of omit method
    not.miss2<-!(is.na(obsmat$x))
    obsmat<-obsmat[not.miss2,]
    if(!is.null(sim.ypl)) sim.ypl<-sim.ypl[rep(not.miss2, nsim)]
  }  else not.miss2<-NULL
  # -----------------------------------------------------------
  # Set options to pass

  if(dist.type=="hist") {
    plot.opt$alpha<-plot.opt$alpha/2
    if(plot.opt$xlab=="") plot.opt$xlab <- which
    if(plot.opt$ylab=="") plot.opt$ylab <- "Counts"
  }
  if(dist.type=="ecdf") {
    if(plot.opt$xlab=="") plot.opt$ylab <- which
    if(plot.opt$ylab=="") plot.opt$xlab <- "Empirical cumulative density function"
  }
  if(dist.type=="qqplot") {
    if(plot.opt$xlab=="") plot.opt$xlab <- paste("Theoretical",which)
    if(plot.opt$ylab=="") plot.opt$ylab <- paste("Empirical",which)
  }

  # -----------------------------------------------------------
  # Test and loop on covariates

  if(!covsplit) {

    if(dist.type=="hist") p<-aux.npdeplot.hist(obsmat, plot.opt, distrib=distrib, nclass=plot.opt$bin.number, sim.ypl=sim.ypl) else
      p<-aux.npdeplot.dist(obsmat, plot.opt, dist.type=dist.type, distrib=distrib, sim.ypl=sim.ypl)

    list_plot[[1]]<-p

  } else {
    # loop on covariate
    idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
    iplot <- 0
    for(icov in 1:length(plot.opt$which.cov)) {
      iplot <- iplot+1 # Counter for list of plots
      lcov <-  plot.opt$which.cov[icov]
      plot.opt2<-plot.opt
      plot.opt2$which.cov<-lcov
      zecov <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"],lcov]
      if(!is.null(not.miss2)) zecov<-zecov[not.miss2]
      ucov = zecov[match(unique(idobs),idobs)]
      if(is.numeric(ucov) & length(unique(ucov))>plot.opt$ncat){ # Continuous covariatewith more than plot.opt$ncat (default 3)
        if(plot.opt$ncat!=3) { # 3 categories or less
          ncat<-plot.opt$ncat
          seqcat<-seq(0,1,length.out=(ncat+1))
          zecov.cat<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
          nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
          namcat<-paste(lcov,nam1,sep=": ")
          zecov.cat<-factor(zecov.cat, labels=namcat)
        } else { # if more than 3 categories, split in 3 ranges
          zecov.cat<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
          namcat<-paste(lcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
          zecov.cat<-factor(zecov.cat, labels=namcat)
        }
      } else { # Categorical covariate defined as factor, or covariate with less than plot.opt$ncat categories
        namcat<-paste(lcov,sort(unique(ucov)), sep=": ")
        zecov.cat<-paste(lcov, zecov, sep=": ")
        zecov.cat<-factor(zecov.cat, labels=namcat)
      }
      obsmat$category<-zecov.cat
      if(dist.type=="hist") p<-aux.npdeplot.hist(obsmat, plot.opt2, distrib=distrib, nclass=plot.opt$bin.number, sim.ypl=sim.ypl) else
        p<-aux.npdeplot.dist(obsmat, plot.opt2, dist.type=dist.type, distrib=distrib, sim.ypl=sim.ypl)
      list_plot[[iplot]]<-p
    }
  }
  if(length(list_plot)==1) list_plot<-list_plot[[1]]
  return( list_plot )

  #invisible(list_plot) # return invisibly, can we return plots that we can manipulate later ?
} # END FUNCTION

###############################	   P(Y<LOQ)	 ########################################

#' Plot of the probability that the observations are below the LOQ
#'
#' Plots the probability that the observations are below the LOQ along with the model predicted interval
#'
#' @usage npde.plot.loq(npdeObject,xaxis="x",nsim=200,...)
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param xaxis a string character, one of "x" (to plot P(Y<LOQ) versus the value of the independent predictor) or "ypred" (versus the value of the population predictions). Defaults to "x"
#' @param nsim number of simulations to be used for the computation of the prediction interval
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export


npde.plot.loq<-function(npdeObject,xaxis="x",nsim=200,...) {
  # Plot of the probability of an observation being LOQ versus X or predictions, overlaying
  ### the predicted probability (according to the model)
  ### the observed probability
  ### a prediction band obtained using the simulated data

  # --------------------------------------------------------------------------
  # plot : loq
  # --------------------------------------------------------------------------

  args1<-match.call(expand.dots=TRUE)
  i1<-match("loq",names(args1))
  if(!is.na(i1)) {
    loq<-as.numeric(args1[[i1]])
  } else {
    if(length(npdeObject["data"]["loq"])==0) {
      ploq<-c()
      yobs<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
      if(length(npdeObject["data"]["loq"])>0) {
        loq<-npdeObject["data"]["loq"]
        if(npdeObject@options$verbose) cat("Computing p(y<LOQ) using LOQ=",loq,"\n")
      } else {
        yloq<-yobs[npdeObject["data"]["icens"]]
        if(length(unique(yloq))==1) {
          if(npdeObject@options$verbose) cat("Same LOQ for all missing data, loq=",loq,"\n")
          loq<-unique(yloq)
        } else {
          loq<-min(unique(yloq))
          if(npdeObject@options$verbose) cat("Computing p(y<LOQ) for the lowest LOQ, loq=",loq,"\n")
        }
        npdeObject["data"]["loq"]<-loq
      }
      if(is.infinite(npdeObject["data"]["loq"])) {
        if(npdeObject@options$verbose) cat("No loq defined in the data, and no censored data to define it, please call npde.plot.loq with the option loq=XXX where XXX is the value of the LOQ.\n")
        return()
      }
    } else loq<-npdeObject["data"]["loq"]
  }


  # censored data
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
  }else{
    has.cens<-FALSE
  }

  # plot option
  plot.opt<-npdeObject["prefs"]

  # nb of simulation
  nsim<-min(nsim,npdeObject["sim.data"]["nrep"])

  # Binning
  xvec<-switch(xaxis, x=npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]], pred=npdeObject["results"]["res"]$ypred, cov="Not implemented yet")

  if(!is.numeric(xvec)) {
    if(npdeObject@options$verbose) cat(xvec,"\n")
    return()
  }

  if(has.cens) {

    ydat<-npdeObject["data"]["data"][,npdeObject["data"]["name.cens"]]

  }else {

    ydat<-rep(0,length(xvec))

  }


  xbin<-npde.binning(xvec,plot.opt,verbose=plot.opt$interactive)
  xgrp<-xbin$xgrp
  xpl<-xbin$xcent # xbin$xat
  nbin<-length(unique(xgrp))
  isamp<-sample(1:npdeObject["sim.data"]["nrep"],nsim)
  ysim<-npdeObject["sim.data"]["datsim"]$ysim
  xtab<-matrix(nrow=nsim,ncol=nbin)

  for(i in 1:nsim)
    xtab[i,]<-tapply(ysim[npdeObject["sim.data"]["datsim"]$irsim==isamp[i]] < loq,xgrp,mean)

  alpha<-(1-plot.opt$vpc.interval)/2
  quant<-c(alpha,0.5,1-alpha)

  # dataframe for ggplot
  ypl<-apply(xtab,2,quantile,quant)
  xobs<-tapply(ydat,xgrp,mean)
  plotdata = data.frame(xpl,t(ypl),xobs)


  #if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0,max(c(xtab,xobs),na.rm=T))

  # --------------------------------------------------------------------------
  # plot : loq
  # --------------------------------------------------------------------------
  # plot options
  userPlotOptions = list(...)
  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, userPlotOptions[intersect(names(userPlotOptions), names(plot.opt))])

  # meta arguments to change col,lwd,lty,pch for lines and symbols
  if ( plot.opt$size %in% userPlotOptions)
  {
    plot.opt$size.pobs = plot.opt$size
    #plot.opt$size.lobs = plot.opt$size
  }

  if ( plot.opt$col %in% userPlotOptions)
  {
    plot.opt$col.pobs = plot.opt$col
    plot.opt$col.lobs = plot.opt$col
  }

  if ( plot.opt$lwd %in% userPlotOptions)
  {
    plot.opt$lwd.lobs = plot.opt$lwd
  }

  if ( plot.opt$pch %in% userPlotOptions)
  {
    plot.opt$pch.pobs <- plot.opt$pch
    plot.opt$pch.pcens = plot.opt$pch
  }

  if ( plot.opt$lty %in% userPlotOptions)
  {
    plot.opt$lty.lobs = plot.opt$lty
  }
  # -----------------------------------------------------------------------------------------------------------------


  #  xlim and ylim
  if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
    x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
  } else {
    x.limits = 1.01*c(0,max(plotdata$xpl,na.rm = TRUE))}

  y.limits = c(0,1)

  # labels x-y axis
  # dont past units if units are empty
  ncharXunits = nchar( gsub( "[[:space:]]", "", npdeObject@data@units$x ) )
  if ( ncharXunits == 0)
    plot.opt$xlab = npdeObject@data@name.predictor
  plot.opt$ylab = "Pr[Y<LOQ]"


  p <- ggplot(plotdata,aes(x = xpl)) +

    # title and axis labs
    theme(plot.title = element_text(hjust = 0.5,size = plot.opt$size.main),
          plot.subtitle = element_text(hjust = 0.5,size = plot.opt$size.sub),

          axis.title.x = element_text(size = plot.opt$size.xlab),
          axis.title.y = element_text(size = plot.opt$size.ylab),

          axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),

          axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),

          panel.background=element_rect("white"),
          panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")),
          panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white")))  +

    labs(title = plot.opt$main, subtitle = plot.opt$sub) +

    coord_cartesian(xlim=x.limits, ylim=y.limits) +

    geom_ribbon(aes(ymin = X2.5., ymax =  X97.5.) ,
                fill=plot.opt$fill.bands,
                alpha = plot.opt$alpha.bands,
                linetype=2) +

    geom_line(aes(y = X2.5.),
              color = plot.opt$col.bands,
              alpha = plot.opt$alpha.bands,
              size = plot.opt$lwd.bands,
              linetype = plot.opt$lty.bands) +

    geom_line(aes(y = X97.5.),
              color = plot.opt$col.bands,
              alpha = plot.opt$alpha.bands,
              size = plot.opt$lwd.bands,
              linetype = plot.opt$lty.bands) +

    geom_line(aes(y = X50.),
              color = plot.opt$col.ther,
              alpha = plot.opt$alpha.ther,
              size = plot.opt$lwd.ther,
              linetype = plot.opt$lty.ther) +

    geom_line(aes(y = xobs),
              color = plot.opt$col.lobs,
              size = plot.opt$lwd.lobs,
              linetype = plot.opt$lty.lobs) +

    scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x))+
    scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))

#  print(p)
  return(p)

 }
