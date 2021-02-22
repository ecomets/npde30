#' Covariate diagnostic plots
#'
#' Boxplot of the selected variable versus categories of covariates
#'
#' @usage npde.plot.covariate(npdeObject, which.y="npd", ...)
#'
#' @aliases aux.npdeplot.boxcov
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which.y a string specifying the variable on the Y-axis (one of "yobs", "npde", "pd", "npd")
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}} and \code{\link{npdeControl}})
#'
#' @return a ggplot object or a list of ggplot objects (grobs)
#' @details For a categorical covariate, boxplots are produced for each category. Continous covariates are split into quantile (by default, first quartile (<Q1), interquartile range (Q1-Q3) and upper quartile (>Q3), but the number of categories can be set by using the ncat argument).
#' @details For each category, the median according to simulations under the model is shown (it can be suppressed by using the argument bands=FALSE)..
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}, \code{\link{npdeControl}}
#' @references K. Brendel, E. Comets, C. Laffont, F. Mentre F. Evaluation of different tests based on observations for external model evaluation of population analyses. \emph{Journal of Pharmacokinetics and Pharmacodynamics}, 37:49-65, 2010.
#' @keywords plot
#' @export
#'

npde.plot.covariate<-function(npdeObject, which.y="npd", ...){ #} xscale=FALSE, onlog=FALSE, ref.prof=NULL, ...) {
  # npdeObject: object returned from a npde run
  # which.y: variable on the Y-axis, one of "npde", "npd", "pd", "yobs" (VPC)  + (? "pde", "cov" ?)

  #plot.opt = npdeObject["prefs"]
  userPlotOptions  = list(...)
  plot.opt <- set.plotoptions.default( npdeObject )
  plot.opt <- modifyList( plot.opt, userPlotOptions[ intersect( names( userPlotOptions ), names( plot.opt ) ) ] )
  
  # -----------------------------------------------------------------------------------
  # Check inputs
  
  if(match(which.y,c("npde","tnpde","pd","npd","yobs"),nomatch=0)==0) {
    if(npdeObject@options$verbose) message(paste("Option which.y=",which.y,"not recognised"))
    return("Option which not recognised")
  }
  if(which.y=="npde") {
    if(is.na(match("npde",colnames(npdeObject@results@res)))) return("npde not computed")
  }
  if(which.y=="npd") {
    if(is.na(match("npd",colnames(npdeObject@results@res)))) {
      if(!is.na(match("pd",colnames(npdeObject@results@res)))) npdeObject@results@res$npd<-qnorm(npdeObject@results@res$pd)
    } else return("pd not computed")
  }
  if(which.y=="pd") {
    if(is.na(match("pd",colnames(npdeObject@results@res)))) return("pd not computed")
  }
  if(length(plot.opt$which.cov)==1) {
    if(plot.opt$which.cov=="all" | plot.opt$which.cov=="") plot.opt$which.cov<-npdeObject["data"]["name.covariates"]
  }
  idx.cov = match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
  if (length(idx.cov)==0) {
    if(npdeObject@options$verbose) message("Error: plot over covariates required but no matching covariate found in dataset")
    return("No matching covariate")
  }
  
  # -----------------------------------------------------------------------------------
  # Setup
  list_plot = list()   # list to stack the ggplot
  
  if(which.y %in% c("pd","pde")) distrib<-"unif" else distrib<-"norm"

  if(plot.opt$ylab=="") {
    plot.opt$ylab <- switch(which.y, "npde"="npde", "npd"="npd", "pd"="pd", "yobs"=paste0( npdeObject@data@name.response),  "cov"="") # cov not a valid option (yet ?)
    if (which.y=="yobs" & npdeObject@data@units$y != "") plot.opt$ylab<-paste0(plot.opt$ylab, "(", npdeObject@data@units$y,")" )
  }
  
  # vpc.interval controls which percentiles we want PI for
  alpha <- (1 - plot.opt$vpc.interval) / 2 
  if(alpha>0.5) alpha<-(1-alpha)
  nrep<-npdeObject["sim.data"]["nrep"]
  
  # -----------------------------------------------------------------------------------
  # Creating obsmat with the first covariate and then cycle over covariates later
  
  obsmat <- data.frame(x=npdeObject@data@data[,npdeObject@data@name.covariates[idx.cov[1]]] )
  obsmat$y <- switch(which.y, "npde"=npdeObject@results@res$npde, "npd"=npdeObject@results@res$npd, "pd"=npdeObject@results@res$pd,
                     "yobs"=npdeObject@results@res$ycomp)
  if(length(npdeObject@data@icens)==0) obsmat$cens<-0 else obsmat$cens<-npdeObject@data@data$cens
  
  if(length(npdeObject@data@loq)>0) obsmat$loq <-npdeObject@data@loq
  obsmat$category<-'none'
  # test if censored data
  if (length(npdeObject["data"]["icens"])>0) has.cens = TRUE else has.cens = FALSE
  not.miss = npdeObject["data"]["not.miss"] # not.miss : not missing data in the data TRUE/FALSE
  obsmat<-obsmat[not.miss,]
  if(sum(is.na(obsmat$y))>0) {# missing data because of omit method
    not.miss2<-!(is.na(obsmat$y))
    obsmat<-obsmat[not.miss2,]
  }  else not.miss2<-NULL
  
  # -----------------------------------------------------------------------------------
  # Simulated data (if approx.pi=FALSE)
  sim.ypl<-NULL
  if(which.y=="yobs") plot.opt$approx.pi <- FALSE
  if(plot.opt$approx.pi==FALSE) {
    sim.ypl<-switch(which.y, "pd"=npdeObject["sim.data"]["datsim"]$pdsim,
                    "npd"=npdeObject["sim.data"]["datsim"]$npdsim,
                    "npde"=npdeObject["sim.data"]["datsim"]$npdesim,
                    "yobs"=npdeObject["sim.data"]["datsim"]$ysim)
    if(length(sim.ypl)==0) {
      if(which.y!="yobs") plot.opt$approx.pi<-TRUE else return()
      if(npdeObject@options$verbose) cat("No simulated values for",which.y," found in data, switching to approximate PI.")
    } else {
      sim.ypl<-sim.ypl[rep(not.miss, nrep)]
      if(!is.null(not.miss2)) sim.ypl<-sim.ypl[rep(not.miss2, nrep)]
    }
  }
  # loop on covariate
  iplot<-0
  idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
  
  for(icov in 1:length(plot.opt$which.cov)) {
    iplot<-iplot+1 # Counter for list of plots
    lcov <-  plot.opt$which.cov[icov]
    plot.opt2<-plot.opt
    plot.opt2$which.cov<-lcov
    plot.opt2$xlab<-lcov
    # if(length(npdeObject@data@units$cov)>0) lunit <- npdeObject["data"]["units"]$cov[npdeObject["data"]["name.covariates"]==lcov] else lunit<-""
    # if(lunit!="") plot.opt2$xlab<-paste(plot.opt2$xlab," (", lunit,")",sep="")
    
    zecov <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"],lcov]
    if(!is.null(not.miss2)) zecov<-zecov[not.miss2]
    ucov <- zecov[match(unique(idobs),idobs)]
    if(is.numeric(ucov) & length(unique(ucov))>plot.opt$ncat){ # Continuous covariate with more than plot.opt$ncat (default 3)
      if(plot.opt$ncat!=3) { # 3 categories or less
        ncat<-plot.opt$ncat
        seqcat<-seq(0,1,length.out=(ncat+1))
        zecov.cat<-cut(zecov,breaks=quantile(ucov,seqcat), include.lowest=TRUE, ordered_result=TRUE)
        if(!is.null(not.miss2)) zecov<-zecov[not.miss2]
        nam1<-paste("q",format(seqcat[-(ncat+1)],digits=2),"-q",format(seqcat[-1],digits=2),sep="")
        namcat<-paste(lcov,nam1,sep=": ")
        zecov.cat<-factor(zecov.cat, labels=namcat, ordered=TRUE)
      } else { # if more than 3 categories, split in 3 ranges
        zecov.cat<-cut(zecov,breaks=quantile(ucov,c(0,0.25,0.75,1)), include.lowest=TRUE, ordered_result=TRUE)
        namcat<-paste(lcov,c("<Q1","Q1-Q3",">Q3"),sep=": ")
        zecov.cat<-factor(zecov.cat, labels=namcat, ordered=TRUE)
      }
    } else { # Categorical covariate defined as factor, or covariate with less than plot.opt$ncat categories
      namcat<-paste(lcov,unique(ucov), sep=": ")
      zecov.cat<-paste(lcov, zecov, sep=": ")
      zecov.cat<-factor(zecov.cat, labels=namcat, ordered=TRUE)
    }
    obsmat2<-obsmat
    obsmat2$grp<-zecov.cat
    pimat<-aux.npdeplot.pimat(obsmat2, xcent=namcat, quantiles=c(alpha, 0.5, 1-alpha), pi.size=plot.opt$pi.size, distrib=distrib, approx.pi=plot.opt$approx.pi, sim.ypl=sim.ypl)
    p1<-aux.npdeplot.boxcov(obsmat2, pimat, plot.opt2)

    list_plot[[iplot]] <- p1
  } # end loop on icov
  return(list_plot) # return invisibly, can we return plots that we can manipulate later ?
}
