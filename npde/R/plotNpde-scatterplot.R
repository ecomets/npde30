#' #################    Scatterplots - VPC, scatterplots versus independent variable, predictions, covariates  ###################

#' Scatterplots and VPC
#'
#' Produces a scatterplot. Different types of scatterplots can be produced, with associated prediction bands (see details).
#'
#' @usage npde.plot.scatterplot(npdeObject, which.x="x", which.y="npde", ref.prof=NULL, ...)
#'
#' @aliases compute.bands.true compute.bands aux.npdeplot.computepi aux.npdeplot.meanprof aux.npdeplot.pimat aux.npdeplot.transformPI aux.npdeplot.transformObs
#'
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which.x a string specifying the variable on the X-axis (one of "x", "pred", "cov")
#' @param which.y a string specifying the variable on the Y-axis (one of "yobs", "npde", "pd", "npd")
#' @param ref.prof either the character string "covariate" or a named list
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}} and \code{\link{npdeControl}})
#'
#' @details VPC: obtained using which.x="x", which.y="yobs"
#' @details Scatterplots of npde/pd/npd can be obtained versus "x" (independent variable) or "pred" (population predictions from the model)
#' @details Scatterplots of npde/pd/npd/observations can be obtained versus covariates by setting the which.x argument to "cov" and selecting the appropriate which.y. The function will use the covariates in the which.cov element of the prefs slot. This can be overriden to cycle over all the covariates in the dataset by supplying the argument which.cov="all" in the call to the function.
#' @details Reference profile: a reference profile can be added to scatterplots of npd and npde versus the independent variable (see Comets et al. 2013)
#' @details If ref.prof="covariate" and an additional argument covsplit is given (covsplit=TRUE), the reference plot will be adjusted for each covariate category over all the covariates in the which.cov element of the prefs slot (see  \code{\link{npdeControl}} for details on the prefs slot of the npdeObject).
#' @details If ref.prof is given as a named list (eg list(ID=c(1,5)) or list(sex=0, dose=c(50,100)), where names should refer to columns in the data file (eg ID should be a column in the data)), the reference profile will be obtained by combining (in the first example above, the reference profile will be obtained using the simulated data for subjects 1 and 5, while in the second example it will be computed using the subjects with sex=0 given doses 50 or 100).
#'
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}, \code{\link{npdeControl}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.  Mentre.
#' Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide.
#' \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @references  E. Comets, T.H.T. Nguyen, and F. Mentré F. Additional features and graphs in the new npde library for R.
#' \emph{22th meeting of the Population Approach Group in Europe}, Glasgow, United Kingdom, 2013.
#' @keywords plot
#' @export
#'
#' @importFrom stats approx
#' @importFrom stats median

# -------------------------------------------------------------------------------------
# npde.plot.meanprofile renamed to npde.plot.scatterplot
# function to create a scatterplot
# -----------------------------------------------------------------------------------

npde.plot.scatterplot<-function(npdeObject, which.x="x", which.y="npde", ref.prof=NULL, ...){ #} xscale=FALSE, onlog=FALSE, ref.prof=NULL, ...) {
    # npdeObject: object returned from a npde run
    # which.x: variable on the X-axis, one of "x", "pred", "cov" ( ? npde/npd ?)
    # which.y: variable on the Y-axis, one of "npde", "npd", "pd", "yobs" (VPC)  + (? "pde", "cov" ?)
    # ref.prof: reference profile (if present, will plot only tnpde or tnpd, no VPC or cov plots)
    ## if ref.prof="covariate" and covsplit is TRUE, the reference plot will be adjusted for each covariate category
    ## otherwise, ref.prof must be a named list, eg list(ID=c(1,5)) to select subjects with ID=1 and 5 as reference; names should refer to columns in the data file (eg ID should be a column in the data)
    ## Note: maybe try to pass ref.prof as an expression ???

#  xscale=FALSE => removed, if ref.prof is given then we use that + option to use ref.prof per covariate category
#  onlog=FALSE # deprecated
  # -----------------------------------------------------------------------------------   # refs and plot options

  #plot.opt = npdeObject["prefs"]
  userPlotOptions  = list(...)
  plot.opt <- set.plotoptions.default( npdeObject )
  plot.opt <- modifyList( plot.opt, userPlotOptions[ intersect( names( userPlotOptions ), names( plot.opt ) ) ] )

  # size replace size.pobs
  if ( plot.opt$size %in% userPlotOptions)
  {
    plot.opt$size.pobs = plot.opt$size
  }

  # col replace  col.pobs
  if ( plot.opt$col %in% userPlotOptions)
  {
    plot.opt$col.pobs = plot.opt$col
  }

# -----------------------------------------------------------------------------------
# Check inputs

  which.y = plot.opt$which

  if(match(which.x,c("x","pred","cov"),nomatch=0)==0) {
    cat("Option which.x=",which.x,"not recognised\n")
    return()
  }
  if(match(which.y,c("npde","tnpde","pd","npd","yobs"),nomatch=0)==0) {
    cat("Option which.y=",which.y,"not recognised\n")
    return()
  }

  if(which.x=="npde" | which.y=="npde") {
    if(is.na(match("npde",colnames(npdeObject@results@res)))) return()
  }
  if(which.x=="npd" | which.y=="npd") {
    if(is.na(match("npd",colnames(npdeObject@results@res)))) {
      if(!is.na(match("pd",colnames(npdeObject@results@res)))) npdeObject@results@res$npd<-qnorm(npdeObject@results@res$pd)
    } else return()
  }
  if(which.x=="pd" | which.y=="pd") {
    if(is.na(match("pd",colnames(npdeObject@results@res)))) return()
  }
  if(which.x=="cov" | plot.opt$covsplit) { # Test if covariates are present in the dataset for plots vs cov or for plots split by cov
    if(length(plot.opt$which.cov)==1) {
      if(plot.opt$which.cov=="all" | plot.opt$which.cov=="") plot.opt$which.cov<-npdeObject["data"]["name.covariates"]
    }
    idx.cov = match(plot.opt$which.cov,npdeObject["data"]["name.covariates"])
    if (length(idx.cov)==0) {
      cat("Error: plot over covariates required but no matching covariate found in dataset")
      return()
    }
  }
  if(which.x=="cov") plot.opt$covsplit<-TRUE # covsplit is used to loop on covariates
  # Only use reference profile for npd or npde versus time
  if(!is.null(ref.prof)) {
    if(!(which.y %in% c("npd","npde"))) ref.prof<-NULL
    if(which.x %in% c("cov","pred")) ref.prof<-NULL # note: important that ref.prof set to NULL and covsplit set to TRUE if which.x="cov"
  }

  # -----------------------------------------------------------------------------------
  # Setup

  list_plot = list()   # list to stack the ggplot

  covsplit <- plot.opt$covsplit

  if(is.numeric(plot.opt$which.cov)) plot.opt$which.cov<-npdeObject["data"]["name.covariates"][plot.opt$which.cov] # convert to names of covariates
  # covariates in the npdeObject => hasCovariates TRUE / FALSE
  if (length(npdeObject["data"]["name.covariates"])>0) hasCovariates = TRUE else hasCovariates = FALSE

  if(which.y %in% c("pd","pde")) distrib<-"unif" else distrib<-"norm"

  # a line at Y=LOQ does not make sense unless we are plotting VPC or transformed npde
  if(which.y %in% c("pd","pde")) plot.opt$line.loq<-FALSE
  if(which.y %in% c("npde","npd") & is.null(ref.prof)) plot.opt$line.loq<-FALSE

  if(plot.opt$xlab=="") {
    plot.opt$xlab <- switch(which.x, "x"=paste0( npdeObject@data@name.predictor ), "pred"=paste0("Predicted ", npdeObject@data@name.response ), "cov"="", "npde"="npde", "npd"="npd", "pd"="pd") # cov, npde, npd, pd: not valid options; cov: to be implemented
    if (which.x=="x" & npdeObject@data@units$x != "") plot.opt$xlab<-paste0(plot.opt$xlab, "(", npdeObject@data@units$x,")" )
    if (which.x=="pred" & npdeObject@data@units$y != "") plot.opt$xlab<-paste0(plot.opt$xlab, "(", npdeObject@data@units$y,")" )
  }

  if(plot.opt$ylab=="") {
    plot.opt$ylab <- switch(which.y, "npde"="npde", "npd"="npd", "pd"="pd", "yobs"=paste0( npdeObject@data@name.response),  "cov"="") # cov not a valid option (yet ?)
    if (which.y=="yobs" & npdeObject@data@units$y != "") plot.opt$ylab<-paste0(plot.opt$ylab, "(", npdeObject@data@units$y,")" )
  }

  # vpc.interval controls which percentiles we want PI for
  alpha <- (1 - plot.opt$vpc.interval) / 2
  if(alpha>0.5) alpha<-(1-alpha)
  nrep<-npdeObject["sim.data"]["nrep"]

  # -----------------------------------------------------------------------------------
  # Creating obsmat
  ## for which.x="cov", create obsmat with the first covariate and then cycle over covariates later

  obsmat <- data.frame(x=switch(which.x, "x"=npdeObject@data@data[,npdeObject@data@name.predictor],
                     "pred"=npdeObject@results@res$ypred,
                     "cov"=npdeObject@data@data[,npdeObject@data@name.covariates[idx.cov[1]]] ))
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

  # ECO TODO: check dimensions when MDV=1 (same dimensions between obsmat and simulated data ? if not cut here, and also check res)

  # Binning
  if(is.numeric(obsmat$x)) {
    xbin<-npde.binning(obsmat$x,plot.opt,verbose=FALSE)
    obsmat$grp <- xbin$xgrp
    matbin<-data.frame(grp=1:length(xbin$xcent), xcent=xbin$xcent, binlabel = names(xbin$xcent)) # keep binning matrix
  } else obsmat$grp <- 1 # for covariates in "cov.scatter", binning will be done later

  # -----------------------------------------------------------------------------------
  # Reference profile
  ## if unique, compute it here; if by covariate, set refprof.by.cov to TRUE and create simulation matrix with all the simulated data

  refprof.by.cov<-FALSE # create indicator variable
  hasRefprof<-!(is.null(ref.prof))
  if(hasRefprof) {
    hasRefprof<-TRUE
   if(hasCovariates) {
     if(tolower(ref.prof) %in% c("covariate","cov","covariates")) { # keep all simulations, add all covariates to msim
       refprof.by.cov<-TRUE
       plot.opt$covsplit<-TRUE
       msim<-npdeObject["sim.data"]["datsim"]$ysim
       msim<-msim[rep(npdeObject["data"]["not.miss"], nrep)]
       if(!is.null(not.miss2)) msim<-msim[rep(not.miss2, nrep)]
       msim<-data.frame(ysim=msim, grp=rep(obsmat$grp, nrep))
     } else {
       if(!is.list(ref.prof)) {
         cat("The reference profile must be entered as a named list, eg list(ID=c(1,5)) to select subjects with ID=1 and 5 as reference; names should refer to columns in the data file.\n")
         ref.prof<-NULL
         hasRefprof<-FALSE
       } else { # one reference profile for all the plots
         msim<-npdeObject["sim.data"]["datsim"]$ysim
         msim<-msim[rep(npdeObject["data"]["not.miss"], nrep)]
         if(!is.null(not.miss2)) msim<-msim[rep(not.miss2, nrep)]
         msim<-data.frame(ysim=msim, grp=rep(obsmat$grp, nrep))
         iuse<-rep(0,dim(obsmat)[1])
         dat1<-npdeObject["data"]["data"][npdeObject["data"]["not.miss"],]
         for(iref in 1:length(ref.prof)) {
           i<-names(ref.prof)[iref]
           i1<-which(dat1[,i] %in% ref.prof[[iref]])
           if(iref==1) idx1<-i1 else idx1<-intersect(idx1,i1)
         }
         iuse[idx1]<-1
         iuse<-rep(iuse, nrep)
         msim<-msim[iuse==1,]
         mpref<-aux.npdeplot.meanprof(mbin=matbin,msim=msim)
       }
     }
   }
  }
  if(hasRefprof) {
    plot.opt$ylab<-paste("Transformed ", plot.opt$ylab,sep="")
    if(length(npdeObject@data@units$y)>0) plot.opt$ylab<-paste(plot.opt$ylab," (Reference profile units:", npdeObject@data@units$y,")",sep="")
  }

  # -----------------------------------------------------------------------------------
  # Creating pimat
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

  if(!covsplit) { # Single plot

    pimat<-aux.npdeplot.pimat(obsmat, xcent=matbin$xcent , quantiles=c(alpha, 0.5, 1-alpha), pi.size=plot.opt$pi.size, distrib=distrib, approx.pi=plot.opt$approx.pi, sim.ypl=sim.ypl)

    if(hasRefprof) {

      obsmat<-aux.npdeplot.transformObs(obsmat, mpref)

      pimat<-aux.npdeplot.transformPI(pimat, mpref)

    }

   ## Plot obsmat with PI in pimat using options in plot.opt

    p1<-aux.npdeplot.scatter(obsmat, pimat, plot.opt)

    list_plot[[1]]<-p1

  } else { # Plot split by covariate, for each covariate in plot.opt$which.cov
    idobs <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"], npdeObject["data"]["name.group"]]
    iplot<-0

    # loop on covariate
    for(icov in 1:length(plot.opt$which.cov)) {
      iplot<-iplot+1 # Counter for list of plots
      lcov <-  plot.opt$which.cov[icov]
      plot.opt2<-plot.opt
      plot.opt2$which.cov<-lcov
      if(which.x=="cov") {
        plot.opt2$xlab<-lcov
      if(length(npdeObject@data@units$cov)>0) lunit <- npdeObject["data"]["units"]$cov[npdeObject["data"]["name.covariates"]==lcov] else lunit<-""
      if(lunit!="") plot.opt2$xlab<-paste(plot.opt2$xlab," (", lunit,")",sep="")
      }
      zecov <- npdeObject["data"]["data"][npdeObject["data"]["not.miss"],lcov]
      if(!is.null(not.miss2)) zecov<-zecov[not.miss2]
      ucov = zecov[match(unique(idobs),idobs)]

      if(which.x=="cov") { # plot versus covariates
        obsmat2<-obsmat
        if(is.numeric(ucov)) {
          obsmat2$x<-zecov
          xbin2<-npde.binning(obsmat2$x,plot.opt,verbose=FALSE)
          obsmat2$grp <- xbin2$xgrp
          xcent2<-xbin2$xcent
          } else {
            namcat<-paste(lcov,unique(ucov), sep=": ")
            zecov.cat<-paste(lcov, zecov, sep=": ")
            zecov.cat<-factor(zecov.cat, labels=namcat, ordered=TRUE)
            obsmat2$x<-match(zecov.cat,levels(zecov.cat))
            obsmat2$grp<-zecov.cat
            xcent2<-1:length(namcat)
        }
        pimat<-aux.npdeplot.pimat(obsmat2, xcent=xcent2 , quantiles=c(alpha, 0.5, 1-alpha), pi.size=plot.opt$pi.size, distrib=distrib, approx.pi=plot.opt$approx.pi, sim.ypl=sim.ypl)
        p1<-aux.npdeplot.scatter(obsmat2, pimat, plot.opt2)
      } else { # plot split by covariates
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

     obsmat$category<-zecov.cat
     if(length(sim.ypl)>0) sim.ypl.cat<-rep(obsmat$category, nrep) else sim.ypl.cat<-1

     # --------------------------------------------------------------------------------
     # Creating pimat by stacking the pimat for the different categories
     pimat<-NULL
     if(hasRefprof) tobsmat<-NULL else tobsmat<-obsmat
     for(icat in levels(obsmat$category)) {
       obsmat.cov<-obsmat[obsmat$category==icat,]
       pimat.cov<-aux.npdeplot.pimat(obsmat.cov, xcent=matbin$xcent , quantiles=c(alpha, 0.5, 1-alpha), pi.size=plot.opt$pi.size, distrib=distrib, approx.pi=plot.opt$approx.pi, sim.ypl=sim.ypl[sim.ypl.cat==icat])
       if(hasRefprof) {
         if(refprof.by.cov) { # create reference profile for the current category
           msim.cov<-msim[rep(obsmat$category==icat,nrep),]
           cat(icat,dim(obsmat.cov),"   sim:",dim(msim.cov),"\n")
           mpref.cov<-aux.npdeplot.meanprof(mbin=matbin,msim=msim.cov)
         } else { # use global reference profile
           mpref.cov<-mpref
         }
         pimat.cov<-aux.npdeplot.transformPI(pimat.cov, mpref.cov)
         obsmat.cov<-aux.npdeplot.transformObs(obsmat.cov, mpref.cov)
         tobsmat<-rbind(tobsmat, obsmat.cov)
       }
     pimat<-rbind(pimat,pimat.cov)
     }
     # scatterplot using the obsmat and pimat generated for the covariate lcov
     p1<-aux.npdeplot.scatter(tobsmat, pimat, plot.opt2)
     list_plot[[iplot]]<-p1

    } # end test on which.x="cov"
    } # end loop on icov
  } # end test on covsplit

  return(list_plot)

 # invisible(list_plot) # return invisibly, can we return plots that we can manipulate later ?
 } # END FUNCTION

# --------------------------------------------------------------
# les options bizarres : onlong and co (see NpdeControl)
# --------------------------------------------------------------

# which.resplot=c("res.vs.pred") : désactivée
# smooth=TRUE, # désactivée # pour  rajouter points milieu bin # pas dans scatter
# line.smooth="s", # désactivée # pour  rajouter points milieu bin # pas dans scatter
# box=TRUE, # désactivée # à remettre
# ncat=1 # fonctionne pas
# interactive=TRUE,  #  désactivée
# boxwex  #  désactivée
# varwidth  #  désactivée # taille boite à moustaches
# range=3 # désactivée, ne sers pas ailleurs que dans npdeControl # taille de l'interquartile dans boite à moustaches
# onlog=FALSE : définie par défaut FALSE dans node.mean.profile, pas redéfinie ailleurs
# log(y) for vpc + meanprofile
# log(x)


