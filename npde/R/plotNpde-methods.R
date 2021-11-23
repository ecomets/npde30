##################################################################################
# Plot for NpdeData

#' Plots a NpdeData object
#'
#' Plots the data in a NpdeData object
#'
#' @param x a NpdeData object
#' @param y unused, here for compatibility with the base plot function
#' @param \dots additional graphical parameters to be passed on to the plot
#' 
#' @return currently does not return anything, use plot(x, plot.type="data") on the npdeObject x (TODO; a ggplot object)
#' 
#' @details The default plot is a spaghetti plot of all the data, with a line joining the observations for each subject. If censored data is present, it is shown with a different symbol and colour.
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @seealso \code{\link{set.plotoptions}}
#' @keywords plot
#' @examples
#'\donttest{
#' data(theopp)
#'
#' x<-npdeData(theopp,name.group="ID",name.predictor="Time",name.response="Conc",
#' name.covariates=c("Wt"),units=list(x="hr",y="mg/L",covariates="kg"))
#' plot(x)
#' }
#' @importFrom graphics plot boxplot hist lines par points polygon rect segments
#' @import ggplot2 
#' @importFrom rlang .data
#' @method plot NpdeData
#' @export

plot.NpdeData <- function(x, y, ...) {
  # Plot the data, either as points or as lines grouped by x@name.group
  if(length(x@data)>0) {
    args1<-match.call(expand.dots=TRUE)
    i1<-match("type",names(args1))
    if(!is.na(i1)) {
      plot.type<-as.character(args1[[i1]])
      plot.type<-plot.type[plot.type!="c"]
    } else plot.type<-"b"
    plot.opt<-set.plotoptions(x)
    plot.opt$new<-TRUE
    plot.opt$xlab<-paste(x@name.predictor," (",x@units$x,")",sep="")
    plot.opt$ylab<-paste(x@name.response," (",x@units$y,")",sep="")
    plot.opt<-replace.plotoptions(plot.opt,...)
    logtyp<-paste(ifelse(plot.opt$xlog,"x",""),ifelse(plot.opt$ylog,"y",""),sep="")
#    if(plot.opt$new) par(mfrow=c(1,1))
    tab<-x@data[x@ind,] # remove missing data
    has.cens<-(length(x@name.cens)>0)
    if(has.cens && max(tab[,x@name.cens])==0) has.cens<-FALSE
    # if(plot.type=="p" | plot.type=="b") {
    #   if(has.cens) {
    #     plot(tab[,x@name.predictor],tab[,x@name.response],xlab=plot.opt$xlab, ylab=plot.opt$ylab,col=plot.opt$col,log=logtyp,xlim=plot.opt$xlim, ylim=plot.opt$ylim,main=plot.opt$main,sub=plot.opt$sub,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis, cex.lab=plot.opt$cex.lab,type="n")
    #     points(tab[tab[,x@name.cens]==0,x@name.predictor], tab[tab[,x@name.cens]==0,x@name.response],col=plot.opt$col.pobs, pch=plot.opt$pch.pobs,cex=plot.opt$cex)
    #     points(tab[tab[,x@name.cens]==1,x@name.predictor], tab[tab[,x@name.cens]==1,x@name.response],col=plot.opt$col.pcens, pch=plot.opt$pch.pcens,cex=plot.opt$cex)
    #   } else
    #     plot(tab[,x@name.predictor],tab[,x@name.response],xlab=plot.opt$xlab, ylab=plot.opt$ylab,col=plot.opt$col.pobs,pch=plot.opt$pch.pobs,log=logtyp,xlim=plot.opt$xlim, ylim=plot.opt$ylim,main=plot.opt$main,sub=plot.opt$sub,cex=plot.opt$cex,cex.axis=plot.opt$cex.axis, cex.lab=plot.opt$cex.lab)
    # }
    # if(plot.type=="l") {
    #   plot(tab[,x@name.predictor],tab[,x@name.response],xlab=plot.opt$xlab, ylab=plot.opt$ylab,col=plot.opt$col,lty=plot.opt$lty,lwd=plot.opt$lwd,type="n", log=logtyp,xlim=plot.opt$xlim,ylim=plot.opt$ylim,main=plot.opt$main,sub=plot.opt$sub, cex=plot.opt$cex,cex.axis=plot.opt$cex.axis, cex.lab=plot.opt$cex.lab)
    # }
    # if(plot.type=="l" | plot.type=="b") {
    #   for(isuj in unique(tab[,x@name.group])) {
    #     lines(tab[tab[,x@name.group]==isuj,x@name.predictor], tab[tab[,x@name.group]==isuj,x@name.response],col=plot.opt$col.lobs, lty=plot.opt$lty,lwd=plot.opt$lwd)
    #   }
    # }
    # if(has.cens) abline(h=x@loq,col=plot.opt$ablinecol,lty=plot.opt$ablinelty, lwd=plot.opt$ablinelwd)

  } else return("No data to plot")
}

# Plot for NpdeRes

#' Plots a NpdeRes object
#'
#' Plots distribution and scatterplots for the npde in a NpdeRes object. Users are advised to use the plot() function on the NpdeObject object resulting from a call to npde() or autonpde() instead of trying to plot only the results element of this object.
#'
#' @param x a NpdeRes object
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @details Four graphs are produced:
#' \describe{
#' \item{a quantile-quantile plot}{plot of the npde versus the corresponding quantiles of a normal distribution, with the line y=x overlayed.}
#' \item{a histogram of the npde}{the shape of the normal distribution is also shown}
#' \item{two scatterplots of the npde}{a plot of the npde versus the independent variable X and a plot of the npde versus the empirical mean of the predicted distribution; for these last two graphs, we plot the lines corresponding to y=0 and to the 5\% and 95\% critical value of the normal distribution delimiting a 90\% prediction interval for the npde}
#' }
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @seealso \code{\link{set.plotoptions}}
#' @keywords plot internal
#' @examples
#'\donttest{
#' data(theopp)
#'}
#' @method plot NpdeRes
#' @export

plot.NpdeRes <- function(x, y, ...) {

  # ------------------------------------------------------------------------------------------
  # ggplot
  # qqplot : pd, npde
  # ------------------------------------------------------------------------------------------
  oldpar <- par(no.readonly = TRUE) 
  on.exit(par(oldpar))   
  xres <- x@results@res[x@results@not.miss,]
  xobs =  x@data@data[x@data@name.predictor][,1]

  if(length(xres$pd)>0) {
    pd<-xres$pd
    nclass<-10
    par(mfrow=c(2,2))
    samp<-sort(pd);ndat<-length(samp)
    theo<-c(1:ndat)/ndat
    qqplot(samp,theo,xlab="Sample quantiles (pd)",ylab="Theoretical Quantiles",
           cex.lab=1.5,main="Q-Q plot versus U(0,1) for pd")
    segments(0,0,1,1)
    #Histogram of pd, with N(0,1) superimposed on the plot
    xh<-hist(pd,nclass=nclass,xlab="pd",main="",cex.lab=1.5)
    abline(h=ndat/nclass,lty=2,lwd=2)

    # residuals
    plot(xobs,pd,xlab=paste(x@data@name.predictor,"(",x@data@units$x,")"),ylab="np",cex.lab=1.5)
    abline(h=0,lty=2)
    x1<-qnorm(0.05)
    abline(h=x1,lty=3);abline(h=(-x1),lty=3)

    plot(xres$ypred,pd,xlab= paste(x@data@name.response,"(",x@data@units$y,")"),ylab="pd",cex.lab=1.5)
    abline(h=0,lty=2)
    abline(h=x1,lty=3);abline(h=(-x1),lty=3)
  }
}

##################################################################################
# Plot for NpdeObject

#' Plots a NpdeObject object
#'
#' Plots the data and diagnostic plots in a NpdeObject object
#'
#' @param x a NpdeObject object
#' @param y unused, here for compatibility with the base plot function
#' @param \dots additional graphical parameters, which when given will supersede graphical preferences stored in the object
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @details The default plots are represented as a 2x2 array with distribution plots on the top row (histogram and QQ-plot),
#'  and scatterplots of npde versus independent variable and population predictions on the bottom row. 
#' The graph is plotted in a graphic device window, unless the result is stored in an object (eg myplot<-plot(x)) which can then be printed (eg using print(myplot)).
#'
#'  @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.Mentre. Metrics for external model evaluation with an application to the population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research}, 23:2036--49, 2006.
#' @seealso \code{\link{set.plotoptions}}
#' @keywords plot
#' @examples
#'\donttest{
#' data(theopp)
#' data(simtheopp)
#'
#' x<-autonpde(theopp,simtheopp,iid="ID",ix="Time", iy="Conc", boolsave=FALSE)
#' plot(x)
#'}
#'
#' @importFrom graphics plot
#' @export

#	setMethod(f="plot",signature="NpdeObject", def=function(x,y,...) {

plot.NpdeObject <- function(x, y, ...) {

  args1<-match.call(expand.dots=TRUE)
  list.args <- list(...)

  i1<-match("new",names(args1))
  if(!is.na(i1)) force.new<-as.logical(as.character(args1[[i1]])) else force.new<-NULL
  verbose<-x@options$verbose
  i1<-match("verbose",names(args1))
  if(!is.na(i1) && is.logical((as.character(args1[[i1]])))) verbose<-as.logical(as.character(args1[[i1]]))
  i1<-match("plot.type",names(args1))
  if(!is.na(i1)) {
    plot.type<-as.character(args1[[i1]])
    plot.type<-plot.type[plot.type!="c"]
  } else { # if the user types dist.type instead of plot.type, and does not specify plot.type, we assume a typo
    i2<-match("dist.type",names(args1))
    if(!is.na(i2)) {
      plot.type<-as.character(args1[[i2]])
      } else plot.type<-"default"
  }

  i1<-match("which",names(args1))
  if(!is.na(i1)) {
    typmet<-as.character(args1[[i1]])
    typmet<-typmet[ typmet !="c"]
    typmet<-intersect(typmet, c("npd","npde","pd","pde")) # check if which is one of the allowed metrics, if not set to npd
  } else typmet<-"npd"
  if(length(typmet)==0) typmet<-"npd" # defaults to npd

  if(verbose) cat("Selected plot type:",plot.type,"\n")
  pltyp<-c("data","default", "ecdf","qqplot","histogram","x.scatter","pred.scatter", "covariates","cov.x.scatter","cov.pred.scatter","cov.hist","cov.qqplot", "cov.ecdf","vpc","loq", "cov.scatter")
  ifnd<-pmatch(plot.type,pltyp)
  if(sum(is.na(ifnd))>0) {
    if(verbose) cat("The following plot types were not found or are ambiguous:", plot.type[is.na(ifnd)],"\n")
  }
  ifnd<-ifnd[!is.na(ifnd)]

  if(length(ifnd)==0) return("Plot type not found\n")
  plot.type<-pltyp[ifnd]
  interactive<-x["prefs"]$interactive
  namObj<-deparse(substitute(x))

  # Check if pd or npde are present in the dataset, if not, perform the computation (ECO TODO remove ?)
  if(length(plot.type)>1 || plot.type[1]!="data") {
    icompute.pd<-icompute.npde<-icompute<-FALSE
    if(!is.na(match("pd", typmet)) | !is.na(match("both", typmet))) {
      if(length(x["results"]["res"]$pd)==0) icompute.pd<-TRUE  }
    if(!is.na(match("npde", typmet)) | !is.na(match("both", typmet))) {
      if(length(x["results"]["res"]$npde)==0) icompute.npde<-TRUE  }
    if(icompute.npde | icompute.pd) {
      icompute<-TRUE
      if(interactive) {
        i2<-(icompute.pd & icompute.npde)
        cok<-readline(prompt=paste("Computations will be performed to obtain", ifelse(icompute.pd,"pd",""),ifelse(i2," and ",""), ifelse(icompute.npde,"npde",""),", proceed ? (y/Y) [default=yes] ",sep=""))
        if(cok!="y"&cok!="Y"&cok!="yes"&cok!="") icompute<-FALSE
      } else
      {if(verbose) cat("Missing some elements for plots, will perform computations\n")}
    }
    if(icompute) {
      x["options"]$calc.npd<-icompute.pd
      x["options"]$calc.npde<-icompute.npde
      x<-npde.main(x)
      assign(namObj,x,envir=parent.frame())
    }
  }

  # ECO: commented this, not sure why it was set that way ???
#  if(typmet!="npde") x@prefs$bands<-FALSE

  # check the argument of plot
  # list.args = c(list(...),formalArgs( plot.NpdeObject ))
  # print(list.args, quote=TRUE)
  # stop()
  
  # remove duplicate arguments that we will set explicitly in case they have been passed in ...
  list.args<-list.args[!(names(list.args) %in% c("dist.type", "which.x", "which.y", "which"))]
  # print(list.args, quote=TRUE)
  # stop()

  # arguments added to list.args MUST HAVE THE NAME THEY HAVE within the function called by do.call => x becomes npdeObject within the auxiliary functions...
  list.args$npdeObject <- x
  
  for(ipl in plot.type) {
    switch (EXPR=ipl,
            "data"={
              plot.data = npde.plot.data(x,...)
              return( suppressWarnings(plot.data))
            },
            "default"={
              plot.default<-npde.plot.default(x, ...)
              invisible( suppressWarnings( plot.default)) # doesn't work to return the plot, but if return, plots the 4 plots
            },

            "x.scatter"={
              if(verbose) cat("Plotting scatterplot versus independent variable\n")
              list.plot.x.scatter = list()
              list.args$which.x<-"x"
              for(imet in typmet) {
                list.args$which.y<-imet
                list.plot.x.scatter[[imet]] <- do.call(npde.plot.scatterplot, list.args)
              }
              if(length(typmet)==1) list.plot.x.scatter<-list.plot.x.scatter[[1]]
              return( suppressWarnings(list.plot.x.scatter))
            },

            "pred.scatter"={
              if(verbose) cat("Plotting scatterplot versus predictions\n")
              list.plot.pred.scatter = list()
              list.args$which.x<-"pred"
              for(imet in typmet) {
                list.args$which.y<-imet
                list.plot.pred.scatter[[imet]] <- do.call(npde.plot.scatterplot, list.args)
              }
              if(length(typmet)==1) list.plot.pred.scatter<-list.plot.pred.scatter[[1]]
              return( suppressWarnings(list.plot.pred.scatter))
            },

            "covariates"={
              if(verbose) cat("Plotting boxplots of", typmet,"versus covariate categories\n")
              plot.covariates<-list()
#              list.args$which.x<-"cov"
#              list.args$new<-force.new
              for(imet in typmet) {
                list.args$which.y<-imet
                plot.covariates[[imet]] <- do.call(npde.plot.covariate, list.args)
              }
              if(length(typmet)==1) plot.covariates<-plot.covariates[[1]]
              return( suppressWarnings(plot.covariates))
            },

            "qqplot"={
              if(verbose) cat("Plotting QQ-plot of the distribution\n")
              list.plot.qqplot = list()
              list.args$dist.type<-"qqplot"
              list.args$new <- force.new
#              print(list.args[names(list.args)=="dist.type"], quote=TRUE)
              for(imet in typmet) {
                list.args$which <- imet # set which variable to plot
                list.plot.qqplot[[imet]] <-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.qqplot<-list.plot.qqplot[[1]]
              return( suppressWarnings(list.plot.qqplot))
            },
            "histogram"={
              if(verbose) cat("Plotting histogram of the distribution\n")
              list.plot.histogram = list()
              list.args$dist.type<-"hist"
              list.args$new <- force.new
              for(imet in typmet) {
                list.args$which <- imet
                list.plot.histogram[[imet]] <-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.histogram<-list.plot.histogram[[1]]
              return( suppressWarnings( list.plot.histogram ))
            },

            "ecdf"={
              if(verbose) cat("Plotting the empirical distribution function of residuals\n")
              list.plot.ecdf = list()
              list.args$dist.type<-"ecdf"
              list.args$new <- force.new
              for(imet in typmet) {
                list.args$which <- imet
                list.plot.ecdf[[imet]]<-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.ecdf<-list.plot.ecdf[[1]]
              return( suppressWarnings(list.plot.ecdf ))
            },

            "vpc"={ # similar to plot.type="x.scatter", which.x="x", which.y="yobs"
              if(verbose) cat("Plotting VPC\n")
              list.args$which.x <-"x"
              list.args$which.y <-"yobs"
              plot.vpc <- do.call(npde.plot.scatterplot, list.args)
              return( suppressWarnings( plot.vpc))
            },

            "cov.scatter"={ # scatterplots of variable versus covariates
              if(verbose) cat("Plotting scatterplot versus covariate(s)\n")
              list.args$covsplit<-TRUE
              list.args$which.x <-"cov"
              # we could probably just iteratively call the same function (but maybe pbs with ggplot objects...)
              list.plot.cov = list()
              for(imet in typmet) {
                list.args$which.y <- imet
                list.plot.cov[[imet]]<-do.call(npde.plot.scatterplot, list.args)
              }
              if(length(typmet)==1) list.plot.cov<-list.plot.cov[[1]]
              return( suppressWarnings(list.plot.cov ))
            },


            "cov.x.scatter"={ # alias for plot.type="x.scatter", covsplit=TRUE
              if(verbose) cat("Plotting scatterplot versus X, split by covariate(s)\n")
              list.args$covsplit<-TRUE
              list.args$which.x <-"x"
              # we could probably just iteratively call the same function (but maybe pbs with ggplot objects...)
              list.plot.cov = list()
              for(imet in typmet) {
                list.args$which.y <- imet
                list.plot.cov[[imet]]<-do.call(npde.plot.scatterplot, list.args)
              }
              if(length(typmet)==1) list.plot.cov<-list.plot.cov[[1]]
              return( suppressWarnings(list.plot.cov ))
            },

            "cov.pred.scatter"={ # alias for plot.type="pred.scatter", covsplit=TRUE
              if(verbose) cat("Plotting scatterplot versus predictions, split by covariate(s)\n")
              list.plot.pred.scatter = list()
              list.args$which.x<-"pred"
              list.args$covsplit <- TRUE
              for(imet in typmet) {
                list.args$which.y<-imet
                list.plot.pred.scatter[[imet]] <- do.call(npde.plot.scatterplot, list.args)
              }
              if(length(typmet)==1) list.plot.pred.scatter<-list.plot.pred.scatter[[1]]
              return( suppressWarnings(list.plot.pred.scatter))
            },

            "cov.hist"={ # alias for plot.type="hist", covsplit=TRUE
              if(verbose) cat("Plotting histogram of the distribution, split by covariate(s)\n")
              list.plot.histogram = list()
              list.args$dist.type<-"hist"
              list.args$covsplit <- TRUE
              # list.args$new <- force.new
              for(imet in typmet) {
                list.args$which <- imet
                list.plot.histogram[[imet]] <-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.histogram<-list.plot.histogram[[1]]
              return( suppressWarnings( list.plot.histogram ))
            },

            "cov.qqplot"={ # alias for plot.type="qqplot", covsplit=TRUE
              if(verbose) cat("Plotting histogram of the distribution, split by covariate(s)\n")
              list.plot.qqplot = list()
              list.args$dist.type<-"qqplot"
              list.args$covsplit <- TRUE
              # list.args$new <- force.new
              for(imet in typmet) {
                list.args$which <- imet # set which variable to plot
                list.plot.qqplot[[imet]] <-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.qqplot<-list.plot.qqplot[[1]]
              return( suppressWarnings(list.plot.qqplot))
            },

            "cov.ecdf"={  # alias for plot.type="ecdf", covsplit=TRUE
              if(verbose) cat("Plotting histogram of the distribution, split by covariate(s)\n")
              list.plot.ecdf = list()
              list.args$dist.type<-"ecdf"
              list.args$covsplit <- TRUE
#              list.args$new <- force.new
              for(imet in typmet) {
                list.args$which <- imet
                list.plot.ecdf[[imet]]<-do.call(npde.plot.dist, list.args)
              }
              if(length(typmet)==1) list.plot.ecdf<-list.plot.ecdf[[1]]
              return( suppressWarnings(list.plot.ecdf ))
            },

            "loq"={
              if(length(x["results"]["ploq"])>0) {
                if(verbose) cat("Plotting p_LOQ=p(yobs<LOQ) \n")
                list.args$xaxis <- "x"
                list.args$nsim <- 200
                list.plot.loq<-do.call(npde.plot.loq, list.args)
                return( suppressWarnings(list.plot.loq ))
              }
            },
           message(paste("Plot ",ipl," not implemented yet"))
    )
  }
}

