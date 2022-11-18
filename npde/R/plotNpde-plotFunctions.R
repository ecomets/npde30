##################################################################################
#' Select plot for a NpdeObject object
#'
#' Select plot for a NpdeObject object
#'
#' @usage npde.plot.select(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,
#' x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.scatter=FALSE, 
#' cov.x.scatter=FALSE, cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, 
#' cov.ecdf=FALSE, cov.hist=FALSE, cov.qqplot=FALSE, vpc=FALSE,...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param data boolean, whether to produce a plot of the data
#' @param ecdf boolean, whether to produce a distribution plot of the empirical distribution function
#' @param qqplot boolean, whether to produce a QQ-plot of the empirical distribution function
#' @param histogram boolean, whether to produce a histogram of the metric
#' @param x.scatter boolean, whether to produce a scatterplot of the metric as a function of X
#' @param pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions
#' @param x.box boolean, whether to produce whisker plots of the metric as a function of X
#' @param pred.box boolean, whether to produce whisker plots of the metric as a function of predictions
#' @param cov.scatter boolean, whether to produce a scatterplot of the metric as a function of covariate(s)
#' @param cov.x.scatter boolean, whether to produce a scatterplot of the metric as a function of X, split by covariate(s)
#' @param cov.pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions, split by covariate(s)
#' @param cov.x.box boolean, whether to produce whisker plots of the metric as a function of X, split by covariate(s)
#' @param cov.pred.box boolean, whether to produce whisker plots of the metric as a function of predictions, split by covariate(s)
#' @param cov.ecdf boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param cov.hist boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param cov.qqplot boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param vpc boolean, whether to produce a VPC
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export
#' @importFrom grid textGrob gpar 
#' @importFrom gridExtra grid.arrange

npde.plot.select<-function(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,x.scatter=FALSE,
                           pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.scatter=FALSE, cov.x.scatter=FALSE,
                           cov.pred.scatter=FALSE,cov.x.box=FALSE,
                           cov.pred.box=FALSE, cov.ecdf=FALSE, cov.hist=FALSE, cov.qqplot=FALSE, vpc=FALSE,...) {
  # Function selecting which plots are to be drawn
  namObj<-deparse(substitute(npdeObject))
  interactive<-npdeObject["prefs"]$interactive
  # ECO TODO: replace with partial matching
  if(data) plot(npdeObject,plot.type="data",...)
  if(ecdf) plot(npdeObject,plot.type="ecdf",...)
  if(qqplot) plot(npdeObject,plot.type="qqplot",...)
  if(histogram) plot(npdeObject,plot.type="histogram",...)
  if(x.scatter) plot(npdeObject,plot.type="x.scatter",...)
  if(pred.box) plot(npdeObject,plot.type="pred.scatter",box=TRUE,...)
  if(x.box) plot(npdeObject,plot.type="x.scatter",box=TRUE,...)
  if(pred.scatter) plot(npdeObject,plot.type="pred.scatter",...)
  if(cov.scatter) plot(npdeObject,plot.type="cov.scatter",...)
  if(cov.x.scatter) plot(npdeObject,plot.type="cov.x.scatter",...)
  if(cov.pred.scatter) plot(npdeObject,plot.type="cov.pred.scatter",...)
  if(cov.ecdf) plot(npdeObject,plot.type="cov.ecdf",...)
  if(cov.hist) plot(npdeObject,plot.type="cov.hist",...)
  if(cov.qqplot) plot(npdeObject,plot.type="cov.qqplot",...)
  if(cov.x.box) plot(npdeObject,plot.type="cov.x.scatter",box=TRUE,...)
  if(cov.pred.box) plot(npdeObject,plot.type="cov.pred.scatter",box=TRUE,...)
  if(vpc) plot(npdeObject,plot.type="vpc",...)
}

#' Default plots for a NpdeObject object
#'
#' Default plots for a NpdeObject object
#'
#' @usage default.npde.plots(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @return a ggplot object
#' @keywords plot internal

#### Meta-niveau
default.npde.plots<-function(npdeObject,...) {
  # When plot(npdeObject) is called without plot.type
  par(mfrow=c(2,2),ask=npdeObject["prefs"]$ask)
  npde.plot.select(npdeObject,qqplot=TRUE,histogram=TRUE, x.scatter=TRUE,pred.scatter=TRUE,new=FALSE,...)
}

#' Plots split by covariate for a NpdeObject object
#'
#' Plots split by covariate for a NpdeObject object (equivalent to using covsplit=TRUE with the appropriate plot.type)
#'
#' @usage npde.plot.splitcov(npdeObject, which.plot="x", ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which.plot one of "x" (scatterplots of the metric versus X), "pred" (scatterplots of the metric versus predictions), "ecdf" (empirical distribution function), "hist" (histogram), "qqplot"
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @keywords plot
#' @export

npde.plot.splitcov<-function(npdeObject,which.plot="x",...) {
  # Parameters or random effects versus covariates
  if(which.plot=="x") {
    plot(npdeObject,plot.type="cov.x.scatter",...)
  }
  if(which.plot=="pred") {
    plot(npdeObject,plot.type="cov.pred.scatter",...)
  }
  if(which.plot=="ecdf") {
    plot(npdeObject,plot.type="cov.ecdf",...)
  }
  if(which.plot=="qqplot") {
    plot(npdeObject,plot.type="cov.qqplot",...)
  }
  if(which.plot=="hist") {
    plot(npdeObject,plot.type="cov.hist",...)
  }
}

#' Plots for pd and npde
#'
#' Plots for pd and npde
#'
#' @aliases npde.plot.pd npde.plot.npde npde.plot.npd
#' @usage npde.plot.pd(npdeObject, ...)
#' @usage npde.plot.npde(npdeObject, ...)
#' @usage npde.plot.npd(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @keywords plot
#' @export

npde.plot.npde<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for npde\n")
  default.npde.plots(npdeObject,...)
}

#' @export

npde.plot.npd<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for npd\n")
  default.npde.plots(npdeObject,which="npd",...)
}


#' @export

npde.plot.pd<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for pd\n")
  default.npde.plots(npdeObject,which="pd",...)
}

################################    Data    #####################################

#' Plot a NpdeData object
#'
#' Produces a spaghetti plot of the data
#'
#' @aliases npde.plot.data
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @keywords plot
#' @export

npde.plot.data<-function(npdeObject,...) {

  ## to do
  ## npdeData = npdeObject["data"]
  ## plot npdeData avec prefs npdeObject
  ## ensuite changer options

  # method plot.npde.data(x,y,...)
  # list(..) = list de preferences
  # ensuite remplacer les prefs par list(...)

  # but plot plot(yvir50@data) = plot(yvir50,plot.type=“data”)

  if(!is(npdeObject,"NpdeObject")) return()
  # data censored / no censored
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
#    icens<-npdeObject["data"]["icens"]
    is.cens<-npdeObject["data"]["data"]$cens==1
  } else { has.cens<-FALSE}

  # data plot x,y and id
  x<-npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]]

  # plot options and user options
  userPlotOptions = list(...)
  plot.opt<-npdeObject["prefs"]
  plot.opt <- modifyList(plot.opt, userPlotOptions[intersect(names(userPlotOptions), names(plot.opt))])
  if(plot.opt$impute.loq)  y<-npdeObject["results"]["res"]$ycomp else y<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  id<-(npdeObject["data"]["data"]$index %in% plot.opt$ilist)
  
  # meta arguments to change col,lwd,lty,pch for lines and symbols
  if ( plot.opt$size %in% userPlotOptions)    plot.opt$size.pobs = plot.opt$size

  if ( plot.opt$col %in% userPlotOptions)  {
    plot.opt$col.pobs = plot.opt$col
    plot.opt$col.lobs = plot.opt$col
  }

  if ( plot.opt$lwd %in% userPlotOptions)
    plot.opt$lwd.lobs = plot.opt$lwd

  if ( plot.opt$pch %in% userPlotOptions)  {
    plot.opt$pch.pobs <- plot.opt$pch
    plot.opt$pch.pcens = plot.opt$pch
  }

  if ( plot.opt$lty %in% userPlotOptions)
    plot.opt$lty.lobs = plot.opt$lty

  # xlab, ylab
  if(plot.opt$xlab=="") {
    plot.opt$xlab <-paste0( npdeObject@data@name.predictor )
    if (npdeObject@data@units$x != "") plot.opt$xlab<-paste0(plot.opt$xlab, " (", npdeObject@data@units$x,")" )
  }
  if(plot.opt$ylab=="") {
    plot.opt$ylab <-paste0( npdeObject@data@name.response)
    if (npdeObject@data@units$y != "") plot.opt$ylab<-paste0(plot.opt$ylab, " (", npdeObject@data@units$y,")" )
  }

  # -----------------------------------------------------------------------------------------------------------------

  # ggplot with censored data
  if(has.cens) { # do we really need to separate those 2 cases ? we oculd use if to add the relevant points for censored data as in the other plots

    xplot = x[id & !is.cens]
    yplot = y[id & !is.cens]

      # data no censored
      grouplot = npdeObject@data@data$index[id & !is.cens]
      dataplot = data.frame(grouplot,xplot,yplot)
      colnames(dataplot) = c("group","x","y")

    if(plot.opt$plot.loq==TRUE)
    {

      # data censored
      group_loq_plot = npdeObject@data@data$index[id & is.cens]
      dataloq_plot = data.frame(group_loq_plot, x[id & is.cens],y[id & is.cens])
      colnames(dataloq_plot) = c("group","x","y")

      # both
      dataplot = rbind( dataplot, dataloq_plot )
      colnames(dataplot) = c("group","x","y")

    }

    # add loq last columns for plot geom_hline
    dataplot = cbind( dataplot, rep( npdeObject@data@loq, dim(dataplot)[1]))
    colnames(dataplot) = c("group","x","y","loq")

    #  xlim and ylim
    if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
      x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
    } else {
      x.limits = 1.01*c(0,max(dataplot$x,na.rm = TRUE))}

    if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
      y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
    } else {
      y.limits = c(min(dataplot$y, dataplot$y,na.rm = TRUE),max(dataplot$y, dataplot$y,na.rm = TRUE))}

    # loq value
    loq = npdeObject@data@loq

    # ggplot template

    p = ggplot(dataplot, aes(x=.data$x, y=.data$y)) +

      theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
            axis.title.y = element_text(size = plot.opt$size.ylab),
            axis.title.x = element_text(size = plot.opt$size.xlab),
            axis.text.x = element_text(size=plot.opt$size.text.x),
            axis.text.y = element_text(size=plot.opt$size.text.y),
            axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
            axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
            panel.background=element_rect("white"),
            panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+

      ggtitle(plot.opt$main) +

      coord_cartesian(xlim=x.limits, ylim=y.limits) +

      geom_point(dataplot,
                 mapping=aes(x=.data$x,y=.data$y),
                 color = plot.opt$col.pobs,
                 alpha = plot.opt$alpha.pobs,
                 size = plot.opt$size.pobs,
                 shape = plot.opt$pch.pobs)  +

      geom_line(aes(group=group),
                linetype = plot.opt$lty.lobs,
                color = plot.opt$col.lobs,
                size = plot.opt$lwd.lobs )+

      {if(plot.opt$line.loq==TRUE)

        geom_hline(dataplot,mapping = aes(yintercept = as.numeric(loq)),
                   linetype = plot.opt$lty.line.loq,
                   color = plot.opt$col.line.loq,
                   size = plot.opt$lwd.line.loq  )}+
      
      {if(plot.opt$plot.loq ==TRUE)
        geom_point(dataloq_plot,
                   mapping = aes(x=.data$x,y=.data$y),
                   color = plot.opt$col.pcens,
                   shape = plot.opt$pch.pcens,
                   size = plot.opt$size.pcens,
                   alpha = plot.opt$alpha.pcens)}  +

      scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x)) +
      scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))
  }# end if cens

  if(!has.cens) {

    xplot = x[id]
    yplot = y[id]

    grouplot = npdeObject@data@data$index[id]
    dataplot = data.frame(grouplot,xplot,yplot)
    colnames(dataplot) = c("group","x","y")

    # xlim and ylim
    if ("xlim" %in% names(plot.opt) & length(plot.opt$xlim)==2) {
      x.limits = c(plot.opt$xlim[1],plot.opt$xlim[2])
    } else {
      x.limits = 1.01*c(0,max(dataplot$x,na.rm = TRUE))}

    if ("ylim" %in% names(plot.opt) & length(plot.opt$ylim)==2) {
      y.limits = c(plot.opt$ylim[1],plot.opt$ylim[2])
    } else {
      y.limits = c(min(dataplot$y,na.rm = TRUE),max(dataplot$y,na.rm = TRUE))}

    ## ggplot template

    p = ggplot(dataplot, aes(x=.data$x, y=.data$y)) +

      theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
            axis.title.y = element_text(size = plot.opt$size.ylab),
            axis.title.x = element_text(size = plot.opt$size.xlab),
            axis.text.x = element_text(size=plot.opt$size.text.x),
            axis.text.y = element_text(size=plot.opt$size.text.y),
            axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
            axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
            panel.background=element_rect("white"),
            panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.minor.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
            panel.grid.minor.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+

       ggtitle(plot.opt$main) +

       coord_cartesian(xlim=x.limits, ylim=y.limits) +

       geom_point(color = plot.opt$col.pobs,
                  alpha = plot.opt$alpha.pobs,
                  size = plot.opt$size.pobs,
                  shape = plot.opt$pch.pobs) +

       geom_line(aes(group=group),
                 linetype = plot.opt$lty.lobs,
                 color = plot.opt$col.lobs,
                 size = plot.opt$lwd.lobs )+

       scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x)) +
       scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))
  }
  # list_plot = list()
  # list_plot[[1]] = p
  # return( list_plot )
  return(p)

} # end function

################################    Default gof plots  #################################

#' Diagnostic plots
#'
#' The default diagnostic plots produced after a call to \code{\link{npde}} or \code{\link{autonpde}} include a histogram of the distribution, a QQ-plot compared to the theoretical distribution, and scatterplots versus the independent variable and versus the population predictions from the model
#'
#' @usage npde.plot.default(npdeObject, ...)
#'
#' @aliases plot
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' 
#' @details By default, npd are used for the diagnostic plots. If an unknown argument to which (eg which="XXX") is given, 
#' this is changed to npd (with a warning message if verbose=TRUE or the verbose option in the option slot of the npdeObject is TRUE).
#' 
#' @return a ggplot object or a list of ggplot objects (grobs)
#' 
#' @export
#' @keywords plot
## #' @keywords plot by default : qqplot, hist, x.scatter, pred.scatter

npde.plot.default<-function(npdeObject,  ...) {
  
  # modify the plot options with the user ones (... can supersede some arguments in plot.opt, eg covsplit to force a plot by covariates)
  userPlotOptions = list(...)
  plot.opt <- set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, userPlotOptions[intersect(names(userPlotOptions), names(plot.opt))])
  list.args <- list(...) # list used to pass on to individual functions through do.call
  list.args<-list.args[!(names(list.args) %in% c("dist.type", "which.x", "which.y", "which"))]
  list.args$npdeObject <- npdeObject
  typmet<-intersect(plot.opt$which, c("npd","npde","pd","pde")) # check if which is one of the allowed metrics, if not set to npd
  if(length(typmet)==0) {
    if(npdeObject@options$verbose) message(paste(plot.opt$which,"not recognised, plotting npd\n"))
      plot.opt$which<-"npd"
  }

  # Check covariate input
  if(plot.opt$covsplit) {
    if(plot.opt$which.cov[1]=="all") plot.opt$which.cov<-npdeObject@data@name.covariates
    found.cov <- intersect(plot.opt$which.cov, npdeObject@data@name.covariates)
    if(length(found.cov)!=length(plot.opt$which.cov)) {
      if(npdeObject@options$verbose) cat("Some covariates not found, check inputs \n")
      plot.opt$which.cov<-found.cov
    }
    if(length(plot.opt$which.cov)==0) plot.opt$covsplit<-FALSE
  }

  if(plot.opt$covsplit && length(plot.opt$which.cov)>0 ) {
#    cat("Currently not splitting by covariate, please call the plots individually using plot.type='' with the argument covsplit=TRUE.\n")
    for(icov in plot.opt$which.cov) {
     list.args$which.cov <- icov 
     list.args$covsplit <- TRUE
     list.args$main <- ""
     
     list.args$which <- plot.opt$which
     list.args$dist.type <- "hist"
     hist<-do.call(npde.plot.dist, list.args)
     list.args$dist.type <- "qqplot"
     qqplot<-do.call(npde.plot.dist, list.args)
     
     list.args$which.y <- plot.opt$which
     list.args$which.x <- "x"
     x.scatter<-do.call(npde.plot.scatterplot, list.args)
     list.args$which.x <- "pred"
     pred.scatter<-do.call(npde.plot.scatterplot, list.args)
     
     list_plot<-list(hist, qqplot, x.scatter, pred.scatter)
     if(length(list_plot)==4) {
       grid.arrange(grobs = list_plot,
                    nrow=2, ncol=2, vjust = 1,
                    top = textGrob(paste0(plot.opt$main,'\n'), gp = gpar(fontsize=plot.opt$size.main)))
     } else {
       if(npdeObject@options$verbose) cat("Problems in some or all default plots, check inputs\n")
     }
    }
  } else {
    list.args$covsplit<-FALSE
    list.args$which <- plot.opt$which
    list.args$dist.type <- "hist"
    list.args$main <- ""
    hist<-do.call(npde.plot.dist, list.args)
    
    list.args$dist.type <- "qqplot"
    qqplot<-do.call(npde.plot.dist, list.args)
    
    list.args$which.y <- plot.opt$which
    list.args$which.x <- "x"
    x.scatter<-do.call(npde.plot.scatterplot, list.args)
    
    list.args$which.y <- plot.opt$which
    list.args$which.x <- "pred"
    pred.scatter<-do.call(npde.plot.scatterplot, list.args)
    
    list_plot<-list(hist, qqplot, x.scatter, pred.scatter)
    if(length(list_plot)==4) {
      grid.arrange(grobs = list_plot,
                   nrow=2, ncol=2, vjust = 1,
                   top = textGrob(paste0(plot.opt$main,'\n'), gp = gpar(fontsize=plot.opt$size.main)))
    }
#    invisible(list_plot) # invisible doesn't work with ggplot objects :-/
    if(length(list_plot)==4) return(list_plot) else return() # ou return(list_plot) dans tous les cas ?
  }
} # end function npde.plot.default
