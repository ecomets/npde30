##################################################################################
#' Select plot for a NpdeObject object
#'
#' Select plot for a NpdeObject object
#'
#' @usage npde.plot.select(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,
#' x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,
#' cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param data boolean, whether to produce a plot of the data
#' @param ecdf boolean, whether to produce a distribution plot of the empirical distribution function
#' @param qqplot boolean, whether to produce a QQ-plot of the empirical distribution function
#' @param histogram boolean, whether to produce a histogram of the metric
#' @param x.scatter boolean, whether to produce a scatterplot of the metric as a function of X
#' @param pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions
#' @param x.box boolean, whether to produce whisker plots of the metric as a function of X
#' @param pred.box boolean, whether to produce whisker plots of the metric as a function of predictions
#' @param cov.x.scatter boolean, whether to produce a scatterplot of the metric as a function of X, split by covariate(s)
#' @param cov.pred.scatter boolean, whether to produce a scatterplot of the metric as a function of predictions, split by covariate(s)
#' @param cov.x.box boolean, whether to produce whisker plots of the metric as a function of X, split by covariate(s)
#' @param cov.pred.box boolean, whether to produce whisker plots of the metric as a function of predictions, split by covariate(s)
#' @param cov.ecdf boolean, whether to produce a distribution plot of the empirical distribution function, split by covariate(s)
#' @param vpc boolean, whether to produce a VPC
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @author Emmanuelle Comets <emmanuelle.comets@@bichat.inserm.fr>
#' @seealso \code{\link{npde}}, \code{\link{autonpde}}, \code{\link{set.plotoptions}}
#' @references K. Brendel, E. Comets, C. Laffont, C. Laveille, and F.
#' Mentre. Metrics for external model evaluation with an application to the
#' population pharmacokinetics of gliclazide. \emph{Pharmaceutical Research},
#' 23:2036--49, 2006.
#' @keywords plot
#' @export

npde.plot.select<-function(npdeObject,data=FALSE,ecdf=FALSE,qqplot=FALSE, histogram=FALSE,x.scatter=FALSE,pred.scatter=FALSE,x.box=FALSE,pred.box=FALSE, cov.x.scatter=FALSE,cov.pred.scatter=FALSE,cov.x.box=FALSE,cov.pred.box=FALSE, cov.ecdf=FALSE, vpc=FALSE,...) {
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
  if(cov.x.scatter) plot(npdeObject,plot.type="cov.x.scatter",...)
  if(cov.pred.scatter) plot(npdeObject,plot.type="cov.pred.scatter",...)
  if(cov.ecdf) plot(npdeObject,plot.type="cov.ecdf",...)
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
#' @keywords plot internal

#### Meta-niveau
default.npde.plots<-function(npdeObject,...) {
  # When plot(npdeObject) is called without plot.type
  par(mfrow=c(2,2),ask=npdeObject["prefs"]$ask)
  npde.plot.select(npdeObject,qqplot=TRUE,histogram=TRUE, x.scatter=TRUE,pred.scatter=TRUE,new=FALSE,...)
}

#' Covariate plots for a NpdeObject object
#'
#' Covariate plots for a NpdeObject object
#'
#' @usage npde.plot.covariates(npdeObject, which="x", ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param which one of "x" (scatterplots of the metric versus X), "pred" (scatterplots of the metric versus predictions) or "ecdf" (empirical distribution function)
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot
#' @export

npde.plot.covariates<-function(npdeObject,which="x",...) {
  # Parameters or random effects versus covariates
  if(which=="x") {
    plot(npdeObject,plot.type="cov.x.scatter",...)
  }
  if(which=="pred") {
    plot(npdeObject,plot.type="cov.pred.scatter",...)
  }
  if(which=="ecdf") {
    plot(npdeObject,plot.type="cov.ecdf",...)
  }
}

#' Plots for pd and npde
#'
#' Plots for pd and npde
#'
#' @aliases npde.plot.pd npde.plot.npde
#' @usage npde.plot.pd(npdeObject, ...)
#' @usage npde.plot.npde(npdeObject, ...)
#' @param npdeObject an object returned by a call to \code{\link{npde}} or \code{\link{autonpde}}
#' @param \dots additional arguments to be passed on to the function, to control which metric (npde, pd, npd) is used or to override graphical parameters (see the PDF document for details, as well as \code{\link{set.plotoptions}})
#' @keywords plot
#' @export

npde.plot.npde<-function(npdeObject,...) {
  # Advanced goodness of fit plots
  if(npdeObject@options$verbose) cat("Plots for npde\n")
  default.npde.plots(npdeObject,...)
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


  # data censored / no censored
  if(length(npdeObject["data"]["icens"])>0) {
    has.cens<-TRUE
    icens<-npdeObject["data"]["icens"]
    is.cens<-npdeObject["data"]["data"]$cens==1
  } else { has.cens<-FALSE}

  # data plot x,y and id
  x<-npdeObject["data"]["data"][,npdeObject["data"]["name.predictor"]]
  y<-npdeObject["data"]["data"][,npdeObject["data"]["name.response"]]
  plot.opt<-npdeObject["prefs"]
  id<-(npdeObject["data"]["data"]$index %in% plot.opt$ilist)

  #if(plot.opt$impute.loq & length(npdeObject["results"]["res"]$ycomp)>0 & npdeObject["options"]["cens.method"]!="omit"){
  #  y<-npdeObject["results"]["res"]$ycomp
  #}

  # plot options and user options
  userPlotOptions = list(...)
  plot.opt<-set.plotoptions.default(npdeObject)
  plot.opt <- modifyList(plot.opt, userPlotOptions[intersect(names(userPlotOptions), names(plot.opt))])

  # meta arguments to change col,lwd,lty,pch for lines and symbols
  if ( plot.opt$size %in% userPlotOptions)
  {
    plot.opt$size.pobs = plot.opt$size
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

  # changes for xlab and ylab
  # start for which.x and which.y by default in NpdeControl
  which.x = plot.opt$axis
  which.y = plot.opt$which

  # xlab
  if(plot.opt$xlab=="") {
    plot.opt$xlab <- switch(which.x, "x"=paste0( npdeObject@data@name.predictor ), "pred"=paste0("Predicted ", npdeObject@data@name.response ), "cov"="", "npde"="npde", "npd"="npd", "pd"="pd") # cov, npde, npd, pd: not valid options; cov: to be implemented
    if (which.x=="x" & npdeObject@data@units$x != "") plot.opt$xlab<-paste0(plot.opt$xlab, "(", npdeObject@data@units$x,")" )
    if (which.x=="pred" & npdeObject@data@units$y != "") plot.opt$xlab<-paste0(plot.opt$xlab, "(", npdeObject@data@units$y,")" )
  }

  # ylab
  if(plot.opt$ylab=="") {
    plot.opt$ylab <- switch(which.y, "npde"="npde", "npd"="npd", "pd"="pd", "yobs"=paste0( npdeObject@data@name.response),  "cov"="") # cov not a valid option (yet ?)
    if (which.y=="yobs" & npdeObject@data@units$y != "") plot.opt$ylab<-paste0(plot.opt$ylab, "(", npdeObject@data@units$y,")" )
  }

  # -----------------------------------------------------------------------------------------------------------------

  # ggplot with censored data
  if(has.cens) {

    xplot = x[id & !is.cens]
    yplot = y[id & !is.cens]

    # data no censored
    grouplot = npdeObject@data@data$index[id & !is.cens]
    dataplot = data.frame(grouplot,xplot,yplot)
    colnames(dataplot) = c("group","x","y")

    # data under the loq
    dataplot_bis = dataplot[dataplot$y<npdeObject@data@loq,]
    colnames(dataplot_bis) = c("group","x","y")

    # data censored
    group_loq_plot = npdeObject@data@data$index[id & is.cens]
    dataloq_plot = data.frame(group_loq_plot, x[id & is.cens],y[id & is.cens])
    colnames(dataloq_plot) = c("group","x","y")

    # data for plot
    dataplot = rbind(dataplot,dataplot_bis,dataloq_plot)

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
      y.limits = c(min(dataloq_plot$y, dataplot$y,na.rm = TRUE),max(dataloq_plot$y, dataplot$y,na.rm = TRUE))}

    # ggplot template
    p = ggplot(dataplot, aes(x=x, y=y)) +

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
                 mapping=aes(x=x,y=y),
                 color = plot.opt$col.pobs,
                 alpha = plot.opt$alpha.pobs,
                 size = plot.opt$size.pobs,
                 shape = plot.opt$pch.pobs)  +

      geom_line(aes(group=group),
                linetype = plot.opt$lty.lobs,
                color = plot.opt$col.lobs,
                size = plot.opt$lwd.lobs )+

      {if(plot.opt$line.loq==TRUE)

        geom_hline(dataplot,mapping = aes(yintercept = as.numeric(loq)))

      } +

      {if(plot.opt$plot.loq==FALSE)

        geom_point(dataplot_bis,
                   mapping=aes(x=x,y=y),
                   color = plot.opt$col.pcens,
                   shape = plot.opt$pch.pcens,
                   size = plot.opt$size.pcens,
                   alpha = plot.opt$alpha.pcens)} +

      {if(plot.opt$plot.loq==TRUE)

        geom_point(dataloq_plot,
                   mapping = aes(x=x,y=y),
                   color = plot.opt$col.pcens,
                   shape = plot.opt$pch.pcens,
                   size = plot.opt$size.pcens,
                   alpha = plot.opt$alpha.pcens)} +

      scale_x_continuous(plot.opt$xlab, scales::pretty_breaks(n = plot.opt$breaks.x)) +
      scale_y_continuous(plot.opt$ylab, scales::pretty_breaks(n = plot.opt$breaks.y))

    print(p)

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

    p = ggplot(dataplot, aes(x=x, y=y)) +

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

    print(p)

  }

  return( p )

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
#' @export
#' @keywords plot
## #' @keywords plot by default : qqplot, hist, x.scatter, pred.scatter

npde.plot.default<-function(npdeObject,  ...) {

  # remove the covariables for the waffle plot by defaults

  if (length(npdeObject@data@name.covariates)!=0){

    new_npdeObject = npdeObject
    drops = new_npdeObject@data@name.covariates
    new_npdeObject@data@data <- new_npdeObject@data@data[ , !(names(new_npdeObject@data@data) %in% drops)]

  }else{

    new_npdeObject = npdeObject

  }

  # modify the plot options with the user ones
  userPlotOptions = list(...)
  plot.opt <- set.plotoptions.default(new_npdeObject)
  plot.opt <- modifyList(plot.opt, userPlotOptions[intersect(names(userPlotOptions), names(plot.opt))])

  # waffle plot with covariate
  if ( plot.opt$covsplit ==TRUE)
  {
    covariates = plot.opt$which.cov
    i=1
    for (covariate in covariates)
    {

      hist.cov <-   npde.plot.dist(npdeObject,
                                   plot.opt$which,
                                   dist.type="hist",
                                   which.cov = covariate,
                                   plot.default=TRUE,...)

      qqplot.cov <-   npde.plot.dist(npdeObject,
                                     plot.opt$which,
                                     dist.type="qqplot",
                                     which.cov = covariate,
                                     plot.default=TRUE,...)

      x.scatter.cov <-   npde.plot.scatterplot(npdeObject,
                                               which.x="x", which.y=plot.opt$which,
                                               dist.type="x.scatter",
                                               which.cov = covariate,
                                               plot.default=TRUE,...)

      pred.scatter.cov <-   npde.plot.scatterplot(npdeObject,
                                                  which.x="pred", which.y=plot.opt$which,
                                                  dist.type="pred.scatter",
                                                  which.cov = covariate,
                                                  plot.default=TRUE,...)


      list_plot = c( hist.cov, qqplot.cov, x.scatter.cov, pred.scatter.cov )

      if (!is.null(list_plot))
      {
        grid.arrange(grobs = list_plot,
                     nrow=2, ncol=2,
                     top = textGrob(paste0(plot.opt$main[i],'\n'),
                                    vjust = 1,
                                    gp = gpar(fontsize=plot.opt$size.main)))
      }
      i=i+1
    } # end loop covariates
  }
  # waffle plot with covariate
  else
  {

    hist <- npde.plot.dist(new_npdeObject,
                           plot.opt$which,
                           dist.type="hist",
                           plot.default=TRUE,...)

    qqplot <- npde.plot.dist(new_npdeObject,
                             plot.opt$which,
                             dist.type="qqplot",
                             plot.default=TRUE,...)

    x.scatter <- npde.plot.scatterplot(new_npdeObject, which.x="x", which.y=plot.opt$which, plot.default=TRUE,...)

    pred.scatter <- npde.plot.scatterplot(new_npdeObject, which.x="pred", which.y=plot.opt$which, plot.default=TRUE,...)

    list_plot = c( hist, qqplot, x.scatter, pred.scatter )

    if (!is.null(list_plot))
    {
      grid.arrange(grobs = list_plot,
                   nrow=2, ncol=2,
                   top = textGrob(paste0(plot.opt$main,'\n'),
                                  vjust = 1,
                                  gp = gpar(fontsize=plot.opt$size.main)))
    }else{
      print("Error in arguments.The list of plots is therefore empty.")
    }

  }

} # end function npde.plot.default





