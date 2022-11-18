####################################################################################################
# Histogram
####################################################################################################

aux.npdeplot.hist<-function(obsmat,  plot.opt, distrib="norm", nclass=10, sim.ypl=NULL) {
  # input
  ## obsmat: matrix with the data to plot, with columns
  ### x; variable to plot
  ### category: covariate category (if "all", overall plot)
  ## plot.opt: list of graphical options
  ## distrib: reference distribution plot (one of norm, unif)
  ## nclass: number of classes for the histogram
  ## sim.ypl: if given, a vector of simulated data for the variable to plot
  nrep<-100
  nameCovariate = plot.opt$which.cov    # nom de la covariable
  namesCategories = sort(unique(obsmat$category ))   # catégories pour la covariable
  if(is.null(namesCategories)) namesCategories<-c("all")
  numberCategories =  length(namesCategories)  # nombre de catégories pour la covariable

  if(plot.opt$approx.pi && is.null(sim.ypl)) plot.opt$approx.pi<-TRUE else nrep<-length(sim.ypl)/dim(obsmat)[1]
  # -----------------------------------------------------------------------------------
  # Observed histogram
  xhist<-hist(obsmat$x,breaks=nclass,plot=FALSE) # breaks overall
  nB<-length(xhist$breaks)
  xwidth<-unique(diff(xhist$breaks))
  obshist<-NULL
  for(icat in namesCategories) {
    obsmat.cov<-obsmat[obsmat$category==icat,]
    x1<-hist(obsmat.cov$x, breaks=xhist$breaks, plot=FALSE)
    obshist.cov<-data.frame(name=xhist$mids, value=x1$counts)
    zecat<-as.character(icat)
    obshist.cov<-data.frame(obshist.cov, category=zecat, stringsAsFactors = FALSE)
    obshist<-rbind(obshist,obshist.cov)
  }
  obsmat$category<-factor(obsmat$category, levels=namesCategories, ordered=TRUE)
  
  # -----------------------------------------------------------------------------------
  # PI for histogram
  pimat<-NULL
  if(plot.opt$bands) {
    alpha<-plot.opt$pi.size
    if(alpha>0.5) alpha<-1-alpha
    for(icat in namesCategories) {
      obsmat.cov<-obsmat[obsmat$category==icat,]
      ndat<-dim(obsmat.cov)[1]
      if(plot.opt$approx.pi) {
        sim.ypl<-switch(distrib,norm=rnorm(ndat*nrep),unif=runif(ndat*nrep)) # PI computed with 100 replicate
      }
      sim.ypl<-matrix(sim.ypl,nrow=ndat)
      tmat<-matrix(nrow=length(xhist$breaks)-1,ncol=nrep)
      for(j in 1:nrep) {
        xvec<-cut(sim.ypl[,j],breaks=xhist$breaks,include.lowest=TRUE, ordered_result=TRUE)
        tmat[,j]<-table(xvec)
      }
      row.names(tmat)<-names(table(xvec))
      bnds<-apply(tmat,1,quantile,c(alpha/2,0.5,1-alpha/2))
      bnds<-t(bnds)
      pimat.cov<-data.frame(x=xhist$mids, lower=bnds[,1], median=bnds[,2], upper=bnds[,3]) # using geom_crossbar
      zecat<-as.character(icat)
      pimat.cov<-data.frame(pimat.cov, category=zecat, stringsAsFactors = FALSE)
      pimat<-rbind(pimat, pimat.cov)
      # x1<-rep(xhist$breaks,each=2) # using geom_ribbon
      # x1<-x1[-c(1,length(x1))]
      # pimat.cov<-data.frame(x=x1, lower=rep(bnds[,1],each=2), median=rep(bnds[,2],each=2), upper=rep(bnds[,3],each=2))
      # zecat<-as.character(icat)
      # pimat.cov<-data.frame(pimat.cov, category=zecat, stringsAsFactors = FALSE)
#      pimat<-rbind(pimat, pimat.cov)
    }
    pimat$category<-factor(pimat$category, levels=namesCategories, ordered=TRUE)
  }

  # -----------------------------------------------------------------------------------
  # Plot, facetting by covariate category

  if(is.null(plot.opt$xlim)) plot.opt$xlim<-c(min(xhist$breaks,na.rm=TRUE), max(xhist$breaks,na.rm=TRUE))
  if(is.null(plot.opt$ylim)) plot.opt$ylim<-c(0, max(c(obshist$value,pimat$upper),na.rm=TRUE))

  p <- ggplot(obsmat, aes(group=category)) +
    theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
          axis.title.x = element_text(size = plot.opt$size.xlab),
          axis.title.y = element_text(size = plot.opt$size.ylab),
          axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),
          axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
          panel.background=element_rect("white"),
          panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
          panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+

    # coordinates x-y
    coord_cartesian(xlim=plot.opt$xlim, ylim=plot.opt$ylim) +

    # Plot bands
    {  if(plot.opt$bands==TRUE)
      geom_crossbar(data=pimat, aes(x=.data$x, y=median, ymin=lower, ymax=upper),
                    width=diff(xhist$mids)[1],
                    colour = plot.opt$col.ther,
                    fill = plot.opt$fill.bands,
                    alpha = plot.opt$alpha.bands,
                    linetype =  plot.opt$lty.ther,
                    size = plot.opt$lwd.ther )} +

    # Plot observed histograms
    geom_bar(data=obshist, aes(x=name, y=value),
             stat="identity",
             width = diff(xhist$mids)[1],
             colour = plot.opt$col.lobs,
             fill = plot.opt$fill,
             alpha = plot.opt$alpha,
             linetype =  plot.opt$lty,
             size = plot.opt$lwd ) +
    # x-y logscales
    { if (plot.opt$xlog == FALSE)
        scale_x_continuous(plot.opt$xlab,
                           scales::pretty_breaks(n = plot.opt$breaks.x))
    } +
    {if (plot.opt$ylog == FALSE)
        scale_y_continuous(plot.opt$ylab,
                           scales::pretty_breaks(n = plot.opt$breaks.y))
    } +
    { if (plot.opt$xlog == TRUE)
        scale_x_log10(plot.opt$xlab,
                      breaks = scales::trans_breaks("log10", function(x) 10 ^ x),labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
    } +
    {if (plot.opt$ylog == TRUE)
        scale_y_log10(plot.opt$ylab,
                      breaks = scales::trans_breaks("log10", function(x) 10 ^ x),labels = scales::trans_format("log10", scales::math_format(10 ^ .x)))
    } +
    #if log scales plot logticks
    { if (plot.opt$xlog == TRUE) annotation_logticks(sides = "b")} +
    { if (plot.opt$ylog == TRUE) annotation_logticks(sides = "l")} +

    # facet wrap over covariate categories
    facet_wrap(.~factor(category, levels=namesCategories, ordered=TRUE), nrow=1) +
    {if(numberCategories==1)
      theme(strip.background = element_blank(), strip.text.x = element_blank())
      } +
    {if (plot.opt$main!="") ggtitle(plot.opt$main)}

  return(p)
} # End function histogram

####################################################################################################
# Empirical cdf  / QQ-plot

aux.npdeplot.dist<-function(obsmat,  plot.opt, dist.type="qqplot", distrib="norm", sim.ypl=NULL) {
  # input
  ## obsmat: matrix with the data to plot, with columns
  ### x; variable to plot
  ### category: covariate category (if "all", overall plot)
  ### cens: 1 if censored, 0 otherwise
  ## plot.opt: list of graphical options
  ## distrib: reference distribution plot (one of norm, unif)
  ## sim.ypl: if given, a vector of simulated data for the variable to plot
  nrep<-500
  nameCovariate = plot.opt$which.cov    # nom de la covariable
  namesCategories = sort(unique(obsmat$category ))   # catégories pour la covariable
  if(is.null(namesCategories)) namesCategories<-c("all")
  numberCategories =  length(namesCategories)  # nombre de catégories pour la covariable

  if(plot.opt$approx.pi && is.null(sim.ypl)) plot.opt$approx.pi<-TRUE else nrep<-length(sim.ypl)/dim(obsmat)[1]

  # -----------------------------------------------------------------------------------
  # Observed ecdf
  xmat<-NULL
  for(icat in namesCategories) {
    obsmat.cov<-obsmat[obsmat$category==icat,]
    ndat<-dim(obsmat.cov)[1]
    xmat.cov<-data.frame(x=sort(obsmat.cov$x),y=seq(1/ndat,1-1/ndat,length.out=ndat), cens=obsmat.cov$cens[order(obsmat.cov$x)])
    zecat<-as.character(icat)
    xmat.cov<-data.frame(xmat.cov, category=zecat, stringsAsFactors = FALSE)
    xmat<-rbind(xmat,xmat.cov)
  }
  xmat$category<-factor(xmat$category, levels=namesCategories, ordered=TRUE)
  if(dist.type=="qqplot" & distrib=="norm") xmat$y<-qnorm(xmat$y)

  # x-y axis labels for ecdf plot
  if(dist.type=="ecdf")  { # already done in function calling this one
    if(plot.opt$xlab=="") plot.opt$ylab <- plot.opt$which
    if(plot.opt$ylab=="") plot.opt$xlab <- "Empirical cumulative density function"
  }

  # -----------------------------------------------------------------------------------
  # PI for ecdf
  pimat<-NULL
  if(plot.opt$bands) {
    alpha<-plot.opt$pi.size
    if(alpha>0.5) alpha<-1-alpha
    for(icat in namesCategories) {
      obsmat.cov<-obsmat[obsmat$category==icat,]
      ndat<-dim(obsmat.cov)[1]
      if(plot.opt$approx.pi) {
        sim.ypl<-switch(distrib,norm=rnorm(ndat*nrep),unif=runif(ndat*nrep)) # PI computed with nrep replicate
      }
      sim.ypl<-matrix(sim.ypl,nrow=ndat)
      sim.ypl<-colsort(sim.ypl)
      bnds<-apply(sim.ypl,1,quantile,c(alpha/2,0.5,1-alpha/2))
      pimat.cov<-data.frame(x=seq(1/ndat,1-1/ndat,length.out=ndat),t(bnds))
      colnames(pimat.cov)[2:4]<-c("lower","median","upper")
      zecat<-as.character(icat)
      pimat.cov<-data.frame(pimat.cov, category=zecat, stringsAsFactors = FALSE)
      pimat<-rbind(pimat, pimat.cov)

    }
    pimat$category<-factor(pimat$category, levels=namesCategories, ordered=TRUE)
  }
  if(dist.type=="qqplot" & distrib=="norm") pimat$x<-qnorm(pimat$x)

  # -----------------------------------------------------------------------------------
  # Plot, facetting by covariate category

  if(is.null(plot.opt$xlim) || length(plot.opt$xlim)!=2) plot.opt$ylim<-c(min(pimat$lower,na.rm=TRUE), max(pimat$upper,na.rm=TRUE))
  if(is.null(plot.opt$ylim) || length(plot.opt$ylim)!=2) {
    if(dist.type=="qqplot") plot.opt$xlim<-c(min(xmat$y,na.rm=TRUE), max(xmat$y,na.rm=TRUE)) else plot.opt$xlim<-c(0, 1)
  }

  xmat.nocens<-xmat[xmat$cens==1,]
  if(sum(xmat$cens)>0) hasCens<-1 else hasCens<-0
  if(hasCens) xmat.cens<-xmat[xmat$cens==0,]

  p<-ggplot(xmat, aes(x=.data$y, y=.data$x, group=factor(category))) +
    # title and layout
    theme(plot.title = element_text(hjust = 0.5, size = plot.opt$size.sub),
          axis.title.x = element_text(size = plot.opt$size.xlab),
          axis.title.y = element_text(size = plot.opt$size.ylab),
          axis.text.x = element_text(size=plot.opt$size.text.x, color = ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.text.y = element_text(size=plot.opt$size.text.y, color = ifelse(plot.opt$yaxt==TRUE,"black","white")),
          axis.line.x = element_line(color=ifelse(plot.opt$xaxt==TRUE,"black","white")),
          axis.line.y = element_line(color=ifelse(plot.opt$yaxt==TRUE,"black","white")),
          panel.background=element_rect("white"),
          panel.grid.major.x = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid),
          panel.grid.major.y = element_line(ifelse(plot.opt$grid==TRUE,"grey80","white"),linetype = plot.opt$lty.grid))+
    # PI
    {if(plot.opt$bands==TRUE) geom_ribbon(data=pimat, aes(x=.data$x, ymin=lower, ymax=upper), linetype = plot.opt$lty.bands,
                colour = plot.opt$col.bands,
                fill = plot.opt$fill.bands,
                size = plot.opt$lwd.bands,
                alpha=plot.opt$alpha.bands) } +

    {if(plot.opt$bands==TRUE) geom_line(data=pimat, aes(x=.data$x, y=median),
                                        linetype = plot.opt$lty.ther,
                                        colour = plot.opt$col.ther,
                                        size = plot.opt$lwd.ther,
                                        alpha=plot.opt$alpha.ther) } +

    # Plotting observed ecdf/qqplot
    {if (plot.opt$type=="l" || plot.opt$type=="b")
      geom_line(data=xmat, aes(x=.data$y, y=.data$x),
                colour = plot.opt$col.lobs,
                linetype = plot.opt$lty.lobs,
                size = plot.opt$lwd.lobs) } +

    {if (plot.opt$type=="p" || plot.opt$type=="b")
      geom_point(xmat.nocens, mapping = aes(x=.data$y, y=.data$x),
                 colour = plot.opt$col.pobs,
                 shape = plot.opt$pch.pobs,
                 alpha = plot.opt$alpha.pobs,
                 size = plot.opt$size.pobs) } +

    { if (hasCens & plot.opt$type %in% c("p","b"))
      geom_point(xmat.cens, mapping = aes(x=.data$y, y=.data$x),
                 color = plot.opt$col.pcens,
                 shape = plot.opt$pch.pcens,
                 size = plot.opt$size.pcens,
                 alpha = plot.opt$alpha.pcens) } +
    
    # Flipping coordinates, setting scales x-y
    scale_x_continuous(plot.opt$xlab,
                       scales::pretty_breaks(n = plot.opt$breaks.x)) +

    scale_y_continuous(plot.opt$ylab,
                       scales::pretty_breaks(n = plot.opt$breaks.y)) +

    coord_flip(xlim=plot.opt$xlim, ylim=plot.opt$ylim) +

    # facet wrap over covariate categories
    facet_wrap(.~factor(category, levels=namesCategories, ordered=TRUE), nrow=1) +
    {if(numberCategories==1)
      theme(strip.background = element_blank(), strip.text.x = element_blank())
    } +
    {if (plot.opt$main!="") ggtitle(plot.opt$main)}

  return(p)

} # End function ecdf
